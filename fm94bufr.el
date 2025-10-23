;;; fm94bufr.el --- BUFR Decoder/Encoder  -*- lexical-binding: t -*-
;;
;;; Commentary:
;; 
;; Decode/encode WMO FM94 BUFR message.
;;
;; Environment variable $BUFR_TABLES must point to a
;; directory where table files (eccodes-style) are located.
;;
;; (C) 2025 alexmaul
;;
;;; Licence:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; Code:
;;
;; table store
;;
(defvar bufr-table-base (substitute-in-file-name "$BUFR_TABLES"))
(defvar bufr-tab_a '())
(defvar bufr-tab_b (make-hash-table :test 'equal))
(defvar bufr-tab_d (make-hash-table :test 'equal))
(defvar bufr-tab_cf (make-hash-table :test 'equal))
(defvar bufr-loadedtables '())
;;
;; global vars for decoding section data
;;
(defvar bufr-start -1)
(defvar bufr-end -1)
(defvar bufr-cur_bit 0)
(defvar bufr-meta (make-hash-table :test 'equal))
(defvar bufr-modifier (make-hash-table :test 'equal)) ; "change"->(width factor offset)
(defvar bufr-obuf nil) ; emacs buffer with original or new BUFR
(defvar bufr-dbuf nil) ; emacs buffer for decoded output
(defvar bufr-olist '()) ; character codepoint list for encoding


;;
;; Reading tables
;;
(defun bufr-read-table-files (which mvers &optional cntr scntr lvers)
  "Read/parse the table files and store them in global variables.

WHICH ones to load: `m´ = master tables or `l´ = local tables,
MVERS: master version, when loading local tables in can be nil.
       Can be a number or the string `latest´, where the tables
       with the highest version are loaded.
       (Be carefull, the bit-width might change between versions!)
Local tables are overlayed in global table variables:
LVERS: local version,
CNTR: centre,
SCNTR: sub-centre."
  (let (tabstr (tabfnlst '()) filePath)
    (cond
     ((equal which "m")
      ;; Prepare filenames for master tables
      (when (equal mvers "latest")
	;; search for table file with highest version
	(let ((vl '()))
	  (dolist (fn (directory-files (format "%s/0/wmo/" bufr-table-base) nil "^[0-9]+"))
     	    (push (string-to-number fn) vl))
	  (setf mvers (apply #'max vl))))
      (setq tabstr (format "%02d" mvers))
      ;; read global tables if not already loaded
      (when (eq (member tabstr bufr-loadedtables) nil)
	(push (format "%s/0/wmo/%d/codetables/" bufr-table-base mvers) tabfnlst)
	(push (format "%s/0/wmo/%d/sequence.def" bufr-table-base mvers) tabfnlst)
	(push (format "%s/0/wmo/%d/element.table" bufr-table-base mvers) tabfnlst)
	(push (format "%s/datacat.table" bufr-table-base) tabfnlst)
	))
     ((equal which "l")
      ;; Prepare filenames for local tables
      (setq tabstr (format "%d-%d-%d" cntr scntr lvers))
      (when (and (eq (member tabstr bufr-loadedtables) nil)
		 (/= 0 lvers))
	;; Load local tables only if neccessary
	(push (format "%s/0/local/%d/%d/%d/codetables/" bufr-table-base lvers cntr scntr) tabfnlst)
	(push (format "%s/0/local/%d/%d/%d/sequence.def" bufr-table-base lvers cntr scntr) tabfnlst)
	(push (format "%s/0/local/%d/%d/%d/element.table" bufr-table-base lvers cntr scntr) tabfnlst)
	(push (format "%s/datacat.table" bufr-table-base) tabfnlst)
	))
     )
    (when (> (length tabfnlst) 0)
      (message "Loading BUFR table files ...")
      ;; Table A
      ;; Columns: code|name
      (setq filePath (pop tabfnlst))
      (when (file-readable-p filePath)
	(with-temp-buffer
	  (insert-file-contents filePath)
	  (let (row)
	    (dolist (line (split-string (buffer-string) "\n" t) row)
	      (unless (equal (substring line 0 1) "#")
		(setq row (split-string (string-trim line) "|" t))
		(push (cons (string-to-number (car row)) (cdr row)) bufr-tab_a)
		))
	    )))
      ;; Table B
      ;; Columns: code|abbreviation|type|name|unit|scale|reference|width|crex_unit|crex_scale|crex_width
      (setq filePath (pop tabfnlst))
      (with-temp-buffer
	(insert-file-contents filePath)
	(let (row fxy)
	  (dolist (line (split-string (buffer-string) "\n" t) row)
	    (unless (equal (substring line 0 1) "#")
	      (setq row (split-string line "|" t))
	      (setq fxy (car row))
	      (puthash fxy (cdr row) bufr-tab_b)
	      ))
	  ))
      ;; Table D
      ;; Key=Value: "fxxyyy"=[fxxyyy, fxxyyy, ...]  ; line break possible in [...]-list
      (setq filePath (pop tabfnlst))
      (with-temp-buffer
	(insert-file-contents filePath)
	(let (fxy pa pb foo bar (seq '()))
	  (setf pa (point))
	  (while (not (equal pa nil))
	    (setf pb (re-search-forward "\"[0-9]+\" = \\[[0-9, \n]+?\\]\n" nil t))
	    (unless (equal pb nil)
	      (setq foo (buffer-substring pa pb))
	      (setq foo (replace-regexp-in-string "[\"[\n ]" "" foo))
	      (setq foo (string-replace "]" "" foo))
	      (setq bar (split-string foo "="))
	      (setq fxy (car bar))
	      (setq seq (split-string (nth 1 bar) ","))
	      (puthash fxy (copy-sequence seq) bufr-tab_d))
	    (setf pa pb)
	    )))
      ;; Table CF - codes/flags
      ;; Columns: idx idx name
      ;; For code table idx is numeric, for flag table it's the nth bit to set.
      ;; Flag: MSB, 1 is left most signficant bit, increasing to right least significant bit.
      (setq filePath (pop tabfnlst))
      (with-temp-buffer
	(dolist (fn (directory-files filePath nil "^[0-9]+\.table"))
	  (insert-file-contents (format "%s/%s" filePath fn))
	  (let (row fxy (cflist '()) bentry)
	    (setq fxy (format "%06d" (string-to-number fn)))
	    (setq bentry (gethash fxy bufr-tab_b))
	    (dolist (line (split-string (buffer-string) "\n" t) row)
	      (unless (equal (substring line 0 1) "#")
		(setq row (cdr (split-string line " ")))
		(if (equal (nth 1 bentry) "flag")
		    ;; Flags
		    (push (cons
			   (ash 1 (- (string-to-number (nth 6 bentry)) (string-to-number (car row))))
			   (mapconcat 'identity (cdr row) " "))
			  cflist)
		  ;; Codes
		  (push (cons (string-to-number (car row)) (mapconcat 'identity (cdr row) " ")) cflist)
		  )))
	    (puthash fxy cflist bufr-tab_cf)
	    (setq cflist '())
	    (erase-buffer)
	    )))
      (push tabstr bufr-loadedtables)
      (message "Table files loaded.")
      ))
  nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Decoding
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bufr-from-flat-bits (width)
  "Read WIDTH bits from current position."
  (let (pval              ; value from point
	(val 0)           ; buffer value for bit-collecting
	(ub width)        ; bits remaining
	(lb bufr-cur_bit) ; bits processed in current octet
	nb                ; bits to process from current octet
	rb)               ; rigth-most bit in current octet
    (while (> ub 0)
      (setf pval (logand 255 (char-after)))
      (if (< (+ lb ub) 8)
	  (progn
	    (setf nb ub)
	    (setf rb (+ lb ub))
	    (setf lb (+ lb nb)))
	(progn
	  (setf nb (- 8 lb))
	  (setf rb 8)
	  (setf lb 0)
	  (forward-char)))
      (setf ub (- ub nb))
      (setf val (ash val nb))
      (setf val (logior val (logand (ash pval (- (- 8 rb))) (- (ash 1 nb) 1)))))
    (setf bufr-cur_bit lb)
    val
    ))


(defun bufr-from-bytes (octets &optional as)
  "Read an ammount of OCTETS (bytes).
AS=num: interpreted as an MSB integer,
AS=str: joined to a string."
  (let (r)
    (if (eq as 'str)
	(progn
	  (setq r (buffer-substring-no-properties (point) (+ (point) octets)))
	  (forward-char octets))
      (let (c)
	(setf r 0)
	(dotimes (i octets)
	  (setf r (ash r 8))
	  (setf c (logand (char-after) 255))
	  (setf r (logior r c))
	  (forward-char))))
    r
    ))


(defun bufr-from-bits (subsidx width &optional as)
  "Read WIDTH bits from current position.
AS=num (default) returns them as an integer number,
or AS=str a string (WIDTH should be a multiple of 8).
SUBSIDX states the currently decoded subset (0..n) when
compression is used, set to -1 if no cempression is used."
  (let ((val 0))
    (if (= subsidx -1)
	;; Read bits without compression
	(setq val (bufr-from-flat-bits width))
      ;; Read bits from compressed data section
      (let ((R0 0) (nbinc 0) (In 0))
	;; base reference value
	(setf R0 (bufr-from-flat-bits width))
	;; amount of bits for each increment value
	(setf nbinc (bufr-from-flat-bits 6))
	(when (eq as 'str)
	  (setf nbinc (* 8 nbinc)))
	(if (= 0 nbinc)
	    ;; All values of I are equal to R0, all In are omitted
	    (setf val R0)
	  (progn
	    ;; read nbic bits from previous subsets
	    (bufr-from-flat-bits (* nbinc subsidx))
	    ;; read nbic bits from current subset
	    (setf In (bufr-from-flat-bits nbinc))
	    ;; read remaining bits
	    (bufr-from-flat-bits (* nbinc (- (gethash "subs" bufr-meta) subsidx 1)))
	    (if (= In (- (ash 1 nbinc) 1))
		;; if In equals "missing"
		(setq val (- (ash 1 width) 1))
	      ;; add base reference to reduced value
	      (setf val (+ R0 In)))
	    ))
	))
    ;; convert (and transpose) the number to string, if requested
    (when (eq as 'str)
      (let ((chrlst '()) cval (i 0))
	(while (< i width)
	  ;; interprete each octet from integer as char, collect these
	  (setf cval (logand 255 (ash val (- i))))
	  (if (/= cval 0)
	      (push cval chrlst)
	    (push 32 chrlst))
	  (setf i (+ i 8)))
	;; concat all chars and decode from Latin1 to UTF8
	(setq val (decode-coding-string (concat chrlst) 'latin-1))
	))
    val))


(defun bufr-get-value (subsidx desc typ factor offset width)
  "Reading bits, and transforming interger values accordingly.
Uses `bufr-*-from-*´ functions. Code/flag table references are looked up."
  (let (rval (accu '()) (qual nil) val awidth afactor aoffset)
    (unless (or (equal (substring desc 0 3) "031")
		(= 0 (length (gethash "quality" bufr-modifier))))
      ;; Read and print associated data quality.
      ;; qual is list of bit-width, car of "quality" is the latest and significant.
      (setf qual (bufr-from-bits subsidx (car (gethash "quality" bufr-modifier)))))
    (cond
     ((or (equal typ "long") (equal typ "double"))
      ;; Read integer and float numbers, apply various modifiers
      (setf awidth (+ width (nth 0 (gethash "change" bufr-modifier))))
      (setf afactor (+ factor (nth 1 (gethash "change" bufr-modifier))))
      (setf aoffset (gethash desc (gethash "newreflst" bufr-modifier) offset))
      (setf aoffset (* aoffset (nth 2 (gethash "change" bufr-modifier))))
      (setf awidth (gethash "locwidth" bufr-modifier awidth))
      (setf val (bufr-from-bits subsidx awidth))
      (if (= val (- (ash 1 awidth) 1))
	  ;; If number equals "missing"
	  (setq rval nil)
	;; else calculate return number
	(setf rval (* (+ val aoffset) (expt 10 (- afactor)))))
      )
     ((equal typ "table")
      ;; Read number and look up in CF table, return "single entry"
      (setf width (gethash "locwidth" bufr-modifier width))
      (setf val (bufr-from-bits subsidx width))
      (setq rval (format "%s %s" val (list (cdr (assoc val (gethash desc bufr-tab_cf))))))
      )
     ((equal typ "flag")
      ;; Read number ab look up in CF table, join results in list
      (setf width (gethash "locwidth" bufr-modifier width))
      (setf val (bufr-from-bits subsidx width))
      (let ((mval val))
	(when (= val (- (ash 1 width) 1))
	  (setq mval (ash 1 (1- width))))
	(dolist (elt (gethash desc bufr-tab_cf) accu)
 	  (when (> (logand mval (car elt)) 0)
	    (push (cdr elt) accu))))
      (setq rval (format "%s %s" val accu))
      )
     ((equal typ "string")
      ;; Read string
      (setf awidth (gethash "newstrlen" bufr-modifier width))
      (setf awidth (gethash "locwidth" bufr-modifier awidth))
      (setq rval (bufr-from-bits subsidx awidth 'str))
      ;; return nil if all chars equal 0xFF
      (when (equal rval (make-string (length rval) ?\377))
	(setq rval nil))
      ))
    (unless (equal (gethash "locwidth" bufr-modifier) nil)
      ;; Remove modifier for single "local descriptor width"
      (remhash "locwidth" bufr-modifier))
    (list rval qual)
    ))


(defun bufr-get-padding (end)
  "Apply padding.
Moves point to END (end-of-section) if needed, depending on
BUFR edition and remaining octets in a section.
If END is nil just move the bit-pointer to the start of the next octet."
  (if (eq end nil)
      (progn
	(setf bufr-cur_bit 0)
	(forward-char))
    (let (dist (here (point)))
      (setf dist (- end here))
      (if (< dist 0)
	  (progn
	    (backward-char dist)
	    (bufr-debug "--------------------------- Error in sect-length: " dist)
	    )
	(forward-char dist))
      )))


(defun bufr-dec-sect-0to1 ()
  "Decoding metadata from sections 0 and 1."
  (let (edition val txt sectstart sectlen)
    ;;
    ;; Section 0
    ;;
    (setq txt (bufr-from-bytes 4 'str))
    (puthash "length" (bufr-from-bytes 3) bufr-meta)
    (setf bufr-end (1- (+ bufr-start (gethash "length" bufr-meta))))
    (setf edition (bufr-from-bytes 1))
    (puthash "edit" edition bufr-meta)
    (with-current-buffer bufr-dbuf
      (insert (format "%s     --  Pos: %d..%d Len: %d\n" txt bufr-start bufr-end (gethash "length" bufr-meta)))
      (insert (format "%-5s   Edition               : %s\n" "edit" (gethash "edit" bufr-meta))))
    ;;
    ;; Section 1
    ;;
    (setf sectstart (point))
    (setq sectlen (bufr-from-bytes 3))
    (bufr-from-bytes 1)
    (if (= edition 4)
	(progn
	  (puthash "centr" (bufr-from-bytes 2) bufr-meta)
	  (puthash "scntr" (bufr-from-bytes 2) bufr-meta)
	  (puthash "upseq" (bufr-from-bytes 1) bufr-meta)
	  (puthash "sect2" (logand (ash (bufr-from-bytes 1) -7) 1) bufr-meta)
	  (puthash "cat" (bufr-from-bytes 1) bufr-meta)
	  (puthash "scat" (bufr-from-bytes 1) bufr-meta)
	  (puthash "lscat" (bufr-from-bytes 1) bufr-meta)
	  (puthash "mvers" (bufr-from-bytes 1) bufr-meta)
	  (puthash "lvers" (bufr-from-bytes 1) bufr-meta)
	  (puthash "year" (bufr-from-bytes 2) bufr-meta)
	  (puthash "month" (bufr-from-bytes 1) bufr-meta)
	  (puthash "day" (bufr-from-bytes 1) bufr-meta)
	  (puthash "hour" (bufr-from-bytes 1) bufr-meta)
	  (puthash "min" (bufr-from-bytes 1) bufr-meta)
	  (puthash "sec" (bufr-from-bytes 1) bufr-meta)
	  )
      (progn
	(puthash "scntr" (bufr-from-bytes 1) bufr-meta)
	(puthash "centr" (bufr-from-bytes 1) bufr-meta)
	(puthash "upseq" (bufr-from-bytes 1) bufr-meta)
	(puthash "sect2" (logand (ash (bufr-from-bytes 1) -7) 1) bufr-meta)
	(puthash "cat" (bufr-from-bytes 1) bufr-meta)
	(puthash "lscat" (bufr-from-bytes 1) bufr-meta)
	(puthash "mvers" (bufr-from-bytes 1) bufr-meta)
	(puthash "lvers" (bufr-from-bytes 1) bufr-meta)
	(setf val (bufr-from-bytes 1))
	(if (< val 50)
	    (puthash "year" (+ 2000 val) bufr-meta)
	  (puthash "year" (+ 1900 val) bufr-meta))
	(puthash "month" (bufr-from-bytes 1) bufr-meta)
	(puthash "day" (bufr-from-bytes 1) bufr-meta)
	(puthash "hour" (bufr-from-bytes 1) bufr-meta)
	(puthash "min" (bufr-from-bytes 1) bufr-meta)
	(puthash "scat" 0 bufr-meta)
	(puthash "sec" 0 bufr-meta)
	))
    (setq txt (bufr-from-bytes (- (+ sectstart sectlen) (point)) 'str))
    (if (< (length txt) 3)
	(puthash "adpd" "" bufr-meta)
      (puthash "adpd" txt bufr-meta))
    (with-current-buffer bufr-dbuf
      (setf val (gethash "centr" bufr-meta))
      (setq txt (list (cdr (assoc val (gethash "001035" bufr-tab_cf)))))
      (insert (format "%-5s   Centre                : %s %s\n" "centr" val txt))
      (setf val (gethash "scntr" bufr-meta))
      (setq txt (list (cdr (assoc val (gethash "001034" bufr-tab_cf)))))
      (insert (format "%-5s   Sub-centre            : %s %s\n" "scntr" val txt))
      (insert (format "%-5s   Update sequence number: %s\n" "upseq" (gethash "upseq" bufr-meta)))
      (insert (format "%-5s   Section 2             : %s\n" "sect2" (gethash "sect2" bufr-meta)))
      (setf val (gethash "cat" bufr-meta))
      (setq txt (cdr (assoc val bufr-tab_a)))
      (insert (format "%-5s   Data category         : %s %s\n" "cat" val txt))
      (insert (format "%-5s   Sub-category          : %s\n" "scat" (gethash "scat" bufr-meta)))
      (insert (format "%-5s   Local sub-category    : %s\n" "lscat" (gethash "lscat" bufr-meta)))
      (insert (format "%-5s   Master table version  : %s\n" "mvers" (gethash "mvers" bufr-meta)))
      (insert (format "%-5s   Local table version   : %s\n" "lvers" (gethash "lvers" bufr-meta)))
      (insert (format "%-5s   Year                  : %s\n" "year" (gethash "year" bufr-meta)))
      (insert (format "%-5s   Month                 : %s\n" "month" (gethash "month" bufr-meta)))
      (insert (format "%-5s   Day                   : %s\n" "day" (gethash "day" bufr-meta)))
      (insert (format "%-5s   Hour                  : %s\n" "hour" (gethash "hour" bufr-meta)))
      (insert (format "%-5s   Minute                : %s\n" "min" (gethash "min" bufr-meta)))
      (insert (format "%-5s   Second                : %s\n" "sec" (gethash "sec" bufr-meta 0)))
      (insert (format "%-5s   Optional ADP data     : '%s'\n" "adpd" (gethash "adpd" bufr-meta)))
      )))


(defun bufr-dec-sect-2to5 ()
  "Decoding sections 2 (if present) and 3 and 4 and 5."
  ;;
  ;; Section 2, if present
  ;;
  (unless (= 0 (gethash "sect2" bufr-meta))
    (let (sectlen txt)
      (setf sectlen (bufr-from-bytes 3))
      (bufr-from-bytes 1)
      (setq txt (bufr-from-bytes (- sectlen 4) 'str))
      (with-current-buffer bufr-dbuf
	(insert
	 (format "s2dat   Sec.2 data (% 3d bytes): '%s'\n"
		 (- sectlen 4)
		 (mapconcat (lambda (c) (format "%02x" (logand 255 c))) txt " "))))
      ))
  ;;
  ;; Section 3 - template data
  ;;
  (let (sectstart sectlen txt val (desclst '()) f x y)
    (setf sectstart (point))
    (setf sectlen (bufr-from-bytes 3))
    (bufr-from-bytes 1)
    (puthash "subs" (bufr-from-bytes 2) bufr-meta)
    (setf val (bufr-from-bytes 1))
    (puthash "obs" (ash (logand val 128) -7) bufr-meta)
    (puthash "comp" (ash (logand val 64) -6) bufr-meta)
    (while (< (point) (- (+ sectstart sectlen) 1))
      ;; Collect list of unexpanded descriptors
      (setf val (bufr-from-bytes 2))
      (setf f (ash val -14))
      (setf x (logand (ash val -8) 63))
      (setf y (logand val 255))
      (setq txt (format "%d%02d%03d" f x y))
      (push txt desclst))
    (puthash "udesc" (reverse desclst) bufr-meta)
    (with-current-buffer bufr-dbuf
      (insert (format "%-5s   Subsets               : %s\n" "subs" (gethash "subs" bufr-meta)))
      (insert (format "%-5s   Observed data         : %s\n" "obs" (gethash "obs" bufr-meta)))
      (insert (format "%-5s   Compression           : %s\n" "comp" (gethash "comp" bufr-meta)))
      (insert (format "%-5s   Unexpanded descriptors: %s\n" "udesc" (gethash "udesc" bufr-meta))))
    (bufr-get-padding (+ sectstart sectlen))
    )
  ;;
  ;; Section 4 - subset data
  ;;
  (let (sectstart sectlen sectend comploopstart (stack '()))
    (setf sectstart (point))
    (setf sectlen (bufr-from-bytes 3))
    (setf sectend (+ (- (point) 3) sectlen))
    (bufr-from-bytes 1)
    (setf comploopstart (point))
    ;; For each subset/report do:
    (dotimes (subsidx (gethash "subs" bufr-meta))
      (with-current-buffer bufr-dbuf
	(insert (format "------  SUBSET %d\n" (1+ subsidx))))
      ;; Reset modifiers set by fxx=201|202|203|204|207|208
      (setq bufr-modifier (make-hash-table :test 'equal))
      ;; Reset modifiers for new subset:
      ;; "change"->(width factor offset)
      ;; width and factor are added, offset is multiplied with existing values
      (puthash "change" (list 0 0 1) bufr-modifier)
      (puthash "quality" '() bufr-modifier)
      (remhash "newstrlen" bufr-modifier)
      (puthash "newreflst" (make-hash-table :test 'equal) bufr-modifier)
      ;; Prepare a fresh stack of unexpanded descriptors
      (setq stack (copy-sequence (gethash "udesc" bufr-meta)))
      ;; Start decoding the data section according the descriptors in stack
      (if (= 0 (gethash "comp" bufr-meta))
	  ;; uncompressed data section
	  (bufr-dec-loop sectend -1 stack)
	;; compressed data section
	(progn
	  (bufr-dec-loop sectend subsidx stack)
	  (goto-char comploopstart)
	  (setf bufr-cur_bit 0))))
    (when (> bufr-cur_bit 0)
      ;; Padding bits to end section on a full octet
      (bufr-get-padding nil))
    (bufr-get-padding (+ sectstart sectlen))
    )
  ;;
  ;; Section 5 - decoding is for mere completeness and to test if everything's gone right.
  ;;
  (let (txt (r nil))
    (setq txt (bufr-from-bytes 4 'str))
    (unless (or (= (point) (1+ bufr-end)) (= (point) (point-max)))
      (setq txt
	    (format "!! Error reaching end, %d<>%d '%s' !!\n"
		    (point) (1+ bufr-end) txt))
      (setq r txt))
    (with-current-buffer bufr-dbuf
      (insert txt))
    r
    ))


(defun bufr-dec-loop (sectend subsidx stack)
  "Main function for iterating over the descriptors on a STACK.
Warn if SECTEND is reached prematurely.
For replication and sequences it call itself with new descriptor lists.
SUBSIDX states the currently decoded subset."
  (let (desc    ; descriptor (string)
	f x y   ; descpriptor parts as numbers
	val)    ; data value
    (while (and stack (< (point) sectend))
      (setq desc (pop stack))
      (setf f (string-to-number (substring desc 0 1)))
      (setf x (string-to-number (substring desc 1 3)))
      (setf y (string-to-number (substring desc 3 6)))
      (cond
       ((= f 0)
	;; Data descriptor
	(let (tabentry typ name unit fact offs wdth)
	  (setq tabentry (gethash desc bufr-tab_b))
	  (setf typ (nth 1 tabentry))
	  (setf name (nth 2 tabentry))
	  (setf unit (nth 3 tabentry))
	  (setf fact (string-to-number (nth 4 tabentry)))
	  (setf offs (string-to-number (nth 5 tabentry)))
	  (setf wdth (string-to-number (nth 6 tabentry)))
	  (setq val (bufr-get-value subsidx desc typ fact offs wdth))
	  (bufr-dec-print desc name (car val) unit (nth 1 val)))
	)
       ((= f 1)
	;; Replication
	(let (rep        ; amount of repliation
	      (loli '()) ; loop-stack with replicated descriptors
	      tabentry   ; table b entry (needed for class 31 descr.)
	      foo name wdth)
	  (bufr-dec-print desc "Replication" "start")
	  (if (= y 0)
	      ;; Delayed replication
	      (progn
		(setq foo (pop stack))
		(setq tabentry (gethash foo bufr-tab_b))
		(setf name (nth 2 tabentry))
		(setf wdth (string-to-number (nth 6 tabentry)))
		(setf rep (bufr-from-bits subsidx wdth))
		(bufr-dec-print foo name rep))
	    ;; Fix replication
	    (setf rep y))
	  (dotimes (i x)
	    ;; Collect replicated descriptors
	    (push (pop stack) loli))
	  (dotimes (i rep)
	    ;; For each replication do recursion
	    (bufr-dec-print desc "Replication" i)
	    (bufr-dec-loop sectend subsidx (reverse loli)))
	  (bufr-dec-print desc "Replication" "end"))
	)
       ((= f 2)
	;; Operator
	(setq val (bufr-dec-eval-oper x y stack subsidx))
	(bufr-dec-print desc (format "Operator, %s" (car val)) (cdr val))
	)
       ((= f 3)
	;; Sequence
	(let (tabentry)
	  (setq tabentry (gethash desc bufr-tab_d))
	  (bufr-dec-print desc "Sequence" "start")
	  ;; Recursion for "expanded descriptors"
	  (bufr-dec-loop sectend subsidx tabentry))
	(bufr-dec-print desc "Sequence" "end")
	)
       ))))


(defun bufr-dec-eval-oper (x y stack subsidx)
  "Handling the operator descriptors for decoding.
Parameters X and Y are the number parts of the descriptor and STACK the
current list of remaining descriptors to process, since some operators
need to read from that list.
SUBSIDX needs to be passed thorugh to bit-reading function for compressed data.

Implemented: fxx= 201 202 203 204 205 206 207 208"
  ;;
  ;; bufr-modifier:
  ;; "change"->(width factor offset)
  ;;
  (let (v a (r nil))
    (cond
     ((= x 1)
      ;; Add (YYY–128) bits to the data width given for each
      ;; data element in Table B, other than CCITT IA5
      ;; (character) data, code or flag tables.
      (setq a (gethash "change" bufr-modifier))
      (if (= y 0)
	  (setq v (list 0 (nth 1 a) (nth 2 a)))
	(setq v (list (- y 128) (nth 1 a) (nth 2 a))))
      (puthash "change" v bufr-modifier)
      (setq r (cons "change data width" v))
      )
     ((= x 2)
      ;; Add YYY–128 to the scale for each data element in
      ;; Table B, other than CCITT IA5 (character) data, code
      ;; or flag tables.
      (setq a (gethash "change" bufr-modifier))
      (if (= y 0)
	  (setq v (list (nth 0 a) 0 (nth 2 a)))
	(setq v (list (nth 0 a) (- y 128) (nth 2 a))))
      (puthash "change" v bufr-modifier)
      (setq r (cons "change scale" v))
      )
     ((= x 3)
      ;; Subsequent element descriptors define new reference
      ;; values for corresponding Table B entries. Each new
      ;; reference value is represented by YYY bits in the Data
      ;; section. Definition of new reference values is concluded
      ;; by coding this operator with YYY = 255. Negative
      ;; reference values shall be represented by a positive
      ;; integer with the left-most bit (bit 1) set to 1
      (setq a (gethash "newreflst" bufr-modifier))
      (setq v '())
      (let (b d yyy)
	(setq yyy (format "%03d" y))
	(while (not (equal yyy "255"))
	  (setq d (pop stack))
	  (setq yyy (substring d 3 6))
	  (if (= y 0)
	      (progn
		(setq b nil)
		(remhash d a)
		)
	    (progn
	      (setf b (bufr-from-bits subsidx y))
	      (when (/= 0 (logand b (ash 1 (1- y))))
		(setf b (- (logxor b (ash 1 (1- y))))))
	      (puthash d b a)))
	  (push (cons d b) v)))
      (setq r (cons "change reference" (reverse v)))
      )
     ((= x 4)
      ;; Precede each data element with YYY bits of
      ;; information. This operation associates a data field
      ;; (e.g. quality control information) of YYY bits with each
      ;; data element.
      ;; The data description operator 2 04 YYY, other than 2 04 000, shall
      ;; be followed immediately by the descriptor 0 31 021 to indicate the
      ;; meaning of the associated field.
					; bufr-modifier{"quality"} is list bits-width, acting as stack.
					; for each new value to push add bits from previous to the new.
      (setq a (gethash "quality" bufr-modifier))
      (if (= y 0)
	  ;; yyy=000 removes quality information last added.
	  (pop a)
	;; when yyy>0
	(if (= 0 (length a))
	    (push y a)             ; push first element
	  (push (+ y (car a)) a))  ; add previous qual to y, then push
	)
      (puthash "quality" a bufr-modifier)
      (setq r (cons "quality information, set width" (car a)))
      )
     ((= x 5)
      ;; YYY characters (CCITT International Alphabet No. 5) are
      ;; inserted as a data field of YYY x 8 bits in length.
      (let (txt)
	(setq txt (string-trim (concat (bufr-from-bits subsidx (* y 8) 'str))))
	(when (or (= (length txt) 0) (equal txt (make-string y ?\377)))
	  (setq txt nil))
	(setq r (cons "signify characters" txt)))
      )
     ((= x 6)
      ;; YYY bits of data are described by the immediately
      ;; following descriptor.
      (puthash "locwidth" y bufr-modifier)
      (setq r (cons "signify data width" y))
      )
     ((= x 7)
      ;; For Table B elements, which are not CCITT IA5
      ;; (character data), code tables, or flag tables:
      ;; 1. Add YYY to the existing scale factor
      ;; 2. Multiply the existing reference value by 10^YYY
      ;; 3. Calculate ((10*YYY)+2)/3, disregard any
      ;;    fractional remainder and add the result to the
      ;;    existing bit width.
      (setq a (gethash "change" bufr-modifier)) ; (width scale reference)
      (if (= y 0)
	  (setq v (list 0 0 1))
	(setq v (list (/ (+ (* 10 y) 2) 3)
		      (+ (nth 1 a) y)
		      (expt 10 y))))
      (puthash "change" v bufr-modifier)
      (setq r (cons "change width, scale, reference" v))
      )
     ((= x 8)
      ;; YYY characters from CCITT International Alphabet
      ;; No. 5 (representing YYY x 8 bits in length) replace the
      ;; specified data width given for each CCITT IA5
      ;; element in Table B.
      (if (= y 0)
	  (remhash "newstrlen" bufr-modifier)
	(puthash "newstrlen" (* y 8) bufr-modifier))
      (setq r (cons "replace string data width" (list y)))
      )
     (t
      (error (format "Operator '2%02d%03d' not implemented!" x y))
      ))
    r
    ))


(defun bufr-dec-print (desc name val &optional unit qual)
  "Print values in a pretty and column-like order to `output buffer´."
  (let (vtxt utxt (qtxt ""))
					; TODO: eval/format QUAL.
    (setq vtxt val)
    (if (or (equal unit nil)
	    (equal unit "Numeric")
	    (equal unit "CCITT IA5")
	    (equal unit "CODE TABLE")
	    (equal unit "FLAG TABLE"))
	(setq utxt "        ")
      (setq utxt (format "[%6s]" unit)))
    (unless (equal qual nil)
      (setq qtxt (format "(q=%d) " qual)))
    (with-current-buffer bufr-dbuf
      (insert (format "%s  %-50.50s %s: %s%-10s\n" desc name utxt qtxt vtxt)))
    ))


(defun bufr-debug (&rest txt)
  (with-current-buffer bufr-dbuf
    (insert (format "DEBUG: %s\n" (mapconcat (lambda (s) (format "%s" s)) txt ", ")))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Encoding
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bufr-enbug (&rest txt)
  (with-current-buffer bufr-obuf
    (insert (format "DEBUG: %s\n" (mapconcat (lambda (s) (format "%s" s)) txt ", ")))))


(defun bufr-to-bytes (octets value)
  "Convert integer VALUE to OCTETS bytes (like casting)."
  (let (c (lst '()))
    (unless (eq octets nil)
    (dotimes (i octets)
      (setf c (logand (ash value (- (* i 8))) 255))
      (push c lst)
      ))
    lst
    ))


(defun bufr-hex-to-num (value)
  "Convert a string VALUE with a series of hex-numbers to a list of bytes."
  (let ((res '()))
    (dolist (c (split-string (replace-regexp-in-string "['()]" "" value) " "))
      (push (string-to-number c 16) res)
      )
    (reverse res)))


(defun bufr-str-to-num (str &optional maxwidth)
  "Convert MAXWIDTH characters of string STR to a bytes in a very long integer."
  (let ((num 0))
    (unless (eq maxwidth nil)
      (if (> maxwidth (length str))
	  (setq str (concat str (make-string (- maxwidth (length str)) ?\40)))
	(setq str (substring str 0 maxwidth))
	))
    (dotimes (i (length str))
      (setf num (ash num 8))
      (setf num (logior num (logand (aref str i) 255))))
    num
    ))


(defun bufr-to-flat-bits (sectrevoct rval awidth)
  "Prepend SECTREVOCT with some bytes made of RVAL of AWIDTH bits length."
  (let ((v 0) (ub awidth) (lb bufr-cur_bit) rb foo)
    ;; ub: unprocessed bits
    ;; lb: left border bit, 0-7
    ;; rb: right border bit, 1-8
    (when (and (> bufr-cur_bit 0) (not (eq sectrevoct nil)))
      (setf v (pop sectrevoct)))
    (while (> ub 0)
      (if (< (+ lb ub) 8)
	  (progn
	    (setf rb (+ lb ub))
	    (setf foo (truncate rval)))
	(progn
	  (setf rb 8)
	  (setf foo (ash (truncate rval) (- (- 8 lb) ub)))))
      (setf foo (logand foo (- (ash 1 (- rb lb)) 1)))
      (setf v (logior v (ash foo (- 8 rb))))
      (setf ub (- ub (- rb lb)))
      (setf lb rb)
      (when (> lb 7)
	(setf lb 0))
      (push v sectrevoct)
      (setf v 0)
      )
    (setf bufr-cur_bit lb)
    )
  sectrevoct
  )


(defun bufr-enc-values (sectvalues)
  "Encode a list of values SECTVALUES, either uncompressed or compressed."
  ;; SECTVALUES looks like:
  ;; without compression: ((width.value) ...)
  ;; with compression   : (((width.typ) valueS1 valueS2 ...) ...)
  (let ((sectoctets'()))
    (if (= (gethash "comp" bufr-meta) 0)
	;; Write plain, uncompressed bits.
	(let (rval awidth)
	  (dolist (con sectvalues)
	    (setf awidth (car con))
	    (setf rval (cdr con))
	    (setq sectoctets (bufr-to-flat-bits sectoctets rval awidth))
	    ))
      ;; Write compressed bits
      (let (R0 Vx Vxr awidth typ)
	(dolist (valset sectvalues)
	  (setf awidth (car (car valset)))
	  (setf typ (cdr (car valset)))
	  (setf R0 (apply #'min (cdr valset)))  ;; reference value
	  (setf Vx (apply #'max (cdr valset)))  ;; maximum value, incl. "missing"
	  (setf Vxr (apply #'max (mapcar        ;; maximum value, without "missing"
				 (lambda (x)
				   (if (eq x (- (ash 1 awidth) 1)) 0 x))
				 (cdr valset))))
	  (if (= R0 Vx)
	      ;; All values are the same
	      (progn
		(setq sectoctets (bufr-to-flat-bits sectoctets R0 awidth))
		(setq sectoctets (bufr-to-flat-bits sectoctets 0 6)))
	    ;; Values are different
	    (let ((nbinc 0) (listIn '()) (bufIn '()) In maxIn)
	      (when (equal typ "string")
		(setf R0 0))
	      ;; Calculate bit width necessary for the maximum difference
	      (setf maxIn (- Vxr R0))
	      (while (> maxIn 0)
		(setf maxIn (ash maxIn -1))
		(setf nbinc (1+ nbinc)))
	      ;; Go through all values ...
	      (dolist (val (cdr valset))
		(setf In (- val R0))
		;; if a difference value In has set all bits of nbinc length increase nbinc
		(when (= In (- (ash 1 nbinc) 1))
		  (setf nbinc (1+ nbinc)))
		;; collect value or nil if the value equals "missing"
		(if (= val (- (ash 1 awidth) 1))
		    (push nil bufIn)
		  (push In bufIn)))
	      ;; Replace collected nil-values by a defference wit all bits set to 1
	      (dolist (In bufIn)
		(if (eq In nil)
		    (push (- (ash 1 nbinc) 1) listIn)
		  (push In listIn)))
	      ;; Write bits of reference value
	      (setq sectoctets (bufr-to-flat-bits sectoctets R0 awidth))
	      (if (equal typ "string")
		  (progn
		    ;; Write bits of nbinc for type "string", where nbinc is number of chararacters.
		    (when (/= (truncate (/ nbinc 8.0)) (/ nbinc 8.0))
		      (setf nbinc (* (1+ (truncate (/ nbinc 8.0))) 8)))
		    (setq sectoctets (bufr-to-flat-bits sectoctets (/ nbinc 8) 6)))
		;; Write bits of nbinc for all other types
		(setq sectoctets (bufr-to-flat-bits sectoctets nbinc 6)))
	      ;; Write bits of all increment values
	      (while (not (eq listIn nil))
		(setq sectoctets (bufr-to-flat-bits sectoctets (pop listIn) nbinc))
		)))
	  )))
    sectoctets
    ))


(defun bufr-set-value (sectrevvalues subsidx descidx desc typ factor offset width mgval)
  "Transforming values accordingly."
  (let (rval (qual nil) (awidth width) (afactor 0) (aoffset 0))
    (unless (or (equal (substring desc 0 3) "031")
		(= 0 (length (gethash "quality" bufr-modifier))))
      ;; Read and print associated data quality.
      ;; qual is list of bit-width, car of "quality" is the latest and significant.
					; TODO set qual
      (setf qual nil)
      )
    (cond
     ((or (equal typ "long") (equal typ "double"))
      ;; Read integer and float numbers
      (unless (equal (substring desc 0 3) "031")
	;; Apply various modifiers (but not for xx=31)
	(setf awidth (+ width (nth 0 (gethash "change" bufr-modifier))))
	(setf afactor (+ factor (nth 1 (gethash "change" bufr-modifier))))
	(setf aoffset (gethash desc (gethash "newreflst" bufr-modifier) offset))
	(setf aoffset (* aoffset (nth 2 (gethash "change" bufr-modifier))))
	(setf awidth (gethash "locwidth" bufr-modifier awidth)))
      (if (eq mgval nil)
          ;; If number equals "missing"
          (setq rval (- (ash 1 awidth) 1))
	;; calculate return number
	(setf rval (truncate (- (/ (string-to-number mgval) (expt 10 (- afactor))) aoffset))))
      )
     ((or (equal typ "table") (equal typ "flag"))
      (setf awidth (gethash "locwidth" bufr-modifier width))
      (if (eq mgval nil)
          (setq rval (- (ash 1 awidth) 1))
	(setf rval (string-to-number mgval)))
      )
     ((equal typ "string")
      (setf awidth (gethash "newstrlen" bufr-modifier width))
      (setf awidth (gethash "locwidth" bufr-modifier awidth))
      (if (eq mgval nil)
          (setq rval (- (ash 1 awidth) 1))
	(setf rval (bufr-str-to-num mgval (/ awidth 8))))
      ))
    (if (= (gethash "comp" bufr-meta) 0)
	;; Without compression: ((width.value) ...)
	(push (cons awidth rval) sectrevvalues)
      (progn
	;; With compression: (((width.typ) valueS1 valueS2 ...) ...)
	(when (= subsidx 0)
	  (push (cons (cons awidth typ) (make-list (gethash "subs" bufr-meta) nil)) sectrevvalues))
	(setf (nth (1+ subsidx) (nth (- (length sectrevvalues) 1 descidx) sectrevvalues)) rval)
	))
    (unless (equal (gethash "locwidth" bufr-modifier) nil)
      ;; Remove modifier for single "local descriptor width"
      (remhash "locwidth" bufr-modifier))
    sectrevvalues
    ))


(defun bufr-enc-eval-oper (x y mgval sectvalues subsidx descidx)
  "Handling the operator descriptors for encoding.

Implemented: fxx= 201 202 203 204 205 206 207 208"
  ;;
  ;; bufr-modifier:
  ;; "change"->(width factor offset)
  ;;
  (let (v a)
    (cond
     ((= x 1)
      ;; Add (YYY–128) bits to the data width given for each
      ;; data element in Table B, other than CCITT IA5
      ;; (character) data, code or flag tables.
      (setq a (gethash "change" bufr-modifier))
      (if (= y 0)
	  (setq v (list 0 (nth 1 a) (nth 2 a)))
	(setq v (list (- y 128) (nth 1 a) (nth 2 a))))
      (puthash "change" v bufr-modifier)
      )
     ((= x 2)
      ;; Add YYY–128 to the scale for each data element in
      ;; Table B, other than CCITT IA5 (character) data, code
      ;; or flag tables.
      (setq a (gethash "change" bufr-modifier))
      (if (= y 0)
	  (setq v (list (nth 0 a) 0 (nth 2 a)))
	(setq v (list (nth 0 a) (- y 128) (nth 2 a))))
      (puthash "change" v bufr-modifier)
      )
     ((= x 5)
      ;; YYY characters (CCITT International Alphabet No. 5) are
      ;; inserted as a data field of YYY x 8 bits in length.
      (setq sectvalues (bufr-set-value sectvalues subsidx descidx "205000" "string" 0 0 (* y 8) mgval))
      )
     ((= x 6)
      ;; YYY bits of data are described by the immediately
      ;; following descriptor.
      (puthash "locwidth" y bufr-modifier)
      )
     ((= x 7)
      ;; For Table B elements, which are not CCITT IA5
      ;; (character data), code tables, or flag tables:
      ;; 1. Add YYY to the existing scale factor
      ;; 2. Multiply the existing reference value by 10^YYY
      ;; 3. Calculate ((10*YYY)+2)/3, disregard any
      ;;    fractional remainder and add the result to the
      ;;    existing bit width.
      (setq a (gethash "change" bufr-modifier)) ; (width scale reference)
      (if (= y 0)
	  (setq v (list 0 0 1))
	(setq v (list (/ (+ (* 10 y) 2) 3)
		      (+ (nth 1 a) y)
		      (expt 10 y))))
      (puthash "change" v bufr-modifier)
      )
     ((= x 8)
      ;; YYY characters from CCITT International Alphabet
      ;; No. 5 (representing YYY x 8 bits in length) replace the
      ;; specified data width given for each CCITT IA5
      ;; element in Table B.
      (if (= y 0)
	  (remhash "newstrlen" bufr-modifier)
	(puthash "newstrlen" (* y 8) bufr-modifier))
      )
     (t
      (error (format "Operator '2%02d%03d' not implemented!" x y))
      ))
    sectvalues
    ))


(defun bufr-enc-sect-1to3 ()
  "Encoding metadata to sections 1, 2, and 3."
  (let (val foo bar mgkey mgval sectlen sectoctets s1sizes (s2present 0) (udlst '()))
    ;; Test validity
    (goto-char (point-min))
    (setq mgkey (buffer-substring (point) (+ (point) 4)))
    (unless (equal mgkey "BUFR")
      (error "Not well-formatted text!"))
    (beginning-of-line)
    (forward-line)
    ;; Section 1
    (setq s1sizes '(("centr" . 2) ("scntr" . 2) ("upseq" . 1)
		    ("sect2" . 1) ("cat" . 1) ("scat" . 1)
		    ("lscat" . 1) ("mvers" . 1) ("lvers" . 1)
		    ("year" . 2) ("month" . 1) ("day" . 1)
		    ("hour" . 1) ("min" . 1) ("sec" . 1)
		    ("adpd" . -1) ("edit" . -1) ))
    (when (eq (re-search-forward "^\\([a-z2]+\\) +[^:]+: \\(.+\\)$" nil t) nil)
      (error "Not well-formatted text!"))
    (setq mgkey (match-string 1))
    (setq mgval (match-string 2))
    (forward-line)
    (while (not (eq (assoc mgkey s1sizes) nil))
      (cond
       ((equal mgkey "sect2")
	(if (equal mgval "1")
	    (progn
	      (setq val (list 128))
	      (setf s2present 1))
	  (setq val (list 0))))
       ((equal mgkey "adpd")
	(if (> (length mgval) 2)
	    (setq val (bufr-hex-to-num mgval))
	  (setq val nil)))
       ((equal mgkey "edit")
        (unless (equal mgval "4")
          (error (format "BUFR edition '%s' not supported!" mgval))
          ))
       (t
	(setq val (bufr-to-bytes (cdr (assoc mgkey s1sizes)) (string-to-number mgval)))))
      (setq sectoctets (append sectoctets val))
      (if (equal mgkey "adpd")
	  (puthash mgkey mgval bufr-meta)
	(puthash mgkey (string-to-number (car (split-string mgval " "))) bufr-meta))
      (re-search-forward "^\\([a-z2]+\\) +[^:]+: \\(.+\\)$")
      (setq mgkey (match-string 1))
      (setq mgval (match-string 2))
      (forward-line)
      ) ;; end while
    (setq val (bufr-to-bytes 3 (+ (length sectoctets) 4)))
    (setq bufr-olist (append bufr-olist val (list 0) sectoctets))
    ;; Section 2
    (when (and (equal mgkey "s2dat") (= s2present 1))
      (setq sectoctets (bufr-hex-to-num mgval))
      (setq val (bufr-to-bytes 3 (+ (length sectoctets) 4)))
      (setq bufr-olist (append bufr-olist val (list 0) sectoctets))
      (re-search-forward "^\\([a-z2]+\\) +[^:]+: \\(.+\\)$")
      (setq mgkey (match-string 1))
      (setq mgval (match-string 2))
      (forward-line))
    ;; Section 3
    (setf foo 0)
    (while (not (eq mgkey nil))
      (cond
       ((equal mgkey "subs")
	(setq sectoctets (bufr-to-bytes 2 (string-to-number mgval)))
	(puthash mgkey (string-to-number mgval) bufr-meta)
	)
       ((and (equal mgkey "obs") (equal mgval "1"))
	(setf foo (logior foo 128))
	)
       ((equal mgkey "comp")
	(when (equal mgval "1")
	  (setf foo (logior foo 64)))
	(puthash mgkey (string-to-number mgval) bufr-meta)
	)
       ((equal mgkey "udesc")
	(dolist (ud (split-string (substring mgval 1 -1) " "))
	  (setf bar (ash (string-to-number (substring ud 0 1)) 6))
	  (setf bar (logior bar (string-to-number (substring ud 1 3))))
	  (push bar udlst)
	  (setf bar (string-to-number (substring ud 3 6)))
	  (push bar udlst)
	  )
	)
					;       (t
					;	(error (format "Unknown key '%s'!" mgkey))
					;	)
       )
      (if (eq (re-search-forward "^\\([a-z2]+\\) +[^:]+: \\(.+\\)$" nil t) nil)
	  (setq mgkey nil)
	(progn
	  (setq mgkey (match-string 1))
	  (setq mgval (match-string 2))))
      (forward-line)
      )
    (setq udlst (reverse udlst))
    (setq sectoctets (append sectoctets (list foo) udlst))
    (setq sectlen (bufr-to-bytes 3 (+ (length sectoctets) 4)))
    (setq bufr-olist (append bufr-olist sectlen (list 0) sectoctets))
    ))


(defun bufr-enc-sect-4 ()
  "Encoding data section 4."
  (let (desc mgval f x y (descidx 0) (sectoctets '()) (sectvalues '()) sectlen)
    ;; Without compression SECTOCTETS is one-dimensional, all values consecutive in one list.
    ;; With compression (see BUFR-META{"comp"}) it is two-dimensional:
    ;; per descriptor a list of values of all subsets,
    ;; at the end of this function all octets are concat'inated.
    (dotimes (subsidx (gethash "subs" bufr-meta))
      ;; Reset modifiers set by fxx=201|202|203|204|207|208
      (setq bufr-modifier (make-hash-table :test 'equal))
      ;; Reset modifiers for new subset:
      ;; "change"->(width factor offset)
      ;; width and factor are added, offset is multiplied with existing values
      (puthash "change" (list 0 0 1) bufr-modifier)
      (puthash "quality" '() bufr-modifier)
      (remhash "newstrlen" bufr-modifier)
      (puthash "newreflst" (make-hash-table :test 'equal) bufr-modifier)
      (setf descidx 0)
      ;; Read/match first line after "------"
      (re-search-forward "^\\([0-9]\\{6\\}\\) +[^:]+: \\(.+\\)$" nil t)
      (setq desc (match-string 1))
      (setq mgval (match-string 2))
      (forward-line)
      (while (not (eq desc nil))
	;; Encode value per descriptor
	(setf f (string-to-number (substring desc 0 1)))
	(setf x (string-to-number (substring desc 1 3)))
	(setf y (string-to-number (substring desc 3 6)))
	(when (equal (string-trim mgval) "nil")
	  (setq mgval nil))
	(cond
	 ((= f 0)
	  ;; Data descriptor
	  (let (tabentry typ fact offs wdth)
	    (setq tabentry (gethash desc bufr-tab_b))
	    (setf typ (nth 1 tabentry))
	    (setf fact (string-to-number (nth 4 tabentry)))
	    (setf offs (string-to-number (nth 5 tabentry)))
	    (setf wdth (string-to-number (nth 6 tabentry)))
	    (setq sectvalues (bufr-set-value sectvalues subsidx descidx desc typ fact offs wdth mgval))
	    (setf descidx (1+ descidx))
	    ))
	 ((= f 1)
	  ;; nothing to encode
	  )
	 ((= f 2)
	  ;; evaluate descriptor for modifiers
	  (setq sectvalues (bufr-enc-eval-oper x y mgval sectvalues subsidx descidx))
	  )
	 ((= f 3)
	  ;; nothing to encode
	  )
	 )
	;; Read/match next line
	(if (eq (re-search-forward "^\\([0-9]\\{6\\}\\) +[^:]+: \\(.+\\)$" (line-end-position) t) nil)
	    ;; Caused by the subset delimiter "------", this stops the while-loop
	    (setq desc nil)
	  (progn
	    (setq desc (match-string 1))
	    (setq mgval (match-string 2))))
	(forward-line)
	)) ; end while, dotimes
    (setq sectoctets (bufr-enc-values (reverse sectvalues)))
    ;; append section size and reversed sectoctets to olist
    (setq sectlen (bufr-to-bytes 3 (+ (length sectoctets) 4)))
    (setq bufr-olist (append bufr-olist sectlen (list 0) (reverse sectoctets)))
    ))


(defun bufr-enc-sect-0fin ()
  "Encode section 0 with overall length and write all bytes as characters."
  (let (tsz txt)
    (setf tsz (+ (length bufr-olist) 12))
    (setq txt (bufr-to-bytes 4 (bufr-str-to-num "BUFR")))
    (setq bufr-olist (append txt
			     (bufr-to-bytes 3 tsz)
			     (list 4)
			     bufr-olist
			     (bufr-to-bytes 4 (bufr-str-to-num "7777"))
			     ))
    ))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main interacive functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bufr-help ()
  "Show help text."
  (interactive)
  (message "***** Decode/encode WMO FM94 BUFR message in current buffer. *****

The environment variable $BUFR_TABLES must point to a directory where
table files (eccodes-style) are located.

*** Decode, 'M-x bufr-decode': ***

In a buffer A containing at least one BUFR message move the cursor over the
message to decode, and start decoding with 'M-x bufr-decode'.
A message starts with the keyword `BUFR´ and ends with `7777´.

The decoded text is displayed in a new buffer B, with it's name set to
A's name plus `-decoded´. In A the curser is set to the start of the message."
	   ))


(defun bufr-decode ()
  "Decode WMO FM94 BUFR message."
  (interactive)
  (let (bnam dnam r)
    ;; Initialise decoding basics
    (setq case-fold-search nil)
    (setf bufr-start -1)
    (setf bufr-end -1)
    (setf bufr-cur_bit 0)
    ;; Also find the keyword when point is on top of it.
    ;; Avoid going past end of buffer.
    (unless (> (+ (point) 4) (point-max))
      (forward-char 4))
    ;; Search for keyword "BUFR", at first backwards from current point.
    ;; When start of buffer is reached search forward.
    (if (search-backward "BUFR" nil t)
	(setf bufr-start (point))
      (if (search-forward "BUFR" nil t)
	  (progn
	    (backward-char 4)
	    (setf bufr-start (point)))
	(error "No BUFR found!")))
    ;; Store buffer with binary BUFR data.
    (setq bufr-obuf (current-buffer))
    ;; Prepare buffer for output
    (setq bnam (buffer-name))
    (setq dnam (concat bnam "-decoded"))
    (setq bufr-dbuf (get-buffer-create dnam))
    (with-current-buffer bufr-dbuf
      (erase-buffer))
    (condition-case err
	(progn
	  (save-excursion
	    ;; In current buffer decode BUFR sections with metadata
	    (bufr-dec-sect-0to1)
	    ;; Load tables if neccessary
	    (bufr-read-table-files "m" (gethash "mvers" bufr-meta))
	    (bufr-read-table-files "l" nil
				   (gethash "centr" bufr-meta)
      				   (gethash "scntr" bufr-meta)
       				   (gethash "lvers" bufr-meta))
	    ;; In current buffer decode BUFR sections with descriptor list and subset data
	    (setq r (bufr-dec-sect-2to5))
	    (setf bufr-end (point))))
      (error (setq r (format "%s" err))))
    ;; All decoding is done, switch to the buffer with decoded data.
    (switch-to-buffer bufr-dbuf)
    (if (not (equal r nil))
	(progn
	  (goto-char (point-max))
	  (message r))
      (progn
	(goto-char (point-min))
	(message "all ok")))
    ))


(defun bufr-encode ()
  "Encodes well-formatted text to WMO FM94 BUFR message."
  (interactive)
  (let (bnam dnam)
    (setq inhibit-eol-conversion t)
    (setq bufr-cur_bit 0)
    (setq bufr-olist '())
    (setq bufr-dbuf (current-buffer))
    (setq dnam (buffer-name))
    (setq bnam (replace-regexp-in-string "-decoded" "" dnam))
;    (when (eq bufr-obuf nil)
      (setq bnam (concat dnam "-encoded"))
      (setq bufr-obuf (get-buffer-create bnam)) ; )
    (condition-case err
     	(progn
     	  (save-excursion
     	    ;; In current buffer decode BUFR sections with metadata
	    (bufr-enc-sect-1to3)
     	    ;; Load tables if neccessary
	    (bufr-read-table-files "m" (gethash "mvers" bufr-meta))
	    (bufr-read-table-files "l" nil
	     			   (gethash "centr" bufr-meta)
	       			   (gethash "scntr" bufr-meta)
	        		   (gethash "lvers" bufr-meta))
	    ;; In current buffer decode BUFR sections with descriptor list and subset data
	    (bufr-enc-sect-4)
	    (bufr-enc-sect-0fin))
	  (with-current-buffer bufr-obuf
	    (when (< bufr-start (point-min))
	      (setf bufr-start (point-min)))
	    (when (or (> bufr-end (point-max)) (= bufr-end -1))
	      (setf bufr-end (point-max)))
	    (goto-char bufr-start)
	    (when (> bufr-end bufr-start)
	      (kill-region bufr-start bufr-end))
	    (insert (encode-coding-string (concat bufr-olist) 'iso-8859-1))
	    ))
      (error (message (format "%s" err))))
    ;; All decoding is done, switch to the buffer with decoded data.
    (switch-to-buffer bufr-obuf)
    (goto-char bufr-start)
    ))
;;
;; END
;;

(provide 'bufr-help)
(provide 'bufr-decode)
(provide 'bufr-encode)

;;; fm94bufr.el ends here
