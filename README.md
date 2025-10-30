# elisp-fm94bufr

Emacs-plugin de-/encoding WMO FM94 BUFR messages.

> The Environment variable $BUFR_TABLES must point to a
> directory where table files (eccodes-style) are located.

(C) 2025 alexmaul

### Licence

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

## Auto-load Plugin

- Windows:

  Emacs config file `C:\Users\<USER>\AppData\Roaming\.emacs`

  Extension-path `C:\Users\<USER>\AppData\Roaming\.emacs.d\extensions`

- Linux

  Emacs config file `$HOME/.emacs`
  
  Extension-path `$HOME/.emacs.d/extensions`

Place the file `fm94bufr.el` from this project in the extension-path and add
the follwing lines to your Emacs config file:

```
(add-to-list 'load-path "c:/Users/amaul/AppData/Roaming/emacs.d/extensions")
(require 'bufr-decode "fm94bufr.el")
```

## Usage

The command `M-x bufr-help` displays this instructions.

### Decode, `M-x bufr-decode`

In a buffer *A* containing at least one BUFR message move the cursor over the
message to decode, and start decoding with `M-x bufr-decode`.
A message starts with the keyword `BUFR` and ends with `7777`.

The decoded text is displayed in a new buffer *B*, with it's name set to
*A*'s name plus `-decoded´. In *A* the curser is set to the start of the message.

### Encode, `M-x bufr-encode`

In a buffer *B* with text decoded from a BUFR message or such text loaded from
a file start encoding with `M-x bufr-encode`.

This buffer *B* can contain only the text of *one* decoded BUFR message!
It'll be evaluated from start to end (point-min to point-max).

In case the text in buffer *B* is the result from a previously decoded a BUFR
message from buffer *A* the start and end points in *A* are remembered and the
encoding of *B* will replace the BUFR in *A*.

You might edit the decoded text to your likings, only you must preserve the
overall structure. Important for the encoding are the first column (keywords
or descriptors) and the values after the first colon `:`. Any text string
following this numeric value is discarded as it was only verbose translation
of the numeric values -- unless the descriptor describes a value of type
"string", in which case all text up to the line-break is encoded accordingly.

If you change the number of subsets or replications, you must reduce or extend
the list of affected descriptors/lines accordingly. Otherwise the encoding
process will miss-step and throw an error or the created BUFR message is faulty.

### Reset encoding-target, `M-x bufr-reset-encode`

As the meta-command `bufr-encode` is intended to replace a previously decoded
BUFR you can reset the target buffer for encoding with `M-x bufr-reset-encode`.

The following command `bufr-encode` will create a new buffer with the name of
buffer *B* plus "-encoded".
