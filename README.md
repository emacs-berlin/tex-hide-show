Tex Hide Show [![Build Status](https://travis-ci.org/emacs-berlin/tex-hide.svg?branch=master)](https://travis-ci.org/emacs-berlin/tex-hide)

# tex-hide-show.el

Hide LaTeX section, subsection etc. in a way Emacs org-mode does WRT title and subtitle.

M-x ‘tex-hide-show-mode’ RET enables it in current buffer.

Command ‘eb-tex-hide-show-section’ toggles visibility of a section at
point. With optional "\C-u" also subsections are affected.

When ‘eb-tex-hide-cycle-p’ is customized to ‘t’, cycles levels resp.
unhides as org-mode's cycling does.

‘eb-tex-hide-cycle-p’ defaults to nil.

See also ‘eb-tex-hide-show-region’.

Bug-reports are welcome at:
https://github.com/emacs-berlin/tex-hide-show/issues