;;; tex-hide-tests.el ---  Tests

;; Authored and maintained by
;; Emacs User Group Berlin <emacs-berlin@emacs-berlin.org>

;; Keywords: languages, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: A still naive implementation of a tex-hide command

;;

;;; Code:

(require 'tex-hide-setup-ert-tests)
(require 'tex-hide-show)

(defvar tex-hide-debug-p nil
  "Avoid error")

;; (setq tex-hide-debug-p t)


;; Elisp
(ert-deftest tex-hide-section-test-rC1CDh ()
  (tex-hide-test-with-tex-buffer-point-min
      "\\documentclass{article}
\\usepackage{titlesec}
\\ Just some testfile
\\setcounter{secnumdepth}{4}

\\titleformat{\\paragraph}
{\\normalfont\\normalsize\\bfseries}{\\theparagraph}{1em}{}
\\titlespacing*{\\paragraph}
{0pt}{3\.25ex plus 1ex minus \.2ex}{1\.5ex plus \.2ex}

\\begin{document}

\\section{Test Section}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\subsection{Test Subsection}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\subsubsection{Test Subsubsection}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\paragraph{Test Paragraph}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\subparagraph{Test Subparagraph}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\paragraph{Test Other Paragraph}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\subparagraph{Test Subparagraph}

  Lorem ipsum dolor
  sit amet consectetuer adipiscing
  elit
\\section{Test Section}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\subsection{Test Subsection}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\end{document}"
    (goto-char (point-min))
    (search-forward "Lorem ipsu")
    (should-not (overlays-at (point)))
    (eb-tex-hide-show-section)
    (should (overlays-at (point)))
    (save-excursion
      (search-forward "Test Subsection")
      (should-not (overlays-at (point))))
    (eb-tex-show)
    (should-not (overlays-at (point)))))

(ert-deftest tex-hide-section-test-GFX7HC ()
  (tex-hide-test-with-tex-buffer-point-min
      "\\documentclass{article}
\\usepackage{titlesec}
\\ Just some testfile
\\setcounter{secnumdepth}{4}

\\titleformat{\\paragraph}
{\\normalfont\\normalsize\\bfseries}{\\theparagraph}{1em}{}
\\titlespacing*{\\paragraph}
{0pt}{3\.25ex plus 1ex minus \.2ex}{1\.5ex plus \.2ex}

\\begin{document}

\\section{Test Section}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\subsection{Test Subsection}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\subsubsection{Test Subsubsection}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\paragraph{Test Paragraph}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\subparagraph{Test Subparagraph}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\paragraph{Test Other Paragraph}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\subparagraph{Test Subparagraph}

  Lorem ipsum dolor
  sit amet consectetuer adipiscing
  elit
\\section{Test Section}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\subsection{Test Subsection}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\end{document}"
    (goto-char (point-min))
    (search-forward "Lorem ipsu")
    (should-not (overlays-at (point)))
    (eb-tex-hide-show-section '(4))
    (should (overlays-at (point)))
    (save-excursion
      (search-forward "subparagraph")
      (should (overlays-at (point))))
    (eb-tex-show)
    (should-not (overlays-at (point)))
    ))

(ert-deftest tex-hide-section-test-jINrLV ()
  (tex-hide-test-with-tex-buffer-point-min
      "\\documentclass{article}
\\usepackage{titlesec}
\\ Just some testfile
\\setcounter{secnumdepth}{4}

\\titleformat{\\paragraph}
{\\normalfont\\normalsize\\bfseries}{\\theparagraph}{1em}{}
\\titlespacing*{\\paragraph}
{0pt}{3\.25ex plus 1ex minus \.2ex}{1\.5ex plus \.2ex}

\\begin{document}

\\section{Test Section}
Lorem ipsum dolor sit amet consectetuer adipiscing elit
\\end{document}"
    (goto-char (point-min))
    (search-forward "Test Section")
    (beginning-of-line) 
    (eb-tex-hide-show-section)
    (save-excursion
      (search-backward "document")
    (should-not (overlays-at (point)))
    )))


(ert-deftest tex-hide-section-test-enagED ()
  (tex-hide-test-with-tex-buffer-point-min
      "\\documentclass{article}
\\usepackage{titlesec}
\\ Just some testfile
\\setcounter{secnumdepth}{4}

\\titleformat{\\paragraph}
{\\normalfont\\normalsize\\bfseries}{\\theparagraph}{1em}{}
\\titlespacing*{\\paragraph}
{0pt}{3\.25ex plus 1ex minus \.2ex}{1\.5ex plus \.2ex}

\\begin{document}

\\section{Test Section}
Lorem ipsum dolor sit amet consectetuer adipiscing elit
\\end{document}"
    (goto-char (point-min))
    (search-forward "{Test Section")
    (search-backward "{") 
    (eb-tex-hide-show-section)
    (should (overlays-at (point)))
    (eb-tex-show)
    (should-not (overlays-at (point)))
    ))

(ert-deftest tex-hide-section-test-KMsWvN ()
  (tex-hide-test-with-tex-buffer-point-min
      "\\documentclass{article}
\\usepackage{titlesec}
\\ Just some testfile
\\setcounter{secnumdepth}{4}

\\titleformat{\\paragraph}
{\\normalfont\\normalsize\\bfseries}{\\theparagraph}{1em}{}
\\titlespacing*{\\paragraph}
{0pt}{3\.25ex plus 1ex minus \.2ex}{1\.5ex plus \.2ex}

\\begin{document}

\\section{Test Section}
Lorem ipsum dolor sit amet consectetuer adipiscing elit
\\end{document}"
    (goto-char (point-min))
    (search-forward "{Test Section")
    (search-backward "{")
    (push-mark)
    (forward-sexp)
    (exchange-point-and-mark) 
    (eb-tex-hide-show-region (point) (mark))
    (should (overlays-at (point)))
    (eb-tex-show)
    (should-not (overlays-at (point)))
    ))

(ert-deftest tex-hide-section-test-mPrnZD ()
  (tex-hide-test-with-tex-buffer-point-min
      "\\documentclass{article}
\\usepackage{titlesec}
\\ Just some testfile
\\setcounter{secnumdepth}{4}

\\titleformat{\\paragraph}
{\\normalfont\\normalsize\\bfseries}{\\theparagraph}{1em}{}
\\titlespacing*{\\paragraph}
{0pt}{3\.25ex plus 1ex minus \.2ex}{1\.5ex plus \.2ex}

\\begin{document}

\\section{Test Section}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\subsection{Test Subsection}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\subsubsection{Test Subsubsection}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\paragraph{Test Paragraph}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\subparagraph{Test Subparagraph}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\paragraph{Test Other Paragraph}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\subparagraph{Test Subparagraph}

  Lorem ipsum dolor
  sit amet consectetuer adipiscing
  elit

% \\section{Test Section}
% Lorem ipsum dolor sit amet consectetuer adipiscing elit
% 
% \\subsection{Test Subsection}
% Lorem ipsum dolor sit amet consectetuer adipiscing elit
% 

\\section{Test Section}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\subsection{Test Subsection}
Lorem ipsum dolor sit amet consectetuer adipiscing elit

\\end{document}"
    (goto-char (point-min))
    (search-forward "Lorem ipsu")
    (should-not (overlays-at (point)))
    (eb-tex-hide-show-section '(4))
    (should (overlays-at (point)))
    (save-excursion
      (search-forward "subparagraph")
      (should (overlays-at (point))))
    (eb-tex-show)
    (should-not (overlays-at (point)))
    ))

(provide 'tex-hide-tests)
;;; tex-hide-tests.el ends here
