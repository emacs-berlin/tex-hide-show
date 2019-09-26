;;; tex-hide-show.el --- Hide-show LaTeX markup sections  -*- lexical-binding: t; -*-

;; Author: Emacs User Group Berlin <emacs-berlin@emacs-berlin.org>
;; Maintainer: Emacs User Group Berlin <emacs-berlin@emacs-berlin.org>

;; Version: 0.1

;; URL: https://github.com/emacs-berlin/tex-hide-show

;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
;; Keywords: languages, convenience

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

;;; Commentary: Hide resp. show hidden LaTeX section, subsection etc.
;; as org-mode does WRT title and subtitle.

;; M-x tex-hide-show-mode RET activates the minor-mode. Command
;; ‘eb-tex-hide-show-section’ toggles visibility of a section at
;; point. With optional "\C-u" also subsections are affected.

;; When ‘eb-tex-hide-cycle-p’ is customized to ‘t’, cycles levels
;; resp. unhides as org-mode's cycling does.

;; ‘eb-tex-hide-cycle-p’ defaults to nil.

(require 'hideshow)

(eval-and-compile
  (defconst eb-tex-hide-rx-all-section-start
    (rx (or "subsubsection" "subparagraph" "subsection" "paragraph" "section"))))

(defconst eb-tex-hide-section-stop
    "section"
    "Hiding subsections must stop at next sextion")

(defcustom eb-tex-hide-cycle-p nil
  "Cycle section and subsections like org-mode does WRT levels.

Default is nil "
  :type 'boolean
  :group 'convenience)

(defvar eb-tex-hide-cycling-commands (list 'eb-tex-hide-show-section))

(defvar eb-hide-show--cycle-now nil
  "Internally used")

(defun eb--in-string-or-comment-p ()
    "Returns beginning position if inside a string or comment,
t at start,nil otherwise. "
    (interactive)
    (let* ((erg (or (nth 8 (parse-partial-sexp (point-min) (point)))
		    (ignore-errors (eq 7 (car-safe (syntax-after (point)))))
		    (looking-at comment-start)
		    (looking-at comment-start-skip))))
      (when (interactive-p) (message "%s" erg))
      erg))

(defun eb-tex-hide-backward-base (form start-re)
  "Move backward to the beginning of a LaTeX markup."
  (unless (eb-tex-hide--beginning-of start-re)
    (let (last done)
      (while (and (not done) (not (bobp)) (re-search-backward (regexp-quote (char-to-string 92)) nil t) (setq last (point)))
	(and (looking-at (concat (regexp-quote (char-to-string 92)) form))) (setq done t))
      (when last (goto-char last)))))

(defun eb-tex-hide-forward-base (start-re &optional arg stop-re)
  "Move forward to the beginning of a LaTeX markup.

Arg FORM: a regular expression like ‘eb-tex-hide-rx-all-section-start’"
  (let ((orig (point))
	last
	done)
    (if (eq 4 (prefix-numeric-value arg))
	;; hide also subsections
	(progn
	  (end-of-line)
	  (while (and (not done) (not (eobp))
		      (re-search-forward (concat (regexp-quote (char-to-string 92)) start-re))
		      (or (eb--in-string-or-comment-p) (setq last (point))))
	    (when (and (not (eb--in-string-or-comment-p)) stop-re (looking-back (concat (regexp-quote (char-to-string 92)) stop-re))) (setq done t)))
	  (when last (goto-char last)
		(forward-paragraph)))
      ;; hide a single section
      (end-of-line)
      (or (and (re-search-forward (concat (regexp-quote (char-to-string 92)) start-re) nil t) (setq last (line-beginning-position)))
	  (forward-paragraph)))

    (when last (goto-char last))
    (skip-chars-backward " \t\r\n\f")
    (and (< orig (point)) (point))))

(defun eb-tex-hide--beginning-of (start-re)
  "Returns position if at a beginning, nil otherwise."
  (and (not (eb--in-string-or-comment-p))
       (looking-at (concat (regexp-quote (char-to-string 92)) start-re))
       (point)))

(defun eb-tex-hide-base (&optional form arg start-re stop-re beg end)
  "Hide visibility of existing form at point."
  (hs-minor-mode 1)
  (save-excursion
    (let* ((form (prin1-to-string form))
           (beg (or beg
		    (eb-tex-hide--beginning-of start-re)
		    (eb-tex-hide-backward-base form start-re)))
	   (end (or end
		    (eb-tex-hide-forward-base start-re arg stop-re)))
	   (modified (buffer-modified-p))
	   (inhibit-read-only t))
      (if (and beg end)
	  (progn
	    (hs-make-overlay beg end 'code)
	    (set-buffer-modified-p modified))
	(error (concat "No " (format "%s" form) " at point"))))))

(defun eb-tex-hide-show-intern (arg form &optional start-re stop-re beg end)
  "Make LaTeX section at point invisible.

With \\[universal-argument] hide all subsections"
  (cond ((not (or (overlays-at (point)) (overlays-at (1- (point)))))
	 (setq eb-hide-show--cycle-now nil)
	 (eb-tex-hide-base form arg start-re stop-re beg end))
	((and eb-tex-hide-cycle-p (not eb-hide-show--cycle-now) (or (overlays-at (point)) (overlays-at (1- (point))))
	      (member last-command eb-tex-hide-cycling-commands))
	 (setq eb-hide-show--cycle-now (not eb-hide-show--cycle-now))
	 (eb-tex-hide-base form '(4) start-re stop-re))
	((overlays-at (point))
	 (setq eb-hide-show--cycle-now nil)
	 (eb-tex-show))
	((overlays-at (1- (point)))
	 (setq eb-hide-show--cycle-now nil)
	 (forward-char -1)
	 (eb-tex-show))
	(t (setq eb-hide-show--cycle-now nil)
	   (eb-tex-hide-base form arg start-re stop-re))))

(defun eb-tex-show ()
  "Remove invisibility of existing form at point."
  (interactive)
  (with-silent-modifications
    (save-excursion
      ;; (back-to-indentation)
      (let ((end (next-overlay-change (point))))
	(hs-discard-overlays (point) end)))))

(defun eb-tex-show-all ()
  "Remove invisibility of hidden forms in buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (end)
      (while (and (not (eobp))  (setq end (next-overlay-change (point))))
	(hs-discard-overlays (point) end)
	(goto-char end)))))

(defun eb-tex-hide-show-region (beg end)
"Toggles visibility of an active region at point."
  (interactive "r")
  (eb-tex-hide-show-intern nil 'region nil nil beg end))

(defun eb-tex-hide-show-section (&optional arg)
  "Hide resp. unhide LaTeX section at point.

With optional \\[universal-argument] also subsections are affected"
  (interactive "P")
  (save-excursion (eb-tex-hide-show-intern arg 'section eb-tex-hide-rx-all-section-start eb-tex-hide-section-stop)))

(defvar tex-hide nil)


(defvar tex-hide-show-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") 'eb-tex-hide-show-section)
    map))

;; (define-derived-mode tex-hide-show-mode hs-minor-mode nil
(define-minor-mode tex-hide-show-mode
    "Build on top of hs-minor-mode" nil nil tex-hide-show-mode-map)

(defalias 'eb-show-all 'hs-show-all)

(provide 'tex-hide-show)
;;; tex-hide-show.el ends here
