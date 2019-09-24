;;; tex-hide-setup-ert-tests.el --- Provide needed forms -*- lexical-binding: t; -*-

;; Authored and maintained by
;; Emacs User Group Berlin <emacs-berlin@emacs-berlin.org>

;; Keywords: lisp

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

;;; Commentary:

;;; Code:

(defvar tex-hide-debug-p nil
  "Avoid error")
;; (setq tex-hide-debug-p t)


(defmacro tex-hide-test (contents mode verbose &rest body)
  "Create temp buffer inserting CONTENTS.

BODY is code to be executed within the temp buffer "
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (insert ,contents)
       (funcall ,mode)
       (when ,verbose
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-region (point-min)(point-max))
       ,@body))
  ;; (sit-for 0.1)
  ))

(defmacro tex-hide-test-point-min (contents mode verbose &rest body)
  "Create temp buffer inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (funcall ,mode)
       (insert ,contents)
       (goto-char (point-min))
       (when ,verbose
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-region (point-min)(point-max)))
       ,@body)))

(defmacro tex-hide-test-with-tex-buffer (contents &rest body)
  "Create temp buffer in `tex-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (insert ,contents)
       (tex-mode)
       (when ,tex-hide-debug-p (switch-to-buffer (current-buffer))
	     (font-lock-fontify-region (point-min)(point-max)))
       ,@body)))

(defmacro tex-hide-test-with-tex-buffer-point-min (contents &rest body)
  "Create temp buffer in `tex-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (insert ,contents)
       (tex-mode)
       (goto-char (point-min))
       (when tex-hide-debug-p (switch-to-buffer (current-buffer))
	     (font-lock-fontify-region (point-min) (point-max)))
       ,@body)
     ))

(provide 'tex-hide-setup-ert-tests)
;; tex-hide-setup-ert-tests.el ends here
