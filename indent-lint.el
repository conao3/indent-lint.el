;;; indent-lint.el --- Async indentation checker  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: tools
;; Package-Requires: ((emacs "26"))
;; License: GPL-3.0
;; URL: https://github.com/conao3/indent-lint.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Async indentation checker


;;; Code:

(require 'diff)

(defgroup indent-lint nil
  "Asynchronous indentation checker"
  :prefix "indent-lint-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/conao3/indent-lint.el"))

(defvar indent-lint-exit-code nil
  "Diff exit code.")

(defvar indent-lint-advice-alist
  '((diff-sentinel . indent-lint-advice--diff-sentinel))
  "Alist for indent-lint advice.
See `indent-lint-setup' and `indent-lint-teardown'.")

(defun indent-lint-advice--diff-sentinel (fn &rest args)
  "Advice for `diff-sentinel'.
FN is `diff-sentinel', ARGS is its arguments."
  (setq indent-lint-exit-code (nth 0 args))
  (apply fn args))

(defun indent-lint (&optional buf)
  "Indent lint for BUF.
If omit BUF, lint `current-buffer'."
  (interactive)
  (let* ((buf* (or buf (current-buffer)))
         (contents (with-current-buffer buf*
                     (buffer-string)))
         (mode (with-current-buffer buf*
                 major-mode))
         (diff-buffer (get-buffer-create "*indent-lint diff*")))
    (with-temp-buffer
      (insert contents)
      (funcall mode)
      (indent-region (point-min) (point-max))
      (diff-no-select buf (current-buffer)
                   nil nil diff-buffer))
    (unless (eq 0 indent-lint-exit-code)
      (display-buffer diff-buffer))
    diff-buffer))

;;;###autoload
(defun indent-lint-setup ()
  "Setup indent-lint."
  (interactive)
  (pcase-dolist (`(,sym . ,fn) indent-lint-advice-alist)
    (advice-add sym :around fn)))

;;;###autoload
(defun indent-lint-teardown ()
  "Setup indent-lint."
  (interactive)
  (pcase-dolist (`(,sym . ,fn) indent-lint-advice-alist)
    (advice-remove sym fn)))

(provide 'indent-lint)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; indent-lint.el ends here
