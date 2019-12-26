;;; indent-lint.el --- Async indentation checker  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: tools
;; Package-Requires: ((emacs "24.4"))
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

(defvar indent-lint-initialized nil
  "Non-nil means initialized `indent-lint'.
See `indent-lint-setup' and `indent-lint-teardown'.")

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
  (when (not indent-lint-initialized)
    (error "Initialize `indent-lint' needed.  Eval `indent-lint-setup' before using this"))
  (let* ((buf* (or buf (current-buffer)))
         (contents (with-current-buffer buf*
                     (buffer-string)))
         (diff-buffer (get-buffer-create "*indent-lint diff*"))
         (diff-buffer-with-line (get-buffer-create "*indent-lint diff with line*")))
    (with-temp-buffer
      (insert contents)
      (let ((buffer-file-name (buffer-name buf*)))
        (normal-mode)
        (indent-region (point-min) (point-max)))
      (diff-no-select buf* (current-buffer)
                      nil 'no-async diff-buffer)
      (diff-no-select buf* (current-buffer)
                      `(,(format "--old-line-format=\"%s:%%dn: warning: Indent mismatch\n\""
                                 (buffer-name buf*))
                        "--new-line-format=\"\""
                        "--unchanged-line-format=\"\"")
                      'no-async diff-buffer-with-line))
    (cond
     ((eq 0 indent-lint-exit-code))
     ((eq 1 indent-lint-exit-code)
      (display-buffer diff-buffer))
     ((eq 2 indent-lint-exit-code)
      (display-buffer diff-buffer)
      (error "Diff error")))
    `(,diff-buffer ,diff-buffer-with-line)))

(defun indent-lint--get-stdin-buffer ()
  "Get stdin string until EOF and return its buffer."
  (let ((read-line (lambda () (read-string "")))
        (buf (get-buffer-create "*stdin*"))
        line)
    (with-current-buffer buf
      (ignore-errors
        (while (setq line (funcall read-line))
          (insert line "\n"))))
    buf))

(defun indent-lint-batch ()
  "Run `indent-lint' and output diff to standard output.
Use this only with --batch, it won't work interactively.

Extra argument; FILENAME is needed to guess `major-mode' to indent.

Status code:
  0 - No indentation errors and no output
  1 - Found indentation errors and diff output
  2 - Diff program exit with errors

Usage:
  - Import code from stdin and guess `major-mode' from header or footer.
      cat sample.el | \
        {EMACS} -Q -l indent-lint.el -f indent-lint-batch

  - Import code from stdin and guess `major-mode' from file extension.
      cat sample.el | \
        {EMACS} -Q -l indent-lint.el -f indent-lint-batch sample.el

  - Import code from file and guess `major-mode' from file extension.
    (Sending EOF is needed after Emacs run)
      {EMACS} -Q -l indent-lint.el -f indent-lint-batch sample.el"
  (unless noninteractive
    (error "`indent-lint-batch' can be used only with --batch"))
  (indent-lint-setup)
  (let ((inhibit-message t)
        (stdin-buf (indent-lint--get-stdin-buffer))
        (file-name (nth 0 command-line-args-left)))
    (with-current-buffer stdin-buf
      (when (and file-name (equal "" (buffer-string)))
        (insert-file-contents file-name))
      (rename-buffer (or file-name "*stdin*")))
    (pcase-let ((`(,diff-buffer ,diff-buffer-with-line) (indent-lint stdin-buf)))
      (when file-name
        (with-current-buffer diff-buffer
          (let ((inhibit-read-only t))
            (save-excursion
              (goto-char (point-min))
              (ignore-errors
                (while (search-forward "#<buffer  *temp*>")
                  (replace-match (format "%s" file-name))))))))
      (cond
       ((eq 0 indent-lint-exit-code))
       ((eq 1 indent-lint-exit-code)
        (princ (with-current-buffer diff-buffer-with-line (buffer-string)))
        (princ (with-current-buffer diff-buffer (buffer-string))))
       ((eq 2 indent-lint-exit-code)
        (princ (with-current-buffer diff-buffer-with-line (buffer-string)))
        (princ (with-current-buffer diff-buffer (buffer-string)))))
      (kill-emacs indent-lint-exit-code))))

;;;###autoload
(defun indent-lint-setup ()
  "Setup indent-lint."
  (interactive)
  (setq indent-lint-initialized t)
  (pcase-dolist (`(,sym . ,fn) indent-lint-advice-alist)
    (advice-add sym :around fn)))

;;;###autoload
(defun indent-lint-teardown ()
  "Setup indent-lint."
  (interactive)
  (setq indent-lint-initialized nil)
  (pcase-dolist (`(,sym . ,fn) indent-lint-advice-alist)
    (advice-remove sym fn)))

(provide 'indent-lint)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; indent-lint.el ends here
