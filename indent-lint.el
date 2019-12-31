;;; indent-lint.el --- Async indentation checker  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: tools
;; Package-Requires: ((emacs "25.1") (async-await "1.0"))
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

;; Indentation checker


;;; Code:

(require 'seq)
(require 'async-await)

(defgroup indent-lint nil
  "Asynchronous indentation checker"
  :prefix "indent-lint-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/conao3/indent-lint.el"))

(defcustom indent-lint-before-indent-fn #'ignore
  "The function to eval before indent.
Function will be called with 2 variables; `(,raw-buffer ,indent-buffer)."
  :group 'indent-lint
  :type 'function)

(defcustom indent-lint-verbose nil
  "If non-nil, output diff verbose."
  :group 'indent-lint
  :type 'boolean)

(defconst indent-lint-directory (eval-and-compile
                                  (file-name-directory
                                   (or (bound-and-true-p
                                        byte-compile-current-file)
                                       load-file-name
                                       (buffer-file-name))))
  "Path to indent-lint root.")

(defun indent-lint--output-debug-info (err)
  "Output debug info form ERR."
  (let ((file (locate-user-emacs-file "flycheck-indent.debug")))
    (with-temp-file file
      (erase-buffer)
      (insert (format "Error at %s\n" (format-time-string "%Y-%m-%d %H:%M")))
      (insert (format "Error: %s\n" (pp-to-string err)))
      (insert "\n")
      (insert (with-output-to-string
                (backtrace))))
    (message (format "Indent-lint exit with errors. See %s" file))))

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
  "Run `indent-lint--sync' and output diff to standard output.
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
  (condition-case err
      (let ((inhibit-message t)
            (stdin-buf (indent-lint--get-stdin-buffer))
            (file-name (nth 0 command-line-args-left)))
        (with-current-buffer stdin-buf
          (when (and file-name (equal "" (buffer-string)))
            (insert-file-contents file-name))
          (rename-buffer (or file-name "*stdin*")))
        (let ((diff-buffer (indent-lint stdin-buf)))
          (princ (with-current-buffer diff-buffer (buffer-string)))
          (kill-emacs 0)))
    (error
     (indent-lint--output-debug-info err))))

(defun indent-lint--promise-indent (buf src-file dest-file)
  "Renturn promise to save BUF to SRC-FILE and save DEST-FILE indented."
  (with-temp-file src-file
    (insert (with-current-buffer buf (buffer-string))))
  (promise-then
   (promise:async-start
    `(lambda ()
       (progn
         (setq user-emacs-directory ,user-emacs-directory)
         (setq package-user-dir ,package-user-dir)
         (package-initialize)
         (with-temp-file ,dest-file
           (insert-file-contents ,src-file)
           (funcall #',(with-current-buffer buf major-mode))
           (indent-region (point-min) (point-max))))))
   (lambda (res)
     (promise-resolve res))
   (lambda (reason)
     (promise-reject `(fail-indent ,reason)))))

(defun indent-lint--promise-diff (buf src-file dest-file)
  "Return promise to diff SRC-FILE and DEST-FILE named BUF."
  (let ((exitcode (lambda (str)
                    (when (stringp str)
                      (let ((reg "exited abnormally with code \\([[:digit:]]*\\)\n"))
                        (if (string-match reg str)
                            (string-to-number (match-string 1 str))
                          nil))))))
    (promise-then
     (promise:make-process
      shell-file-name
      shell-command-switch
      (mapconcat
       #'shell-quote-argument
       `("diff"
         "--old-line-format"
         ,(if indent-lint-verbose
              (format "%s:%%dn: warning: Indent mismatch\n-%%L" (buffer-name buf))
            (format "%s:%%dn: warning: Indent mismatch\n" (buffer-name buf)))
         "--new-line-format" ,(if indent-lint-verbose "+%L" "")
         "--unchanged-line-format" ""
         ,src-file
         ,dest-file)
       " "))
     (lambda (res)
       (seq-let (stdin stdout) res
         (let ((code 0)
               (output (concat stdin stdout)))
           (promise-resolve `(,code ,output)))))
     (lambda (res)
       (seq-let (msg stdin stdout) res
         (let ((code (funcall exitcode msg))
               (output (concat stdin stdout)))
           (cond
            ((eq code 1)
             (promise-resolve `(,code ,output)))
            ((eq code 2)
             (promise-reject `(fail-diff ,output)))
            (t
             (promise-reject `(fail-diff-unknown ,output))))))))))

;;;###autoload
(async-defun indent-lint (&optional buf)
  "Indent BUF in clean Emacs and lint async."
  (interactive)
  (let ((buf* (get-buffer (or buf (current-buffer))))
        (src-file   (make-temp-file "emacs-indent-lint"))
        (dest-file  (make-temp-file "emacs-indent-lint")))
    (condition-case err
        (let* ((res (await (indent-lint--promise-indent buf* src-file dest-file)))
               (res (await (indent-lint--promise-diff buf* src-file dest-file))))
          (ignore-errors
            (delete-file src-file)
            (delete-file dest-file))
          (with-current-buffer (generate-new-buffer "*indent-lint*")
            (seq-let (code output) res
              (insert output)
              (insert
               (format "\nDiff finished%s.  %s\n"
                       (cond ((equal 0 code) " (no differences)")
                             ((equal 1 code) " (has differences)")
                             ((equal 2 code) " (diff error)")
                             (t (format "(unknown exit code: %d)" code)))
                       (current-time-string)))
              (special-mode)
              (diff-mode)
              (display-buffer (current-buffer))
              (current-buffer))))
      (error
       (pcase err
         (`(error (fail-indent ,reason))
          (warn "Fail indent
  buffer: %s\n  major-mode: %s\n  src-file: %s\n  dest-file: %s\n  reason: %s"
                (prin1-to-string buf*)
                (with-current-buffer buf* major-mode)
                src-file dest-file
                (prin1-to-string reason)))

         (`(error (fail-diff ,reason))
          (warn "Fail diff.
  buffer: %s\n  src-file: %s\n  dest-file: %s\n  reason: %s"
                (prin1-to-string buf*)
                src-file dest-file
                (prin1-to-string reason)))

         (`(error (fail-diff-unknown ,reason))
          (warn "Fail diff unknown.
  buffer: %s\n  src-file: %s\n  dest-file: %s\n  reason: %s"
                (prin1-to-string buf*)
                src-file dest-file
                (prin1-to-string reason))))))))

(provide 'indent-lint)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; indent-lint.el ends here
