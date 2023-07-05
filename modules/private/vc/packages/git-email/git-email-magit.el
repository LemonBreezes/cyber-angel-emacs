;;; git-email-magit.el --- Magit integration for git-email -*- lexical-binding: t; -*-

;; Copyright (C) 2021  all contributors <~yoctocell/git-email-deve@lists.sr.ht>

;; Author: Xinglu Chen <public@yoctocell.xyz>
;; URL: https://git.sr.ht/~yoctocell/git-email
;; Version: 0.2.0
;; Package-Requires: ((emacs "27") (magit "3.0.0") (transient "0.2.0"))
;; Keywords: git mail
;; License: GNU General Public License >= 3

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library offers transient commands in the magit-status buffer
;; for interacting with git-email.

;;; Code:

(require 'git-email)
(require 'transient)
(require 'magit-process)
(require 'magit-patch)

(defun git-email-magit-patch-send (range args files)
  "Send a set of patches via email."
  ;; This is largely copied from magit-patch's `magit-patch-create'
  ;; function.
  (interactive
   (if (not (eq transient-current-command 'magit-patch-create))
       (list nil nil nil)
     (cons (if-let ((revs (magit-region-values 'commit)))
               (if (length= revs 1)
                   (list "-1" (car revs))
                 (concat (car (last revs)) "^.." (car revs)))
             (let ((range (magit-read-range-or-commit
                           "Format range or commit")))
               (if (string-search ".." range)
                   range
                 (format "%s~..%s" range range))))
           (let ((args (transient-args 'magit-patch-create)))
             (list (-filter #'stringp args)
                   (cdr (assoc "--" args)))))))
  (let ((files (nreverse
                (split-string
                 (magit--with-temp-process-buffer
                   (let* ((status (magit-process-git t "format-patch" range args "--" files))
                          (output (buffer-string)))
                     output))))))
    (git-email--send-files files)
    (mapc #'delete-file files)))


(transient-append-suffix 'magit-patch-create "c"
  '(1 "s" "Send patch" git-email-magit-patch-send))

(provide 'git-email-magit)
;;; git-email-magit.el ends here
