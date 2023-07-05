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
(require 'magit-patch)
(require 'magit-log)

(defun git-email--escape-string (str)
  "Escape STR if it has spaces in it."
  (if (string-match-p "\s" str)
      (format "\"%s\"" str)
    str))

;;;###autoload
(defun git-email-magit-patch-send (args &optional commit)
  (interactive
   (let ((args (transient-args 'magit-patch-create)))
     (list (mapconcat #'git-email--escape-string
                      (seq-filter #'stringp args)
                      " "))))
  ;; For some reason, `git-email-format-patch' gets called before
  ;; `magit-log-select' has retunred anything, leading to an error.
  (if commit
      (git-email-format-patch args commit nil)
    (magit-log-select (lambda (commit)
                        (git-email-magit-patch-send args commit)))))

(transient-append-suffix 'magit-patch-create "c"
  '(1 "s" "Send patch" git-email-magit-patch-send))

(provide 'git-email-magit)
;;; git-email-magit.el ends here
