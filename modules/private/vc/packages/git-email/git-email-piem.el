;;; git-email-piem.el --- Piem integration for git-email -*- lexical-binding: t; -*-

;; Copyright (C) 2021  all contributors <~yoctocell/git-email-deve@lists.sr.ht>

;; Author: Xinglu Chen <public@yoctocell.xyz>
;; URL: https://git.sr.ht/~yoctocell/git-email
;; Version: 0.2.0
;; Package-Requires: ((emacs "27") (piem "0.1.0"))
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

;; This library sets some variables for git-email to make it work
;; better with Piem.

;;; Code:

(require 'git-email)
(require 'piem)

;; TODO: Use `piem-merged-inboxes' instead of `piem-inboxes'
;; <https://inbox.kyleam.com/piem/20210610185943.14155-5-kyle@kyleam.com/T/#u>
(defun git-email-piem-get-to-address ()
  "Return the \"to\" address for the current project.
This will read the `piem-inboxes' variable."
  (let ((inbox (git-email-piem-inbox-by-coderepo)))
    (piem-inbox-get :address inbox)))

(defun git-email-piem-inbox-by-coderepo ()
  "Return the inbox based on the current project."
  (catch 'hit
    (dolist (inbox piem-inboxes)
      (let* ((info (cdr inbox))
             (dir (run-hook-with-args-until-success
                   'git-email-get-current-project-functions))
             (repo (plist-get info :coderepo)))
        (when (string-equal repo dir)
          (throw 'hit (car inbox)))))))

;;;###autoload
(define-minor-mode git-email-piem-mode
  "Piem integration for git-email.
If called interactively, toggle the mode.  A positive prefix ARG
enables the mode, any other prefix ARG disables it.  If called
from Lisp, enable the mode if ARG is omitted or nil.

This will make git-email read the `piem-inboxes' variable to
determine the \"to\" address."
  :group 'git-email
  :global t
  :init-value nil
  (if git-email-piem-mode
      (add-to-list 'git-email-get-to-address-functions
                   #'git-email-piem-get-to-address)
    (setq git-email-get-to-address-functions
          (delete 'git-email-piem--get-to-address
                  git-email-get-to-address-functions))))


(provide 'git-email-piem)
;;; git-email-piem.el ends here
