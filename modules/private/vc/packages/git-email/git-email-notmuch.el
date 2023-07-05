;;; git-email-notmuch.el --- Notmuch integration for git-email -*- lexical-binding: t; -*-

;; Copyright (C) 2021  all contributors <~yoctocell/git-email-deve@lists.sr.ht>

;; Author: Xinglu Chen <public@yoctocell.xyz>
;; URL: https://git.sr.ht/~yoctocell/git-email
;; Version: 0.2.0
;; Package-Requires: ((emacs "27") (notmuch "0.30.0"))
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
;; better with Notmuch.

;;; Code:

(require 'git-email)
(require 'notmuch-mua)

;;;###autoload
(define-minor-mode git-email-notmuch-mode
  "Notmuch integration for git-email.
If called interactively, toggle the mode.  A positive prefix ARG
enables the mode, any other prefix ARG disables it.  If called
from Lisp, enable the mode if ARG is omitted or nil."
  :group 'git-email
  :global t
  :init-value nil
  (if git-email-notmuch-mode
      (progn
        (setq git-email-compose-email-function 'notmuch-mua-mail)
        (setq git-email-send-email-function 'notmuch-mua-send-and-exit))
    (setq git-email-compose-email-function 'message-mail)
    (setq git-email-send-email-function 'message-send-and-exit)))

(provide 'git-email-notmuch)
;;; git-email-notmuch.el ends here
