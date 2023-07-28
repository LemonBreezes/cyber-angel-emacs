;;; git-email.el --- Work with git and email -*- lexical-binding: t; -*-

;; Copyright (C) 2021  all contributors <~yoctocell/git-email-deve@lists.sr.ht>

;; Author: Xinglu Chen <public@yoctocell.xyz>
;; URL: https://git.sr.ht/~yoctocell/git-email
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
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

;; This package integrates with git and email and offers two main functions
;;
;; * `git-email-send-email' sends an email based on a patch file generated by
;; 'git format-patch'.  It inserts the relevant headers and the diff into the
;; message buffer.
;;
;; * `git-email-format-patch' is a wrapper for 'git format-patch' and it
;; automatically composes an email for each generated patch, in the same way
;; as `git-email-send-email'.

;;; Code:

(require 'project)
(require 'message)
(require 'mailheader)
(require 'cl-lib)


;;;; Customization options

(defgroup git-email nil
  "Work with git and email."
  :group 'convenience)

(defcustom git-email-compose-email-function #'message-mail
  "The function used to compose patch mail."
  :type 'function
  :group 'git-email
  :package-version '(git-email . "0.1.0"))

(defcustom git-email-send-email-function #'message-send-and-exit
  "The function used to send mail."
  :type 'function
  :group 'git-email
  :package-version '(git-email . "0.1.0"))

(defcustom git-email-pre-compose-email-hook nil
  "A list of functions called before running `git-email--compose-email'."
  :type 'hook
  :group 'git-email
  :package-version '(git-email . "0.1.0"))

(defcustom git-email-post-compose-email-hook nil
  "A list of functions called after running `git-email--compose-email'."
  :type 'hook
  :group 'git-email
  :package-version '(git-email . "0.1.0"))

(defcustom git-email-headers
  '(subject from cc in-reply-to message-id references)
  "List of headers that can be updated via `git-email-rewrite-header'."
  :type '(symbol)
  :group 'git-email
  :package-version '(git-email . "0.2.0"))

(defcustom git-email-get-current-project-functions
  '(git-email--get-current-project)
  "Hooks to run to get the path to the current project.
The path should end with a trailing \"/\"."
  :type 'hook
  :group 'git-email
  :package-version '(git-email . "0.2.0"))

(defcustom git-email-get-files-functions
  '(git-email--dired-files
    git-email--ibuffer-files
    git-email--vc-dir-files)
  "An list of functions to for getting a list of patches to send."
  :type 'hook
  :group 'git-email
  :package-version '(git-email . "0.1.0"))

(defcustom git-email-get-to-address-functions
  '(git-email--get-to-address)
  "Functions to run to get the \"to\" address of a message."
  :type 'hook
  :group 'git-email
  :package-version '(git-email . "0.1.0"))

(defcustom git-email-format-patch-default-args ""
  "Default arguments to give to `git format-patch'."
  :type 'string
  :group 'git-email
  :package-version '(git-email . "0.1.0"))

(defcustom git-email-format-patch-extra-args
  '("--cover-letter"
    "--rfc"
    "--signoff"
    "--range-diff="
    "--interdiff="
    "--base="
    "--no-base"
    "--subject-prefix="
    "--thread="
    "--output-directory="
    "--to="
    "--reroll-count="
    "--in-reply-to=")
  "List of arguments to display in `git-email-format-patch'."
  :type '(string)
  :group 'git-email
  :package-version '(git-email . "0.2.0"))

(defcustom git-email-revision-limit 100
  "How many revisions to show when in `git-email-format-patch'."
  :type 'int
  :group 'git-email
  :package-version '(git-email . "0.1.0"))

(defcustom git-email-revision-command
  "git log --no-color --pretty='format:%h %d %s' --abbrev-commit -n "
  "Command to run to get a list of revisions."
  :type 'string
  :group 'git-email
  :package-version '(git-email . "0.1.0"))

(defcustom git-email-revision-parser #'git-email--parse-revision
  "Function for parsing the output of ‘git-email-revision-command’."
  :type 'hook
  :group 'git-email
  :package-version '(git-email . "0.2.0"))

(defcustom git-email-get-revision-functions '(git-email--log-get-revision)
  "List of functions to get the base commit for `git format-patch'.
If none of the functions return non-nil value,
`git-email--minibuffer-get-revision' will be used as a fallback."
  :type 'hook
  :group 'git-email
  :package-version '(git-email . "0.2.0"))

(defcustom git-email-generate-message-buffer
  #'git-email-generate-message-buffer-name
  "Function for generating the name of a message buffer.
The function must take three arguments: the type, the \"To\"
address, and the group name.  See `message-generate-new-buffers'
for more information.

By default it will be \"*git-email unsent patch to *** TO ADDRESS
HERE *** *\"."
  :type '(choice (const nil)
	  (sexp :tag "unique" :format "unique\n" :value unique
		:match (lambda (widget value) (memq value '(unique t))))
	  (const unsent)
	  (const standard)
	  (function :format "\n    %{%t%}: %v"))
  :group 'git-email
  :package-version '(git-email . "0.3.0"))

(defcustom git-email-buffer-p-function
  #'git-email-buffer-p
  "Function to test if buffer has an unsent patch.
The function must take one argument: the buffer's name."
  :type 'function
  :group 'git-email
  :package-version '(git-email . "0.3.0"))

;; Remove Compiler warnings
(declare-function dired-get-filename "dired.el")
(declare-function dired-map-over-marks "dired.el")
(declare-function ibuffer-get-marked-buffers "ibuffer.el")
(declare-function vc-dir-marked-files "vc-dir.el")
(declare-function vc-dir-current-file "vc-dir.el")
(declare-function log-view-current-entry "log-view.el")


;;;; Get files to send

(defun git-email--check-file (file)
  "Check if FILE is a patch."
  (if (and (file-readable-p file)
           (or (string-match-p "\\.patch$" file)
               (string-match-p "\\.diff$" file)))
      file
    (user-error "Not a valid patch!")))

(defun git-email--dired-files ()
  "Return list of filenames for marked files in `dired'.
If no marks are found, return the filename at point."
  (when (eq major-mode 'dired-mode)
    (delq nil
          (mapcar
           (lambda (f) (if (file-directory-p f) nil f))
           (dired-map-over-marks (dired-get-filename) nil)))))

(defun git-email--vc-dir-files ()
  "Return list of filenames for marked files in `vc-dir'.
If no marks are found, return the filename at point."
  (when (eq major-mode 'vc-dir)
    (let* ((marked-files (nreverse (vc-dir-marked-files)))
           (files (if marked-files
                      marked-files
                    (list (vc-dir-current-file)))))
      files)))

(defun git-email--ibuffer-files ()
  "Return list of filenames for marked files in `ibuffer'."
  (when (eq major-mode 'ibuffer-mode)
    (let ((marked-files (nreverse
                         (mapcar (lambda (b) (buffer-file-name b))
                                 (ibuffer-get-marked-buffers)))))
      marked-files)))

(defun git-email--minibuffer-file ()
  "Prompt for a file to send as a patch."
  (list (car (find-file-read-args "Find patch: "
                                  (confirm-nonexistent-file-or-buffer)))))

(defun git-email--get-files ()
  "Return list of filenames for marked files in `vc-dir'.
If no marks are found, return the filename at point."
  (let ((files (or (run-hook-with-args-until-success
                    'git-email-get-files-functions)
                   (git-email--minibuffer-file))))
    (when (mapcar #'git-email--check-file files)
      files)))

(declare-function projectile-project-root "projectile")

(defun git-email--project-current ()
  "Return directory from `project-current' based on Emacs version."
  (if (>= emacs-major-version 29)
      (project-root (project-current))
    (cdr (project-current))))

(defun git-email--get-current-project ()
  "Return the path of the current project.
Falls back to `default-directory'."
  (or (and (bound-and-true-p projectile-known-projects)
           (projectile-project-root))
      (and (bound-and-true-p project-list-file)
           (git-email--project-current))
      (vc-root-dir)
      default-directory))


;;;; Get contents from patch

(defun git-email--extract-diff (patch-file)
  "Extract the diff from PATCH-FILE."
  (with-temp-buffer
    (insert-file-contents patch-file)
    (goto-char (point-min))
    (buffer-substring
     (- (re-search-forward "\n\n") 1)
     (point-max))))

;; See https://emacs.stackexchange.com/a/5408
(defun git-email--fontify-diff (text)
  "Fontify TEXT as diffs in `message-mode'."
  (with-temp-buffer
    (erase-buffer)
    (insert text)
    (delay-mode-hooks (diff-mode))
    (font-lock-default-function #'diff-mode)
    (font-lock-default-fontify-region (point-min)
                                      (point-max)
                                      nil)
    (buffer-string)))

(defun git-email--fontify-using-faces (text)
  "Fontify TEXT using faces."
  (let ((pos 0)
        next)
    (while (setq next (next-single-property-change pos 'face text))
      (put-text-property pos next 'font-lock-face
                         (get-text-property pos 'face text) text)
      (setq pos next))
    (add-text-properties 0  (length text) '(fontified t) text)
    text))

(defun git-email--remove-subject (header)
  "Remove HEADER if it is the subject."
  (not (string-equal (car header) "Subject")))

(defun git-email--extract-headers (patch-file)
  "Extract all the headers from a PATCH-FILE."
  (with-temp-buffer
    (insert-file-contents patch-file)
    ;; Find the first headers, and jump to the start of it's line
    (re-search-forward "\\([A-Za-z0-9-]+\\): \\(.*\\)")
    (beginning-of-line)
    (let ((headers (mail-header-extract-no-properties)))
      ;; Headers are returned as downcased symbols, but all
      ;; compose-mail functions expect headers to be capitialized
      ;; strings.
      (dolist (h headers headers)
        (when (symbolp (car h))
          (setcar h (capitalize (symbol-name (car h)))))))))

(defun git-email--get-to-address ()
  "Get the \"to\" address of the message.

This runs \“git config --list\" in the current directory
so might not always work."
  (string-trim (shell-command-to-string  "git config --get sendemail.to")))

(defun git-email--compose-email (patch-file)
  "Given a PATCH-FILE, compose an email.
Extracts the relevant headers and the diff from the PATCH-FILE and inserts
them into the message buffer."
  (let* ((headers (git-email--extract-headers patch-file))
         ;; Remove empty headers.
         (used-headers (seq-filter
                        (lambda (header)
                          (not (string-equal (cdr header) "")))
                        headers))
         (to-address (seq-position used-headers
                                   'to
                                   (lambda (a b)
                                     (equal (car a) b))))
         ;; Don't insert 'to' address if the patch already contains one.
         (to-address (or (cdr (assoc "To" used-headers 'string-equal))
                         (run-hook-with-args-until-success
                          'git-email-get-to-address-functions)))
         ;; Don't insert 'to' address if the patch already contains one.
         (to (if (string-equal to-address "")
                 "*** TO ADDRESS HERE ***"
               to-address))
         (diff (git-email--extract-diff patch-file)))
    (funcall git-email-compose-email-function to
             (cdr (assoc "Subject" used-headers 'string-equal))
             (seq-filter #'git-email--remove-subject used-headers))
    ;; Insert diff at the beginning of the body
    (goto-char (point-min))
    (let ((_ (or (re-search-forward
                  "^<#part \\(encrypt\\|sign\\)=.*mime>$"
                  nil t)
                 (re-search-forward (regexp-quote mail-header-separator) nil t))))
      (save-excursion
        (insert (git-email--fontify-using-faces
                 (git-email--fontify-diff diff)))))
    ;; Jump to subject or 'to' address if they are emtpy
    (when (or (re-search-backward "\\*\\*\\* TO ADDRESS HERE \\*\\*\\*" nil t)
              (re-search-backward "\\*\\*\\* SUBJECT HERE \\*\\*\\*" nil t))
      (delete-region (point) (point-at-eol)))))


;;;; Format patches

(defvar git-email--revision-history '()
  "Minibuffer history of `git-email--minibuffer-get-revision'.")

(defun git-email--minibuffer-get-revision ()
  "Let the user choose a git revision from the minibuffer."
  (interactive)
  (let* ((default-directory (run-hook-with-args-until-success
                             'git-email-get-current-project-functions))
         ;; Last element is an empty string
         (revs (split-string
                (shell-command-to-string
                 (concat
                  git-email-revision-command
                  (int-to-string git-email-revision-limit)))
                "\n"))
         ;; Sort the candidates correctly.
         ;; See https://emacs.stackexchange.com/a/41808.
         (sorted-revs
          (lambda (string pred action)
            (if (eq action 'metadata)
                '(metadata (display-sort-function . identity)
                  (cycle-sort-function . identity))
              (complete-with-action
               action revs string pred)))))
    (git-email--parse-revision (completing-read "Revision: " sorted-revs
                                       nil nil nil 'git-email--revision-history))))

(defun git-email--parse-revision (rev)
  "Return only the revision hash from REV.
This parses the output of ‘git-email--minibuffer-get-revision’."
  (substring rev 0 7))

(defvar git-email--format-patch-args-history '()
  "Minibuffer history of `git-email-format-patch' ARGS.")

(defun git-email--log-get-revision ()
  "Get the revision at point in `log-view-mode'."
  (when (eq major-mode 'vc-git-log-view-mode)
    (cadr (log-view-current-entry (point) t))))

;;;###autoload
(defun git-email-format-patch (args range keep)
  "Format and send patch(es) using 'git format-patch'.

ARGS are additional arguments to give to 'git format-patch'.  By
default, the arguments in `git-email-format-patch-default-args'
will be used.

Patches for the commits in RANGE will be created.

With prefix argument KEEP, keep the generated patches.  The
default behavior is to delete them after sending the message."
  (interactive
   (list (apply #'concat (mapcar
                          (lambda (a) (concat a " "))
                          (completing-read-multiple
                           "Args: " git-email-format-patch-extra-args
                           nil nil git-email-format-patch-default-args
                           'git-email--format-patch-args-history)))
         (or (run-hook-with-args-until-success
              'git-email-get-revision-functions)
             (git-email--minibuffer-get-revision))
         current-prefix-arg))
  ;; List of patches generated, the last element is an empty string
  ;; so remove it.  Reverse the list so we edit the cover letter first.
  (let ((files (nreverse (butlast
                          (split-string
                           (shell-command-to-string
                            (format "git format-patch %s %s"
                                    args range))
                           "\n")))))
    (git-email--send-files files)
    (unless keep
      (mapc #'delete-file files))))


;;;; Misc

(defun git-email-generate-message-buffer-name (_type address _group)
  "Generate a buffer name in the form of:

\"* git-email unsent patch to *** TO ADDRESS HERE *** *\""
  (generate-new-buffer-name
   (concat "*git-email unsent patch to " address " *" )))

(defun git-email-buffer-p (name)
  "Test if NAME buffer has an unsent patch.
The heuristic is to check if the buffer's name includes the
string 'git-email-unsent-patch'."
  (if (string-match "git-email-unsent-patch" name)
      t
    nil))


;;;; Operate on emails

(defun git-email-message-buffer-greaterp (old new)
  "Compare the number in the buffer name of OLD with NEW."
  (cl-flet ((regexp (name)
                    (string-to-number
                     (replace-regexp-in-string ".*<\\([0-9]+\\)>"
                                               "\\1"
                                               name))))
    (let ((old (regexp old))
          (new (regexp new)))
      (> old new))))

(defun git-email-send-all ()
  "Send all unsent patches."
  (interactive)
  ;; Sort the buffers so that [PATCH 0/N] comes first, this prevents
  ;; the ordering from getting messed up.
  (let* ((message-buffers (seq-filter #'git-email-buffer-p
                                      (message-buffers)) )
         (sorted-buffers (sort message-buffers #'git-email-message-buffer-greaterp)))
    (mapc (lambda (b)
            (switch-to-buffer b)
            (funcall git-email-send-email-function))
          sorted-buffers)))

(defun git-email--rewrite-header-in-buffer (buffer header value append)
  "Rewrite BUFFER's HEADER with VALUE.
If APPEND is non-nil, append the VALUE to the existing one
instead of overwriting it."
  (switch-to-buffer buffer)
  (save-excursion
    (goto-char (point-min))
    (let* ((case-fold-search t))
      (if (re-search-forward (concat "^" (capitalize header) ":") nil t)
          (progn
            (unless append
              (delete-region (point) (point-at-eol)))
            (goto-char (point-at-eol))
            (insert " " value))
        (progn
          (re-search-forward "^Subject: .*$")
          (insert "\n" (concat (capitalize header) ": " value)))))))

(defun git-email--send-files (files)
  "Send email for each file in FILES."
  (dolist (file files)
    (run-hooks 'git-email-pre-compose-email-hook)
    (let ((message-generate-new-buffers
           git-email-generate-message-buffer))
      (git-email--compose-email file))
    (run-hooks 'git-email-post-compose-email-hook)))

(defvar git-email--rewrite-header-history '()
  "Minibuffer history of `git-email-rewrite-header'.")

;;;###autoload
(defun git-email-rewrite-header (header value &optional append)
  "Re-write value of HEADER to VALUE.
If HEADER doesn't exist yet, just set it to VALUE.
 
With prefix argument APPEND, append the VALUE to HEADER instead
of overwriting it.

This is mainly useful if you forgot to Cc someone when using
`git-email-format-patch', or if you are waiting for Debbugs to
give you an address to send your patches to."
  (interactive
   (list (completing-read "Header to re-write: "
                          git-email-headers
                          nil nil nil 'git-email--rewrite-header-history)
         (read-from-minibuffer "Set header to: ")
         current-prefix-arg))
  (let ((buffers (message-buffers))
        (start-buffer (current-buffer)))
    (save-excursion
      (mapc (lambda (buffer)
              (git-email--rewrite-header-in-buffer
               buffer header value append))
            buffers))
    (switch-to-buffer start-buffer)))

;;;###autoload
(defun git-email-send-email (files)
  "Send FILES as patch(es) to someone using your MUA."
  (interactive (list (git-email--get-files)))
  (git-email--send-files files))

(provide 'git-email)
;;; git-email.el ends here
