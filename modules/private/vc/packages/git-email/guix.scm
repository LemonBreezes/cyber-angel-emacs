;;; Guix package definition for git-email
;;;
;;; Copyright (C) 2021  all contributors <~yoctocell/git-email-devel@lists.sr.ht>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(use-modules (guix packages)
             (guix build utils)
             (guix build-system emacs)
             (guix gexp)
             (guix profiles)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (gnu packages version-control)
             (gnu packages mail)
             (gnu packages texinfo)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (gnu packages base)
             (ice-9 popen)
             (ice-9 rdelim))

;;; Commentary:
;;;
;;; This file contains the package definitinon for git-email.  Run
;;; `guix environment --load=guix.scm' to create a development
;;; environment.
;;;
;;; Code:

(define* (git-output source-dir #:rest args)
  "Execute 'git ARGS ...' command and return its output without trailing
newspace."
  (with-directory-excursion source-dir
    (let* ((port   (apply open-pipe* OPEN_READ "git" args))
           (output (read-string port)))
      (close-port port)
      (string-trim-right output #\newline))))

(define (current-commit source-dir)
  (git-output source-dir "rev-parse" "HEAD"))

(define-public git-email-dev
  (let* ((source-dir (dirname (current-filename)))
         (commit (current-commit source-dir))
         (revision "0"))
    (package
      (name "git-email-dev")
      (version (git-version "0.2.0" revision commit))
      (source (local-file source-dir
                          #:recursive? #t
                          #:select? (git-predicate source-dir)))
      (build-system emacs-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           ;; piem is not yet packaged in Guix.
           (add-after 'unpack 'remove-piem
             (lambda _
               (delete-file "git-email-piem.el")))
           (add-before 'install 'makeinfo
             (lambda _
               (invoke "makeinfo" "doc/git-email.texi"))))))
      (native-inputs
       `(("texinfo" ,texinfo)))
      (propagated-inputs
       `(("emacs" ,emacs)
         ("emacs-magit" ,emacs-magit)
         ("notmuch" ,notmuch)
         ("emacs-mu4e" ,emacs-mu4e)))
      (license license:gpl3+)
      (home-page "https://sr.ht/~yoctocell/git-email")
      (synopsis "Format and send Git patches in Emacs")
      (description "This package provides utilities for formatting and
sending Git patches via Email, without leaving Emacs."))))

git-email-dev
