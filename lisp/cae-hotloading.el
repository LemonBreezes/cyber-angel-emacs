;;; lisp/cae-hotloading.el -*- lexical-binding: t; -*-

;; Clean up duplicate idle timers since we are hot-reloading our config.
;; NOTE If the timers have a different idle time, the last one defined one will
;; be taken.
;; NOTE If this ever causes problems, I can always limit it to only applying to
;; functions from my config.
(defun cae-cleanup-duplicate-idle-timers ()
  (setq timer-idle-list
        (cl-remove-duplicates timer-idle-list
                              :test (lambda (x y)
                                      (and
                                       (equal (timer--function x)
                                              (timer--function y))
                                       (equal (timer--args x)
                                              (timer--args y)))))))

(run-with-idle-timer 3 t #'cae-cleanup-duplicate-idle-timers)

(defun cae-dir-locals-cache-lookup (file)
  (setq file (expand-file-name file))
  (let ((best nil))
    ;; Iterate over each cached entry in dir-locals-directory-cache.
    (dolist (entry dir-locals-directory-cache)
      ;; entry is typically ("DIRECTORY" CLASS MTIME ...)
      ;; We check if "DIRECTORY" is a prefix of FILE and pick the longest match.
      (when (and
             ;; Only check for classes with symbol names.
             (symbolp (cadr entry))
             (string-prefix-p (car entry) file
                              (memq system-type '(windows-nt cygwin ms-dos)))
             (or (null best)
                 (> (length (car entry)) (length (car best)))))
        (setq best entry)))
    best))

;; Reload all dir-local variables defined in my Emacs config when a file is
;; saved. I am abusing the fact that all of them are defined through classes
;; rather than `.dir-locals.el' files.
(defun cae-hotloading-reload-all-dir-locals ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (buffer-file-name)
                 (cae-dir-locals-cache-lookup (buffer-file-name)))
        (with-current-buffer buffer
          (hack-dir-local-variables-non-file-buffer))))))

(dir-locals-set-class-variables
 'doom
 '((nil
    . ((eval
        . (progn
            ;; Prevent an Elisp file from being saved if it contains an error.
            ;; This forces us to write our config so that it's idempotent, as it
            ;; will get reloaded on every save.
            (when (and (derived-mode-p 'emacs-lisp-mode)
                       (buffer-file-name)
                       (not (cl-member (file-name-nondirectory (buffer-file-name))
                                       `("init.el"
                                         ,dir-locals-file
                                         "custom.el"
                                         "packages.el")
                                       :test #'string=))
                       (not (string-match-p "/packages/"
                                            buffer-file-name))
                       (not (string-match-p "/trash/"
                                            buffer-file-name))
                       (not (string-match-p "/benchmarks/"
                                            buffer-file-name))
                       (not (string-match-p "/vanilla-emacs-configs/"
                                            buffer-file-name))
                       (not (string-match-p "/dir-local-files/"
                                            buffer-file-name))
                       (bound-and-true-p cae-config-finished-loading))
              (add-hook 'write-file-functions 'eval-buffer 1 t)
              (add-hook 'write-file-functions 'cae-hotloading-reload-all-dir-locals 2 t))

            (when (and (buffer-file-name)
                       (derived-mode-p 'emacs-lisp-mode)
                       (require 'apheleia nil t))
              (apheleia-mode +1))

            ;; Do not render `blamer' hints since we use `git-auto-commit-mode'.
            (setq-local blamer--block-render-p t)

            ;; Automatically update Eshell aliases.
            (when (and (buffer-file-name) (bound-and-true-p eshell-aliases-file)
                       (file-equal-p (buffer-file-name)
                                     (bound-and-true-p eshell-aliases-file))
                       (fboundp 'eshell-read-aliases-list))
              (add-hook 'after-save-hook #'eshell-read-aliases-list nil t))

            ;; Automatically commit saved files to Git and push them to the
            ;; remote.
            (when (and (buffer-file-name)
                       (bound-and-true-p doom-user-dir)
                       (not (file-in-directory-p
                             (buffer-file-name)
                             (concat doom-user-dir "secrets/")))
                       (require 'git-auto-commit-mode nil t))
              (setq-local gac-automatically-add-new-files-p nil)
              (git-auto-commit-mode 1))))))
   (magit-status-mode . ((magit-todos-exclude-globs . (".git/" "shared-local/"))))))

(dir-locals-set-directory-class doom-private-dir 'doom)
