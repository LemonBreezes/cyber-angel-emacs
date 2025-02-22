;;; lisp/cae-hotloading.el -*- lexical-binding: t; -*-

(defun cae-cleanup-duplicate-idle-timers ()
  (setq timer-idle-list
        (cl-remove-duplicates
         timer-idle-list
         :test (lambda (x y)
                 (and
                  (equal (timer--function x)
                         (timer--function y))
                  (or (equal (timer--args x)
                             (timer--args y))
                      (and (symbolp (timer--function x))
                           (string-prefix-p "cae-"
                                            (symbol-name (timer--function x))))))))))
(defun cae-cleanup-duplicate-timers ()
  (setq timer-list
        (cl-remove-duplicates
         timer-list
         :test (lambda (x y)
                 (and
                  (equal (timer--function x)
                         (timer--function y))
                  (symbolp (timer--function x))
                  (string-prefix-p "cae-"
                                   (symbol-name (timer--function x))))))))

(run-with-idle-timer 3 t #'cae-cleanup-duplicate-idle-timers)
(run-with-idle-timer 3 t #'cae-cleanup-duplicate-timers)

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
                                         "doctor.el"
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
              (add-hook 'write-file-functions 'eval-buffer 1 t))

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
                       (eq system-type 'gnu/linux)
                       (bound-and-true-p doom-user-dir)
                       (not (file-in-directory-p
                             (buffer-file-name)
                             (concat doom-user-dir "secrets/")))
                       (require 'git-auto-commit-mode nil t))
              (setq-local gac-automatically-add-new-files-p nil)
              (git-auto-commit-mode 1))))))
   (magit-status-mode . ((magit-todos-exclude-globs . (".git/" "shared-local/"))))))

(dir-locals-set-directory-class doom-private-dir 'doom)

;;Local Variables:
;;eval: (cae-hotloading-reload-dir-locals-for-class 'doom)
;;End:
