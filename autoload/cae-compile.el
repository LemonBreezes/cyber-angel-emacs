;;; autoload/cae-compile.el -*- lexical-binding: t; -*-

(defvar cae-compile-interesting-file-name-regexp
  "[a-zA-Z0-9-_+]+")

(defvar cae-compile--exit-code nil)
(defun cae-compile-store-exit-code-a (&optional exit-code _)
  (setq cae-compile--exit-code (or exit-code 0)))
(advice-add #'kill-emacs :before #'cae-compile-store-exit-code-a)

(defvar cae-compile-files-to-ignore
  `(,(expand-file-name "ui/doom-dashboard/config.el" doom-modules-dir)
    ,(expand-file-name "lang/org/autoload/org.el" doom-modules-dir)
    ,(expand-file-name "tools/debugger/autoload/debugger.el" doom-modules-dir)
    ,(expand-file-name "lang/cc/autoload.el" doom-modules-dir)
    ,(expand-file-name "config/default/autoload/text.el" doom-modules-dir)))

(defvar cae-compile-native-comp-speed 3)

(defun cae-compile--compile-pdf-tools ()
  (unless (ignore-errors (and (require 'pdf-tools nil t)
                              (pdf-info-check-epdfinfo))
                         t)
    (advice-add #'fboundp :around
                (defun tmp/fboundp-a (oldfun function)
                  (unless (eq function 'make-process)
                    (funcall oldfun function))))
    (let ((compilation-filter-hook
           (remove 'comint-truncate-buffer compilation-filter-hook)))
      (unwind-protect
          (pdf-tools-build-server (or (and (stringp pdf-info-epdfinfo-program)
                                           (file-name-directory
                                            pdf-info-epdfinfo-program))
                                      pdf-tools-directory)
                                  ;; Use system versions of dependencies.
                                  t)
        (advice-remove #'fboundp #'tmp/fboundp-a)))))

(defun cae-compile-file-not-in-unused-module-p (&optional file-name)
  (not (when-let* ((file-name (or file-name (buffer-file-name)))
                   (file-path (or (and (file-directory-p file-name)
                                       (expand-file-name file-name))
                                  (file-name-directory file-name)))
                   (module-dir (cl-find-if (lambda (x) (string-prefix-p x file-path))
                                           doom-modules-dirs))
                   (module (doom-module-from-path file-path)))
         (not (doom-module-p (car module) (cdr module))))))

;;;###autoload
(cl-defun cae-compile-this-elisp-file ()
  (when (not (bound-and-true-p cae-config-finished-loading))
    (message "Config not finished loading")
    (cl-return-from cae-compile-this-elisp-file))
  (unless (or no-byte-compile
              (not (stringp (buffer-file-name)))
              (file-in-directory-p (buffer-file-name) doom-local-dir)
              (file-in-directory-p (buffer-file-name) (or (bound-and-true-p cae-multi-local-dir)
                                                          doom-local-dir)) ; dummy value
              (not (string-match-p cae-compile-interesting-file-name-regexp
                                   (file-name-base (buffer-file-name))))
              (not (string-prefix-p "flycheck_" (buffer-file-name))))
    (byte-compile-file (buffer-file-name))
    (let ((native-comp-speed cae-compile-native-comp-speed))
      (emacs-lisp-native-compile-and-load))))

(defun cae-compile-list-files-to-compile (&optional arg)
  (let (result)
    (dolist
        (s (nconc
            ;; Compiling `lisp/lib' creates some errors and these functions
            ;; are not that important to have compiled anyways.
            (directory-files-recursively doom-core-dir
                                         "[a-za-z0-9]+\\.el$"
                                         nil
                                         #'ignore)
            (directory-files-recursively doom-modules-dir
                                         "[a-za-z0-9]+\\.el$"
                                         nil
                                         #'cae-compile-file-not-in-unused-module-p)
            (directory-files-recursively
             doom-user-dir
             "[a-zA-Z0-9]+\\.el$"
             nil
             (lambda (s)
               (not
                (cl-member s '("eshell" "packages" "snippets" ".local" ".git"
                               "shared-local" "media")
                           :test (lambda (x y)
                                   (string= (file-name-nondirectory x)
                                            y)))))))
           result)
      (unless
          (or (string= (file-name-nondirectory s) "packages.el")
              (string= (file-name-nondirectory s) "doctor.el")
              (string= (file-name-nondirectory s) dir-locals-file)
              (string-prefix-p "flycheck_" (file-name-nondirectory s))
              (cl-member s cae-compile-files-to-ignore :test #'string=)
              (and cae-compile--exit-code
                   (not (eq cae-compile--exit-code 0))
                   (not (file-exists-p (concat s "c"))))
              (eq this-command 'kill-emacs)
              (and (file-newer-than-file-p (concat s "c") s)
                   (not arg)))
        (push s result)))))

;;;###autoload
(cl-defun cae-compile-my-private-config (&optional arg)
  (interactive "P")
  (when (not (bound-and-true-p cae-config-finished-loading))
    (message "Config not finished loading")
    (cl-return-from cae-compile-my-private-config))
  (when (not cae-config-compilation-on-kill-enabled)
    (cl-return-from cae-compile-my-private-config))
  (mapc (lambda (s)
          (ignore-errors (byte-compile-file s))
	  (let ((native-comp-speed cae-compile-native-comp-speed))
            (ignore-errors (native-compile s))))
        (cae-compile-list-files-to-compile arg)))



;;;###autoload
(defun cae-compile-rebuild-package ()
  (when (and (stringp (buffer-file-name))
             (stringp (file-name-directory (buffer-file-name)))
             (string-prefix-p (expand-file-name ".local/straight/" doom-emacs-dir)
                              (file-name-directory (buffer-file-name))))
    (require 'straight)
    (when-let ((package (straight--determine-repo buffer-file-name)))
      (mapc #'delete-file (directory-files (file-name-directory
                                            (buffer-file-name))
                                           nil
                                           "flycheck_.*"))
      (straight-rebuild-package package))))

(defun cae-compile-next-file (files)
  (when files
    (let ((file (pop files)))
      (if (file-exists-p file)
          (progn
            (message "Compiling %s" file)
            (ignore-errors (byte-compile-file file))
            (let ((native-comp-speed cae-compile-native-comp-speed))
              (ignore-errors (native-compile file)))
            (run-with-idle-timer 0.1 nil #'cae-compile-next-file files))
        (run-with-idle-timer 0.1 nil #'cae-compile-next-file files)))))

(defun cae-compile-schedule-native-compilation ()
  (when (and (bound-and-true-p cae-config-finished-loading)
             (bound-and-true-p cae-config-incremental-compilation-enabled-p))
    (run-with-idle-timer 0.1 nil #'cae-compile-next-file
                         (cae-compile-list-files-to-compile))))

;; Run early in case I want to `C-g' and inspect the output.
(add-hook 'kill-emacs-hook #'cae-compile-my-private-config -1)
