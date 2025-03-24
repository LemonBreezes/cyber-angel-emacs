;;; vanilla-emacs-configs/evil-collection-loading-problem.el -*- lexical-binding: t; -*-

;; Bootstrap straight
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                                    'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'evil)
(straight-use-package 'evil-collection)

(defun evil-collection-check-preloaded-packages ()
  "Check if packages used by evil-collection are already loaded and output a message.
This function checks a list of built-in packages that evil-collection modules depend on."
  (interactive)
  (let ((packages '(tabulated-list
                    tab-bar
                    simple
                    replace
                    package
                    kmacro
                    info
                    help-mode
                    elisp-mode
                    eldoc
                    compile
                    comint)))
    (dolist (package packages)
      (if (featurep package)
          (message "Package '%s' is already loaded." package)
        (message "Package '%s' is not loaded." package)))
    (message "Check complete.")))

;; At this point:
;; compile is not loaded
;; comint is not loaded
;; kmacro is not loaded

(require 'evil)
(message "%s %s"
         (let ((features-old features))
           (require 'evil-collection)
           (cl-set-difference features features-old))
         (let ((features-old features))
           (evil-collection-init)
           (cl-set-difference features features-old)))

;;(evil-collection annalist)
;;(evil-collection-unimpaired
;; evil-collection-tabulated-list
;; evil-collection-tab-bar
;; evil-collection-simple
;; evil-collection-replace
;; evil-collection-process-menu
;; evil-collection-package-menu
;; evil-collection-kmacro
;; evil-collection-info
;; evil-collection-indent
;; evil-collection-help
;; evil-collection-elisp-mode
;; evil-collection-eldoc
;; evil-collection-compile
;; evil-collection-comint
;; evil-collection-buff-menu)
