;;; vanilla-emacs-configs/evil-collection-loading-problem.el -*- lexical-binding: t; -*-

;; Bootstrap straight
(defvar features-not-loaded nil)

;; Checking a few extra ones.
(cl-loop for feature in '(tabulated-list tab-bar simple replace kmacro info help elisp-mode eldoc compile comint cus-edit occur) do
         (if (featurep feature)
             (message "Package '%s' is already loaded." feature)
           (message "Package '%s' is not loaded." feature)
           (setq features-not-loaded
                 (cons feature features-not-loaded))))
(message "Check complete.")

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

(require 'evil)

(cl-loop for feature in '(info kmacro) do
         (when (featurep feature)
           (message "Package '%s' was loaded by Evil." feature)
           (setq features-not-loaded
                 (delete feature features-not-loaded))))

(require 'evil-collection)
(evil-collection-init)

;; This one lists what features are loaded at the time of calling `evil-collection-init'.
(cl-loop for feature in features do
         (when (string-prefix-p "evil-collection-" (symbol-name feature))
           (message "Loaded feature: %s" feature)
           (when (featurep (intern (string-remove-prefix "evil-collection-" (symbol-name feature))))
             (message "  - Underlying package: %s" (intern (string-remove-prefix "evil-collection-" (symbol-name feature)))))))

(cl-loop for feature in features-not-loaded do
         (when (featurep feature)
           (message "Packages %s is now loaded" feature)))
