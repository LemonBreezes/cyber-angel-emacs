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

(require 'evil)

(message "%s %s"
         (let ((features-old features))
           (require 'evil-collection)
           (cl-set-difference features features-old))
         (let ((features-old features))
           (evil-collection-init)
           (cl-set-difference features features-old)))
