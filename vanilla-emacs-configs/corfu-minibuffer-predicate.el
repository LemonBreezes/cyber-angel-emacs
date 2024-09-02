;;; vanilla-emacs-configs/corfu-minibuffer-predicate.el -*- lexical-binding: t; -*-

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

(straight-use-package 'corfu)
(require 'corfu)

(setq global-corfu-minibuffer
      (defun +corfu-minibuffer-predicate ()
        (not (or (bound-and-true-p mct--active)
                 (where-is-internal #'vertico-exit
                                    (list (current-local-map)))
                 (and (featurep 'auth-source)
                      (eq (current-local-map) read-passwd-map))
                 (and (featurep 'helm-core) (helm--alive-p))
                 (and (featurep 'ido) (ido-active))
                 (where-is-internal #'minibuffer-complete
                                    (list (current-local-map)))
                 (memq #'ivy--queue-exhibit post-command-hook)))))

(global-corfu-mode +1)
