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
      (defun +corfu-enable-in-minibuffer-p ()
        "Return non-nil if Corfu should be enabled in the minibuffer.
See `+corfu-want-minibuffer-completion'."
        (pcase +corfu-want-minibuffer-completion
          ('nil nil)
          ('aggressive
           (not (or (bound-and-true-p mct--active)
                    (bound-and-true-p vertico--input)
                    (and (featurep 'auth-source)
                         (eq (current-local-map) read-passwd-map))
                    (and (featurep 'helm-core) (helm--alive-p))
                    (and (featurep 'ido) (ido-active))
                    (where-is-internal 'minibuffer-complete
                                       (list (current-local-map)))
                    (memq #'ivy--queue-exhibit post-command-hook))))
          (_ (where-is-internal #'completion-at-point
                                (list (current-local-map)))))))

(add-hook 'minibuffer-setup-hook
          (defun +corfu-add-cape-dabbrev-h ()
            (add-hook 'completion-at-point-functions #'cape-dabbrev 20 t)))

(setq corfu-auto t
      corfu-auto-delay 0.05)
(global-corfu-mode +1)

(straight-use-package 'vertico)
(require 'vertico)
(vertico-mode +1)
