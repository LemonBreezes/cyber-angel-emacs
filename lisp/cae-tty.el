;;; lisp/cae-tty.el -*- lexical-binding: t; -*-

(when (cae-tty-disable-unicode-p)
  (after! org-eldoc
    (setq org-eldoc-breadcrumb-separator " -> "))
  (after! embrace
    (setq embrace-help-separator " -> "))
  (after! replace
    (setq query-replace-from-to-separator " -> "))
  (after! helm-files
    (setq helm-rsync-percent-sign "%"))
  (after! which-key
    (setq which-key-separator " -> "))
  (dolist (fn '(nerd-icons-faicon
                nerd-icons-octicon))
    (advice-add fn :override #'ignore)))

;; Stuff so that Emacs doesn't break in the Terminal.
(when (modulep! :completion vertico +childframe)
  (unless (cae-display-graphic-p)
    (remove-hook 'vertico-mode-hook #'vertico-posframe-mode)))
(when (modulep! :ui ligatures)
  (when (cae-tty-disable-unicode-p)
    (setq +ligatures-in-modes nil)
    (remove-hook 'doom-init-ui-hook #'+ligatures-init-h)
    (remove-hook 'doom-init-ui-hook #'+ligature-init-composition-table-h)
    (remove-hook 'doom-init-ui-hook #'+ligatures-init-buffer-h)))

;; Remove some hooks that don't work in the terminal.
(unless (cae-display-graphic-p)
  (remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
    #'vi-tilde-fringe-mode)
  (let ((hook (if (daemonp)
                  'server-after-make-frame-hook
                'after-init-hook)))
    (remove-hook hook #'doom-init-fonts-h -100)
    (remove-hook hook #'doom-init-theme-h -90))
  (remove-hook 'doom-init-ui-hook #'window-divider-mode))

(when (modulep! :tools pdf)
  (use-package! pdftotext
    :defer t :init
    (defadvice! +pdf-view-mode-a (oldfun &rest args)
      :around #'pdf-view-mode
      (if (cae-display-graphic-p)
          (apply oldfun args)
        (apply #'pdftotext-mode args)))))

;; Automatically enable & disable the posframe when using multiple client frames.
;;(add-hook 'pre-command-hook
;;          (cae-defun cae-tty-setup-posframe-h ()
;;            "Setup frame for TTY."
;;            (if (cae-display-graphic-p)
;;                (progn (when (and (boundp 'vertico-posframe-mode)
;;                                  (not vertico-posframe-mode))
;;                         (vertico-posframe-mode +1))
;;                       (when (and (boundp 'helm-display-function)
;;                                  (not (eq helm-display-function #'helm-posframe-display)))
;;                         (posframe-delete-all)
;;                         (helm-posframe-enable)))
;;              (when (bound-and-true-p vertico-posframe-mode)
;;                (vertico-posframe-mode -1))
;;              (when (and (boundp 'helm-display-function)
;;                         (eq helm-display-function #'helm-posframe-display))
;;                (helm-posframe-disable)))))
;;
;;(add-hook 'doom-switch-frame-hook
;;          (cae-defun cae-tty-setup-frame-h ()
;;            (unless (cae-display-graphic-p)
;;              (when (bound-and-true-p corfu-mode)
;;                (corfu-terminal-mode +1)))))
