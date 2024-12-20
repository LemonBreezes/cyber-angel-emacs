;;; lisp/cae-tty.el -*- lexical-binding: t; -*-

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

;; Make some overlays more visible in the terminal.
(unless (cae-display-graphic-p)
  (after! corfu
    (set-face-attribute 'corfu-default nil :background nil))
  (after! eros
    (set-face-attribute 'eros-result-overlay-face nil :background nil))
  (after! magit
    (set-face-attribute 'magit-section-highlight nil :background nil)
    (set-face-attribute 'magit-diff-added-highlight nil :background nil)
    (set-face-attribute 'magit-diff-base-highlight nil :background nil)
    (set-face-attribute 'magit-diff-added nil :background nil))
  (after! lsp-headerline
    (set-face-attribute 'header-line nil :inherit 'mode-line-inactive))
  (remove-hook 'dired-mode-hook #'diredfl-mode))

(when (modulep! :tools pdf)
  (use-package! pdftotext
    :defer t :init
    (defadvice! +pdf-view-mode-a (oldfun &rest args)
      :around #'pdf-view-mode
      (if (cae-display-graphic-p)
          (apply oldfun args)
        (apply #'pdftotext-mode args)))))

(when (not (cae-display-graphic-p))
  (cae-tty-disable-unicode))

;; This is code from when I tried to run Emacs simultaneously from terminal
;; frames and GUI frames but decided to remove it because I did not use Emacs
;; that way much.

;; Would also need to set `vertico-multiform-categories'.

;; Automatically enable & disable the posframe when using multiple client frames.
;;(add-hook! 'pre-command-hook
;;  (defun cae-tty-setup-posframe-h ()
;;    "Setup frame for TTY."
;;    (if (cae-display-graphic-p)
;;        (progn (when (and (boundp 'vertico-posframe-mode)
;;                          (not vertico-posframe-mode))
;;                 (vertico-posframe-mode +1))
;;               (when (and (boundp 'helm-display-function)
;;                          (not (eq helm-display-function #'helm-posframe-display)))
;;                 (posframe-delete-all)
;;                 (helm-posframe-enable)))
;;      (when (bound-and-true-p vertico-posframe-mode)
;;        (vertico-posframe-mode -1))
;;      (when (and (boundp 'helm-display-function)
;;                 (eq helm-display-function #'helm-posframe-display))
;;        (helm-posframe-disable)))))
;;
;;(add-hook! 'doom-switch-frame-hook
;;  (defun cae-tty-setup-frame-h ()
;;    (unless (cae-display-graphic-p)
;;      (when (bound-and-true-p corfu-mode)
;;        (corfu-terminal-mode +1)))))
