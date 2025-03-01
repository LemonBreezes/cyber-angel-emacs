;;; lisp/cae-tty.el -*- lexical-binding: t; -*-

(defun cae-terminal-type ()
  (cond
   ;; If Emacs is running in a GUI, you have full Unicode/font support.
   ((cae-display-graphic-p)
    2)
   ((getenv "WT_SESSION")
    1)
   ;; Linux virtual console (tty). The TERM variable is usually "linux"
   ((string-prefix-p "/dev/tty" (terminal-name))
    0)
   ;; Otherwise, if LANG indicates UTF-8 you’re probably in a modern terminal emulator.
   ((and (getenv "LANG") (string-match "utf8" (getenv "LANG")))
    1)
   (t 0)))

(unless (cae-display-graphic-p)
  ;; Stuff so that Emacs doesn't break in the Terminal.
  (when (modulep! :completion vertico +childframe)
    (remove-hook 'vertico-mode-hook #'vertico-posframe-mode))
  (when (modulep! :ui ligatures)
    (setq +ligatures-in-modes nil)
    (remove-hook 'doom-init-ui-hook #'+ligatures-init-h)
    (remove-hook 'doom-init-ui-hook #'+ligature-init-composition-table-h)
    (remove-hook 'doom-init-ui-hook #'+ligatures-init-buffer-h))

  ;; Make the cursor blink. I set this because the solid cursor makes it harder
  ;; to see the character under it.
  (setq visible-cursor t)

  ;; Remove some hooks that don't work in the terminal.
  (remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
    #'vi-tilde-fringe-mode)
  (let ((hook (if (daemonp)
                  'server-after-make-frame-hook
                'after-init-hook)))
    (remove-hook hook #'doom-init-fonts-h -100)
    (remove-hook hook #'doom-init-theme-h -90))
  (remove-hook 'doom-init-ui-hook #'window-divider-mode)

  ;; Allow copying from Emacs to OS in SSH sessions.
  (use-package! clipetty
    :defer t :when (and (getenv "SSH_CONNECTION")
                        (not (cae-display-graphic-p)))
    :init (add-hook 'doom-first-buffer-hook #'global-clipetty-mode))

  ;; Make some overlays more visible in the terminal.
  (after! corfu
    (set-face-attribute 'corfu-default nil :background 'unspecified))
  (after! eros
    (set-face-attribute 'eros-result-overlay-face nil :background 'unspecified))
  (after! magit
    (set-face-attribute 'magit-section-highlight nil :background 'unspecified)
    (set-face-attribute 'magit-diff-added-highlight nil :background 'unspecified)
    (set-face-attribute 'magit-diff-base-highlight nil :background 'unspecified)
    (set-face-attribute 'magit-diff-added nil :background 'unspecified))
  (after! lsp-headerline
    (set-face-attribute 'header-line nil :inherit 'mode-line-inactive))

  ;; Remove (error Window system frame should be used) in the terminal when using
  ;; `chatgpt-shell'.
  (after! chatgpt-shell
    ;; This feature is not supported in terminal Emacs anyways.
    (setq chatgpt-shell-render-latex nil))

  (cae-tty-disable-unicode-and-or-icons))

;; Fix clipboard issues in the terminal.
(cond ((executable-find "termux-setup-storage")
       (setq xclip-method 'termux-clipboard-get)))

;; BUG Do not emit an error if `xclip' is not found.
(defadvice! cae-handle-missing-xclip-program ()
  :before-until #'doom-init-clipboard-in-tty-emacs-h
  (and (memq system-type '(gnu gnu/linux gnu/kfreebsd))
       (not (executable-find "xclip"))))

(when (modulep! :tools pdf)
  (use-package! pdftotext
    :defer t :init
    (defadvice! +pdf-view-mode-a (oldfun &rest args)
      :around #'pdf-view-mode
      (if (cae-display-graphic-p)
          (apply oldfun args)
        (apply #'pdftotext-mode args)))))


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
