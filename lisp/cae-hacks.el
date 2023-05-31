;;; lisp/cae-hacks.el -*- lexical-binding: t; -*-

;; For when we compile Doom.
(defvar personal-keybindings nil)

(unless (or (featurep 'smartparens)
            (autoloadp (symbol-function 'sp-local-pair)))
  (defalias 'sp-local-pair #'ignore))

(defun cae-hacks-shut-up-a (oldfun &rest args)
  (advice-add #'message :override #'ignore)
  (unwind-protect (apply oldfun args)
    (advice-remove #'message #'ignore)))

;; Prevent the minibuffer from "glitching" the workspace switch.
(defadvice! cae-hacks-workspace-ignore-minibuffer-a (&rest _)
  :before-until #'+workspace/switch-to
  (minibuffer-window-active-p (selected-window)))

;; Prevent hydras from remaining active when switching workspaces.
(defun cae-hacks-hydra-quit-h (&rest _)
  (hydra-keyboard-quit))
(after! hydra
  (add-hook 'persp-before-switch-functions #'cae-hacks-hydra-quit-h))

;; Make `advice-remove' ignore the keyword argument
(defadvice! cae-hacks-advice-remove-ignore-keyword-args-a (args)
  :filter-args #'advice-remove
  (if (keywordp (nth 1 args))
      (list (nth 0 args) (nth 2 args))
    args))

;; Use --no-sandbox when running Chromium, Discord, etc. as the root user.
(when (eq (user-uid) 0)
  (defadvice! cae-hacks-call-process-shell-command-a (args)
    :filter-args #'call-process-shell-command
    (when (cl-member (file-name-base (car (split-string (car args) " ")))
                     '("chromium-bin-browser"
                       "chromium-bin"
                       "google-chrome-beta"
                       "discord"
                       "signal-desktop"
                       "vscode" "vscodium" "code")
                     :test #'string=)
      (setf (car args)
            (concat (string-trim-right (car args))
                    " --no-sandbox")))
    args))

;; Call `pp-eval-last-sexp' when `eros-eval-last-sexp' is called with a negative
;; prefix argument
(defadvice! cae-hacks-eros-eval-last-sexp-with-pp-a (oldfun arg)
  :around #'eros-eval-last-sexp
  (if (or (eq arg '-)
          (and (numberp arg)
               (< arg 0)))
      (funcall #'pp-eval-last-sexp
               (if (numberp arg)
                   arg nil))
    (funcall oldfun arg)))

;; A generic adviser for responding yes to yes or no prompts automatically.
(defun cae-hacks-always-yes-a (oldfun &rest args)
  (cl-letf (((symbol-function #'yes-or-no-p) (symbol-function #'always))
            ((symbol-function #'y-or-n-p) (symbol-function #'always)))
    (apply oldfun args)))

;; Compile Vterm without asking.
(defvar vterm-always-compile-module t)

;; Use the system's `libvterm' if available.
(defvar vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes")

;; I'm disabling this workaround until I run into a problem.
(defadvice! cae-hacks-ignore-this-command-keys-a (oldfun &rest args)
  :around #'embark--act
  (advice-add #'set--this-command-keys :override #'ignore)
  (unwind-protect (apply oldfun args)
    (advice-remove #'set--this-command-keys #'ignore)))

;; White list local variables for some projects.
;; (defadvice! cae-hacks-whitelist-some-dir-locals-a (oldfun variables dir-name)
;;   :around #'hack-local-variables-filter
;;   (if (and default-directory
;;            (cl-member default-directory
;;                       `(,doom-user-dir "~/src/atlas/")
;;                       :test #'file-in-directory-p))
;;       (progn (advice-add #'safe-local-variable-p :override #'always)
;;              (unwind-protect (funcall oldfun variables dir-name)
;;                (advice-remove #'safe-local-variable-p #'always)))
;;     (funcall oldfun variables dir-name)))

;; Disable `diff-hl-mode' in my Doom private dir.
(defadvice! cae-hacks-disable-diff-hl-in-private-config-a (&optional arg)
  :before-until #'diff-hl-mode
  (file-in-directory-p default-directory doom-user-dir))

;; Fix `save-some-buffers' so that I can continue the command after quitting a
;; diff with "q".
(defadvice! cae-hacks-quit-view-mode-a (oldfun)
  :around #'+popup/quit-window
  (if view-mode
      (View-quit)
    (funcall oldfun)))
(advice-add #'meow-quit :around #'cae-hacks-quit-view-mode-a)

;; Remove this as soon as Doom fixes the error upstream.
(defadvice! cae-hacks-monkey-patch-consult-for-doom (oldfun &rest args)
  :around #'consult--ripgrep-make-builder
  (if (null args)
      #'consult--ripgrep-make-builder
    (apply oldfun args)))

;; Make `eshell-previous-prompt' properly handle the case when there is no
;; previous prompt.
(defadvice! cae-hacks-jump-back-if-bolp (oldfun &rest args)
  :around #'eshell-previous-prompt
  (let ((p (point)))
    (apply oldfun args)
    (when (bolp)
      (goto-char p))))

;;; GC hacks

;; The purpose of these functions is to safely disable GC during long-running
;; tasks.

(defconst cae-hacks-gc-cons-threshold (* 3 1024 1024 1024))
(defconst cae-hacks-gc-cons-percentage 10)
(defconst cae-hacs-gc-idle-delay 5)
(defvar cae-hacks--gc-percentage nil)
(defvar cae-hacks--gc-messages nil)
(defvar cae-hacks--gc-disabled nil)     ;Make these functions idempotent.
(defvar cae-hacks--gcmh-mode nil)
(defvar cae-hacks--gc-idle-timer nil)

(defun cae-hacks-disable-gc ()
  "Raise the GC threshold to a large value and enable GC messages."
  (unless cae-hacks--gc-disabled
    (setq cae-hacks--gcmh-mode        (bound-and-true-p gcmh-mode))
    (and (fboundp #'gcmh-mode) (gcmh-mode -1))
    (setq cae-hacks--gc-messages      garbage-collection-messages
          cae-hacks--gc-percentage    gc-cons-percentage
          garbage-collection-messages t
          gc-cons-threshold           cae-hacks-gc-cons-threshold
          gc-cons-percentage          cae-hacks-gc-cons-percentage)
    (setq cae-hacks--gc-idle-timer
          (run-with-idle-timer cae-hacs-gc-idle-delay
                               nil #'cae-hacks-garbage-collect))
    (when (timerp (bound-and-true-p gcmh-idle-timer))
      (cancel-timer gcmh-idle-timer))
    (add-hook 'post-gc-hook #'cae-hacks-enable-gc)
    (setq cae-hacks--gc-disabled t)))

(defun cae-hacks-garbage-collect ()
  (garbage-collect)
  (cae-hacks-enable-gc))

(defun cae-hacks-enable-gc ()
  "This is the inverse of `cae-hacks-disable-gc'.
It is meant to be used as a `post-gc-hook'."
  (when cae-hacks--gc-disabled
    (and (fboundp #'gcmh-mode) (gcmh-mode cae-hacks--gcmh-mode))
    (when (timerp cae-hacks--gc-idle-timer)
      (cancel-timer cae-hacks--gc-idle-timer))
    (setq garbage-collection-messages cae-hacks--gc-messages
          gc-cons-percentage          cae-hacks--gc-percentage
          cae-hacks--gc-messages      nil
          cae-hacks--gc-percentage    nil
          cae-hacks--gcmh-mode        nil
          cae-hacks--gc-idle-timer    nil)
    (remove-hook 'post-gc-hook #'cae-hacks-enable-gc)
    (setq cae-hacks--gc-disabled nil)))

(if (boundp 'after-focus-change-function)
    (add-function :after after-focus-change-function
                  (lambda ()
                    (unless (frame-focus-state)
                      (cae-hacks-garbage-collect))))
  (add-hook 'after-focus-change-function #'cae-hacks-garbage-collect))

(add-hook 'kill-emacs-hook #'cae-hacks-disable-gc -10)
