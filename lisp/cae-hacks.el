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

(defconst cae-hacks-gc-threshold (* 3 1024 1024 1024))
(defconst cae-hacks-gc-percentage 10)
(defvar cae-hacks--gc-messages nil)
(defvar cae-hacks--gcmh-mode nil)

(defun cae-hacks-disable-gc ()
  (setq cae-hacks--gcmh-mode (or cae-hacks--gcmh-mode
                                 gcmh-mode))
  (gcmh-mode -1)
  (setq cae-hacks--gc-messages (and cae-hacks--gc-messages
                                    garbage-collection-messages)
        garbage-collection-messages t
        gc-cons-threshold cae-hacks-gc-threshold
        gc-cons-percentage cae-hacks-gc-percentage)
  (when (timerp gcmh-idle-timer)
    (cancel-timer gcmh-idle-timer)))

(defun cae-hacks-disable-gc-temporarily (&rest _)
  (cae-hacks-disable-gc)
  (run-with-idle-timer
   10 nil
   (lambda ()
     (gcmh-mode cae-hacks--gcmh-mode)
     (setq garbage-collection-messages cae-hacks--gc-messages
           cae-hacks--gc-messages nil
           cae-hacks--gcmh-mode nil
           gc-cons-threshold gcmh-low-cons-threshold
           gc-cons-percentage cae-hacks-gc-percentage))))

(advice-add #'save-some-buffers :before #'cae-hacks-disable-gc-temporarily)
(add-hook 'git-timemachine-mode-hook #'cae-hacks-disable-gc-temporarily -1)
(add-hook 'kill-emacs-hook #'cae-hacks-disable-gc -10)
