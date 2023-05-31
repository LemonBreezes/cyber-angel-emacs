;;; lisp/cae-hacks.el -*- lexical-binding: t; -*-

;; This function is like `doom-shut-up-a' but does not write messages to the
;; messages buffer.
(defun my-doom-shut-up-a (oldfun &rest args)
  (advice-add #'message :override #'ignore)
  (unwind-protect (quiet! (apply oldfun args))
    (advice-remove #'message #'ignore)))

;; Prevent the minibuffer from "glitching" the workspace switch.
(defadvice! +workspace/switch-to-ignore-minibuffer-a (&rest _)
  :before-until #'+workspace/switch-to
  (minibuffer-window-active-p (selected-window)))

;; Prevent hydras from remaining active when switching workspaces.
(after! (:all persp-mode hydra)
  (defun +hydra-quit-a (&rest _)
    (hydra-keyboard-quit))
  (add-hook 'persp-before-switch-functions #'+hydra-quit-a))

;; Prevent hydras from glitching Switch Window out
(after! hydra
  (defadvice! +switch-window--list-a (&rest _)
    :before #'switch-window--list
    (hydra-keyboard-quit)))

;; Make `advice-remove' ignore the keyword argument
(defadvice! advice-remove--ignore-keyword-args (args)
  :filter-args #'advice-remove
  (if (keywordp (nth 1 args))
      (list (nth 0 args) (nth 2 args))
    args))

;; Use --no-sandbox when running Chromium, Discord, etc. as the root user.
(when (eq (user-uid) 0)
  (defun +call-process-shell-command-a (args)
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
    args)
  (advice-add #'call-process-shell-command :filter-args
              #'+call-process-shell-command-a))

;; Call `pp-eval-last-sexp' when `eros-eval-last-sexp' is called with a negative
;; prefix argument
(defadvice! +eros-eval-lavst-sexp-a (oldfun &rest args)
  :around #'eros-eval-lastv-sexp
  (if (eq (car args) '-)
      (funcall #'pp-eval-last-sexp (cdr args))
    (apply oldfun args)))
