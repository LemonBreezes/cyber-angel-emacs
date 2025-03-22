;;; preamble.el -*- lexical-binding: t; -*-

(defun remove-from-list (list-var element &optional append compare-fn)
  "Remove ELEMENT from the value of LIST-VAR if it is present.
The test for presence is done with `equal', or with COMPARE-FN if that's
non-nil.

This is the inverse of `add-to-list'. If LIST-VAR contains a list,
this function removes the first occurrence of ELEMENT from it.

The return value is the new value of LIST-VAR.

If optional argument APPEND is non-nil, and if ELEMENT is a cons cell,
then this function uses `eq' instead of `equal' to determine whether
ELEMENT is already present in LIST-VAR, and it only compares the car
of ELEMENT."
  (let ((lst (symbol-value list-var))
        (test-func (or compare-fn (if (and append (consp element)) #'eq #'equal))))
    (when lst
      (if (and append (consp element))
          ;; For cons cells with APPEND, we only compare the car
          (let ((car-element (car element)))
            (set list-var (delq (car (cl-member car-element lst
                                                :key #'car
                                                :test test-func))
                                lst)))
        ;; Normal case - remove the element if it exists
        (set list-var (cl-remove element lst :count 1 :test test-func))))
    (symbol-value list-var)))

(defun cae-add-dir-to-path (dir)
  "Add DIR to the PATH environment variable and `exec-path` if not already present."
  (when (and (file-directory-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat (getenv "PATH") ":" dir))
    (setq exec-path (append exec-path (list dir)))))

(when (string-equal system-type "android")
  (cae-add-dir-to-path "/data/data/com.termux/files/usr/bin"))

(when (getenv "WSL_DISTRO_NAME")
  (dolist (wslpath '("/mnt/c/Windows/System32/WindowsPowerShell/v1.0"
                     "/mnt/c/Windows/System32"))
    (cae-add-dir-to-path wslpath)))

;; On NixOS, this is necessary.
(when (executable-find "nixos-rebuild")
  (cae-add-dir-to-path "~/.local/bin"))

;; This is so that I don't accidentally start Emacs as a daemon.
(when (daemonp) (kill-emacs))

(when (boundp 'safe-local-variable-directories)
  (dolist (dir (list doom-user-dir doom-emacs-dir "~/org" (getenv "HOME")))
    (add-to-list 'safe-local-variable-directories dir)))

;; Do not start incrementally loading packages until I am actually AFK.
(setq doom-incremental-first-idle-timer 60)

(setq native-comp-async-jobs-number (num-processors)
      native-comp-async-report-warnings-errors 'silent)

(when cae-init-debug-enabled-p
  (load! "lisp/cae-debug" doom-user-dir))
(when cae-init-dir-locals-enabled-p
  (load! "lisp/cae-dir-locals" doom-user-dir))
(when cae-init-hacks-enabled-p
  (load! "lisp/cae-hacks" doom-user-dir))

;; Set a fallback theme.
(setq doom-theme 'wheatgrass)

(setq evil-undo-system 'undo-fu)

;; Make it easier to debug lazy loading issues.
(when init-file-debug (setq doom-incremental-first-idle-timer nil))

;; I never want to see loading messages.
(unless init-file-debug
  (cae-defadvice! cae-load-ignore-message-a (args)
    :filter-args #'load
    (cl-destructuring-bind (file &optional noerror nomessage nosuffix must-suffix) args
      (list file noerror t nosuffix must-suffix))))
