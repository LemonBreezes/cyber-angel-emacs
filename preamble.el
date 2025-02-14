;;; preamble.el -*- lexical-binding: t; -*-
(defun cae-add-dir-to-path (dir)
  "Add DIR to the PATH environment variable and `exec-path` if not already present."
  (when (and (file-directory-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat (getenv "PATH") ":" dir))
    (setq exec-path (append exec-path (list dir)))))
(defun cae-add-hostname-to-modeline ()
  "Compute a short hash from system-name and add it to `global-mode-string`."
  (let ((hostname (concat " " (truncate-string-to-width
                                 (secure-hash 'sha512 system-name) 6))))
    (setf (aref hostname 1) (downcase (aref system-name 0)))
    (unless (or (equal global-mode-string (list hostname))
                (cl-member hostname global-mode-string
                           :test (lambda (x y) (and (stringp y) (string-equal x y)))))
      (if (and global-mode-string (listp global-mode-string))
          (appendq! global-mode-string (list hostname))
        (setq global-mode-string (list hostname))))))
(when (string-equal system-type "android")
  (cae-add-dir-to-path "/data/data/com.termux/files/usr/bin"))

(when (getenv "WSL_DISTRO_NAME")
  (dolist (wslpath '("/mnt/c/Windows/System32/WindowsPowerShell/v1.0"
                     "/mnt/c/Windows/System32"))
    (cae-add-dir-to-path wslpath)))

;; This is so that I don't accidentally start Emacs as a daemon.
(when (daemonp) (kill-emacs))

(when (boundp 'safe-local-variable-directories)
  (dolist (dir (list doom-user-dir doom-emacs-dir "~/org" (getenv "HOME")))
    (add-to-list 'safe-local-variable-directories dir)))

(cae-add-hostname-to-modeline)

;; Set up the secrets directory and its module path.
(defvar cae-multi-secrets-dir (expand-file-name "secrets/" doom-user-dir))
(make-directory cae-multi-secrets-dir t)
(defvar cae-multi-secrets-modules-dir (concat cae-multi-secrets-dir "modules/"))
(make-directory cae-multi-secrets-modules-dir t)
(add-to-list 'doom-module-load-path cae-multi-secrets-modules-dir)

;; This is where I clone Git projects to.
(defvar cae-repo-dir "~/src/")

(defconst cae-mem-threshold (* 180 1024 1024)
  "Memory threshold in bytes for enabling high GC thresholds.")
(defconst cae-gc-threshold (* 128 1024 1024 1024)
  "GC threshold to use when the system has ample memory.")

(when (and (>= (num-processors) 32)
           (not (eq system-type 'windows-nt))
           (not (functionp 'igc-collect))
           (> (car (memory-info)) cae-mem-threshold))
  (setq gcmh-high-cons-threshold cae-gc-threshold
        consult--gc-threshold cae-gc-threshold
        cae-hacks-gc-cons-threshold cae-gc-threshold
        +lsp--default-gcmh-high-cons-threshold cae-gc-threshold))

;; Do not start incrementally loading packages until I am actually AFK.
(setq doom-incremental-first-idle-timer 60)

(setq native-comp-async-jobs-number (num-processors)
      native-comp-async-report-warnings-errors 'silent)

(load! "lisp/cae-debug")
(load! "lisp/cae-hacks")

;; Set a fallback theme.
(setq doom-theme 'wheatgrass)

(setq evil-undo-system 'undo-fu)

;; Make it easier to debug lazy loading issues.
(when init-file-debug (setq doom-incremental-first-idle-timer nil))

;; I never want to see loading messages.
(unless init-file-debug
  (defadvice! cae-load-ignore-message-a (args) :filter-args #'load
    (cl-destructuring-bind (file &optional noerror nomessage nosuffix must-suffix) args
      (list file noerror t nosuffix must-suffix))))
