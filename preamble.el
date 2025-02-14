;;; preamble.el -*- lexical-binding: t; -*-

(when (string-equal system-type "android") ;; Add Termux binaries to PATH environment
  (let ((termuxpath "/data/data/com.termux/files/usr/bin"))
    (setenv "PATH" (concat (getenv "PATH") ":" termuxpath))
    (setq exec-path (append exec-path (list termuxpath)))))

(when (getenv "WSL_DISTRO_NAME")
  (let ((wslpath "/mnt/c/Windows/System32/WindowsPowerShell/v1.0"))
    (setenv "PATH" (concat (getenv "PATH") ":" wslpath))
    (setq exec-path (append exec-path (list wslpath)))))

(when (getenv "WSL_DISTRO_NAME")
  (let ((wslpath "/mnt/c/Windows/System32"))
    (setenv "PATH" (concat (getenv "PATH") ":" wslpath))
    (setq exec-path (append exec-path (list wslpath)))))

;; This is so that I don't accidentally start Emacs as a daemon.
(when (daemonp) (kill-emacs))

(when (boundp 'safe-local-variable-directories)
  (add-to-list 'safe-local-variable-directories doom-user-dir)
  (add-to-list 'safe-local-variable-directories doom-emacs-dir)
  (add-to-list 'safe-local-variable-directories "~/org")
  (add-to-list 'safe-local-variable-directories (getenv "HOME")))

;; Add hostname to the modeline.
(let ((hostname (concat " " (truncate-string-to-width
                             (secure-hash 'sha512 system-name) 6))))
  (setf (aref hostname 1) (downcase (aref system-name 0)))
  (unless (or (equal global-mode-string (list hostname))
              (cl-member hostname global-mode-string
                         :test (lambda (x y) (and (stringp y) (string-equal x y)))))
    (if (and global-mode-string (listp global-mode-string))
        (appendq! global-mode-string (list hostname))
      (setq global-mode-string
            (list hostname)))))

;; Set up the secrets directory and its module path.
(defvar cae-multi-secrets-dir (expand-file-name "secrets/" doom-user-dir))
(make-directory cae-multi-secrets-dir t)
(defvar cae-multi-secrets-modules-dir (concat cae-multi-secrets-dir "modules/"))
(make-directory cae-multi-secrets-modules-dir t)
(add-to-list 'doom-module-load-path cae-multi-secrets-modules-dir)

;; This is where I clone Git projects to.
(defvar cae-repo-dir "~/src/")

(when (and (>= (num-processors) 32)
           (not (eq system-type 'windows-nt))
           ;; Experimental incremental garbage collector.
           (not (functionp 'igc-collect))
           ;; 180GB of RAM.
           (> (car (memory-info))
              (* 180 1024 1024)))
  (let ((cae-gc-cons-threshold (* 128 1024 1024 1024)))
    (setq gcmh-high-cons-threshold cae-gc-cons-threshold
          consult--gc-threshold cae-gc-cons-threshold
          cae-hacks-gc-cons-threshold cae-gc-cons-threshold
          +lsp--default-gcmh-high-cons-threshold cae-gc-cons-threshold)))

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
