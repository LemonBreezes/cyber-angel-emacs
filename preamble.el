;;; preamble.el -*- lexical-binding: t; -*-

(defun cae-add-dir-to-path (dir)
  "Add DIR to the PATH environment variable and `exec-path` if not already present."
  (when (and (file-directory-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat (getenv "PATH") ":" dir))
    (setq exec-path (append exec-path (list dir)))))

(defun cae-add-cpus-to-modeline ()
  "Show the number of CPUs on the system in `global-mode-string`."
  (let* ((n-cpus (num-processors))
         (cpu-string (format "%d" n-cpus)))
    (unless (or (equal global-mode-string (list cpu-string))
                (cl-member cpu-string global-mode-string
                           :test (lambda (x y) (and (stringp y) (string-equal x y)))))
      (if (and global-mode-string (listp global-mode-string))
          (appendq! global-mode-string (list cpu-string))
        (setq global-mode-string (list cpu-string))))))

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

(cae-add-cpus-to-modeline)

;; Set up the secrets directory and its module path.
(defvar cae-multi-secrets-dir (expand-file-name "secrets/" doom-user-dir))
(make-directory cae-multi-secrets-dir t)
(defvar cae-multi-secrets-modules-dir (concat cae-multi-secrets-dir "modules/"))
(make-directory cae-multi-secrets-modules-dir t)
(defvar cae-multi-org-dir "~/org/")
(make-directory cae-multi-org-dir t)
(add-to-list 'doom-module-load-path cae-multi-secrets-modules-dir)

;; This is where I clone Git projects to.
(defvar cae-repo-dir "~/src/")

;; Do not start incrementally loading packages until I am actually AFK.
(setq doom-incremental-first-idle-timer 60)

(setq native-comp-async-jobs-number (num-processors)
      native-comp-async-report-warnings-errors 'silent)

(load! "lisp/cae-debug")
(load! "lisp/cae-dir-locals")
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
