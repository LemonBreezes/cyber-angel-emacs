;;; core.el -*- lexical-binding: t; -*-

(require 'cae-lib)

;; Load secrets
(when (file-exists-p (concat cae-multi-secrets-dir "secrets.el"))
  (load! (concat cae-multi-secrets-dir "secrets.el") "/"))

;; Ensure this is defined even if its module is not loaded.
(defvar cae-default-minibuffer-maps
  (append '(minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map
            read-expression-map)
          (when (modulep! :completion ivy)
            '(ivy-minibuffer-map
              ivy-switch-buffer-map))
          (when (or (modulep! :cae helm) (modulep! :completion helm))
            '(helm-map
              helm-rg-map
              helm-read-file-map)))
  "A list of all the keymaps used for the minibuffer.")

;; This helps me debug issues with my config.
(setq persistent-scratch-autosave-mode t)
(setq persistent-scratch-what-to-save
      '(point))
(setq persistent-scratch-autosave-interval
      '(idle . 10))
(setq persistent-scratch-backup-directory
      (concat doom-cache-dir "persistent-scratch-backups/"))
(setq persistent-scratch-save-file
      (concat user-emacs-directory "persistent-scratch"))
(make-directory persistent-scratch-backup-directory t)
(setq persistent-scratch-backup-filter
      (lambda (_)
        (when (> (doom-directory-size persistent-scratch-backup-directory)
                 (* 1024 1024))
          (message (concat "URGENT: Persistent scratch is over 1 GB. "
                           "Write logic for cleaning it.")))))
(dolist (buffer (buffer-list))
  (when (string= "*scratch*" (buffer-name buffer))
    (with-current-buffer buffer
      (persistent-scratch-mode +1))))
(persistent-scratch-setup-default)

;; Helm is not our main completion system.
(when (and (modulep! :completion helm)
           (modulep! :completion vertico))
  (remove-hook 'doom-first-input-hook #'helm-mode))

;; Have a fallback completion system.
(unless (or (modulep! :completion helm)
            (modulep! :completion ivy)
            (modulep! :completion vertico))
  (icomplete-mode +1)
  (icomplete-vertical-mode +1))

;; For some reason Persp is picking up a few buffers that it should not.
(when (modulep! :ui workspaces)
  (add-hook! 'persp-add-buffer-on-after-change-major-mode-filter-functions
    (defun cae-persp-skip-buffer-p (buffer)
      (string-match-p "^\\*.*[lL]og.*\\*" (buffer-name buffer)))))

;; Set up fonts
(unless (or (memq system-type '(cygwin windows-nt ms-dos))
            (not (cae-display-graphic-p)))
  (let ((fonts-to-check '(("Aporetic Sans Mono" doom-font)
                          ("IBM Plex Mono" doom-serif-font)
                          ("Aporetic Sans" doom-variable-pitch-font))))
    (dolist (font fonts-to-check)
      (if (find-font (font-spec :name (car font)))
          (set (cadr font) (font-spec :family (car font) :size 18))
        (warn "Font %s does not exist!" (car font))))))

;; Do not break my clipboard in SSH sessions.
(when (and (modulep! :os tty)
           (getenv "SSH_TTY")
           (not (cae-display-graphic-p)))
  (remove-hook 'tty-setup-hook #'doom-init-clipboard-in-tty-emacs-h))

(after! tramp
  ;; Tramp still doesn't work for me on this computer so don't blindly copy
  ;; this.
  (setq tramp-shell-prompt-pattern
        "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"
        tramp-default-remote-shell (executable-find "bash")))

;; Automatically mark scripts as executable.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(after! ghub
  (setq ghub-use-workaround-for-emacs-bug-54989
        (version< emacs-version "29.0")))

;; I really don't like this advice. Just let me kill the buffer.
(advice-remove #'kill-current-buffer #'doom--switch-to-fallback-buffer-maybe-a)

;; I regularly PR Doom Emacs.
(cae-advice-add #'doom-docs-read-only-h :override #'ignore)

;; If I start Emacs in `.xinitrc' without a window manager, I want to be in
;; fullscreen.
(when (and (eq 'x (framep (selected-frame)))
           (not (or (getenv "EXWM_RUNNING")
                    (getenv "RATPOISON")
                    (getenv "I3SOCK")
                    (getenv "KDE_FULL_SESSION")
                    (getenv "GNOME_DESKTOP_SESSION_ID")
                    (getenv "XDG_CURRENT_DESKTOP")
                    (getenv "WAYLAND_DISPLAY")))
           (or (not (modulep! :cae exwm))
               (bound-and-true-p cae-exwm-disabled-p)))
  (toggle-frame-fullscreen))
