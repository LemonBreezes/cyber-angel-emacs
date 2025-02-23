;;; core.el -*- lexical-binding: t; -*-

;; Load secrets
(when (file-exists-p (concat cae-multi-secrets-dir "secrets.el"))
  (load! (concat cae-multi-secrets-dir "secrets.el")))

;; Ensure this is defined even if its module is not loaded.
(unless (boundp '+default-minibuffer-maps)
  (defvar +default-minibuffer-maps
    (append '(minibuffer-local-map
              minibuffer-local-ns-map
              minibuffer-local-completion-map
              minibuffer-local-must-match-map
              minibuffer-local-isearch-map
              read-expression-map)
            (cond ((modulep! :completion ivy)
                   '(ivy-minibuffer-map
                     ivy-switch-buffer-map))
                  ((modulep! :completion helm)
                   '(helm-map
                     helm-rg-map
                     helm-read-file-map))))
    "A list of all the keymaps used for the minibuffer."))

;; This helps me debug issues with my config.
(setq persistent-scratch-autosave-mode t)
(setq persistent-scratch-what-to-save
      '(point narrowing text-properties))
(persistent-scratch-setup-default)

(require 'cae-lib)
(load! "lisp/cae-tty")
(load! "lisp/cae-bindings")
(load! "lisp/cae-multi")              ;Run parallel Emacs instances.
(load! "lisp/cae-smartparens")        ;Allow Smartparens to be disabled. This
                                        ;is also our Smartparens configuration.
(load! "lisp/cae-projectile")         ;Allow Projectile to be disabled. This
                                        ;is also our Projectile configuration.
(when (modulep! :editor evil)
  (after! evil
    (load! "lisp/cae-evil")))
(add-hook! 'exwm-init-hook
  (load! "lisp/cae-exwm"))

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
  (let ((fonts-to-check '(("Iosevka Comfy" doom-font)
                          ("IBM Plex Mono" doom-serif-font)
                          ("Iosevka Comfy Duo" doom-variable-pitch-font))))
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
(advice-add #'doom-docs-read-only-h :override #'ignore)

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
