;;; private/exwm/config.el -*- lexical-binding: t; -*-

;; ;; Do not load EXWM when we are in SSH-X11.
(when (getenv "SSH_TTY")
  (setenv "EXWM_RUNNING" "true"))

(when (and (eq 'x (framep (selected-frame)))
           (not (getenv "EXWM_RUNNING")))

  ;; Prevent nested Emacs sessions from loading EXWM.
  (defun +exwm-flag-as-enabled () (setenv "EXWM_RUNNING" "true"))
  (add-hook 'doom-first-init-hook #'+exwm-flag-as-enabled)

  (defvar +exwm-vanilla-emacs-config-file
    (expand-file-name "./exwm-vanilla-emacs-default-config.el" (doom-module-locate-path :private 'exwm))
    "The configuration loaded in our nested vanilla Emacs sessions.
The configuration must be a single symbolic expression as it is
handed off to. A macro syntax is valid. Macro
expansion occurs within the parent Emacs session.")
  (defvar +exwm-vanilla-emacs-theme
    (lambda ()
      (if (memq doom-theme '(modus-operandi modus-vivendi))
          doom-theme
        'wheatgrass))
    "The theme loaded in our vanilla Emacs child sessions.")

  (defun +exwm-rename-buffer-to-title ()
    "Rename the buffer to its `exwm-title'."
    (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                (string= "gimp" exwm-instance-name))
      (exwm-workspace-rename-buffer exwm-title)))
  (add-hook 'exwm-update-title-hook #'+exwm-rename-buffer-to-title)

  (use-package! exwm
    :config
    (require 'exwm-randr)
    (require 'exwm-systemtray)
    (exwm-randr-enable)

    (setq! exwm-systemtray-height (line-pixel-height))
    (exwm-systemtray-enable)

    (unless (modulep! :ui workspaces)
      (setq exwm-workspace-number 4)
      (setq exwm-input-global-keys
            `(;; Bind "s-r" to exit char-mode and fullscreen mode.
              ([?\s-r] . exwm-reset)
              ;; Bind "s-w" to switch workspace interactively.
              ([?\s-w] . exwm-workspace-switch)
              ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
              ,@(mapcar (lambda (i)
                          `(,(kbd (format "s-%d" i)) .
                            (lambda ()
                              (interactive)
                              (exwm-workspace-switch-create ,i))))
                        (number-sequence 0 9))
              ;; Bind "s-&" to launch applications ('M-&' also works if the output
              ;; buffer does not bother you).
              ([?\s-&] . (lambda (command)
		           (interactive (list (read-shell-command "$ ")))
		           (start-process-shell-command command nil command)))
              ([?\s-1] . (lambda ()
                           (interactive)
                           (exwm-workspace-switch-create 0)))
              ([?\s-2] . (lambda ()
                           (interactive)
                           (exwm-workspace-switch-create 1)))
              ([?\s-3] . (lambda ()
                           (interactive)
                           (exwm-workspace-switch-create 2)))
              ([?\s-4] . (lambda ()
                           (interactive)
                           (exwm-workspace-switch-create 3)))
              ([?\s-5] . (lambda ()
                           (interactive)
                           (exwm-workspace-switch-create 4)))
              ([?\s-6] . (lambda ()
                           (interactive)
                           (exwm-workspace-switch-create 5)))
              ([?\s-7] . (lambda ()
                           (interactive)
                           (exwm-workspace-switch-create 6)))
              ([?\s-8] . (lambda ()
                           (interactive)
                           (exwm-workspace-switch-create 7)))
              ([?\s-9] . (lambda ()
                           (interactive)
                           (exwm-workspace-switch-create 8)))
              ([?\s-0] . (lambda ()
                           (interactive)
                           (exwm-workspace-switch-create 9))))))


    ;; A few `ido' fixes.
    (use-package! exwm-config
      :config
      (exwm-config--fix/ido-buffer-window-other-frame))

    ;; Using `helm-display-buffer-in-own-frame' causes EXWM to emit an error.
    (after! helm
      (when (eq helm-default-display-buffer-functions
                #'helm-display-buffer-in-own-frame)
        (setq! helm-default-prompt-display-function #'helm-default-display-buffer)))

    ;; Nested Emacs sessions break when `exwm-xim' is used.
    ;; Configure emacs input methods in all X windows.
    ;; There are two ways to work around this:
    ;; 1. Either do not enable `exwm-xim'
    ;; 2. or compile Emacs `--without-xim'.
    ;; 3. You can also disable `xim' through `Xresources', though I have not tested this approach.
    ;; (use-package! exwm-xim
    ;;   :config
    ;;   ;; These variables are required for X programs to connect with XIM.
    ;;   (setenv "XMODIFIERS" "@im=exwm-xim")
    ;;   (setenv "GTK_IM_MODULE" "xim")
    ;;   (setenv "QT_IM_MODULE" "xim")
    ;;   (setenv "CLUTTER_IM_MODULE" "xim")
    ;;   (setenv "QT_QPA_PLATFORM" "xcb")
    ;;   (setenv "SDL_VIDEODRIVER" "x11")
    ;;   (exwm-xim-enable))

    (exwm-enable)

    ;; Never suspend Emacs when using EXWM. Doing so locks up Emacs.
    (map! [remap suspend-frame] #'undefined)
    (advice-add #'suspend-frame :override #'ignore)

    ;; Prevent EXWM buffers from changing their name while not focused.
    ;; This allows Persp to restore them as intended.
    (when (modulep! :ui workspaces)
      (defun +exwm-rename-buffer-to-title ()
        "Rename the buffer to its `exwm-title'."
        (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                    (string= "gimp" exwm-instance-name))
          (exwm-workspace-rename-buffer exwm-title)))
      (advice-add #'exwm--update-utf8-title :around
                  #'exwm--update-utf8-title-advice))

    ;; Show EXWM buffers in buffer switching prompts.
    (add-hook 'exwm-mode-hook #'doom-mark-buffer-as-real-h)

    ;; Set up key translations for using Emacs keybindings in X windows.
    (unless (modulep! :editor evil)
      (setq exwm-input-simulation-keys
            '(([?\C-p] . [up])
              ([?\C-n] . [down])
              ([?\M-w] . [?\C-c])
              ([?\C-y] . [?\C-v])
              ([?\C-x ?h] . [?\C-a])
              ([?\C-e] . [end])
              ([?\C-a] . [home])
              ([?\C-v] . [next])
              ([?\M-v] . [prior])
              ([?\C-d] . [delete])
              ([?\C-s] . [?\C-k])
              ([?\C-k] . [S-end delete])
              ([?\C-w] . [?\C-x])       ; Use C-F4 to close tabs in Firefox.
              ([?\C-f] . [right])       ; Use F3 to search in Firefox.
              ([?\C-x ?\C-s] . [?\C-s])
              ([?\C-f] . [right])
              ([?\C-b] . [left])
              ([?\M-b] . [C-left])
              ([?\M-f] . [C-right])
              ([?\M-d] . [C-delete])))
      (map! :map exwm-mode-map
            "C-c C-u" (cmd! (exwm-input--fake-key ?\C-a)
                            (exwm-input--fake-key 'backspace))))

    ;; For people who run nested Emacs instances within EXWM.
    (setq! exwm-replace nil)

    (when (modulep! :ui popup)
      (cl-pushnew ?\C-` exwm-input-prefix-keys))
    (cl-pushnew ?\C-g exwm-input-prefix-keys)

    (after! tooltip
      (add-to-list 'tooltip-frame-parameters '(parent-frame . nil) t))

    ;; Prevent `window-live-p' and `frame-live-p' from producing errors when
    ;; their argument is nil.
    (defun +exwm-select-window-a (oldfun window &rest args)
      (when window (apply oldfun window args)))
    (advice-add #'select-window :around #'+exwm-select-window-a)

    ;; Remove invalid face errors
    (setq-hook! exwm-mode
      outline-minor-mode-highlight nil))

  (use-package! exwm-edit
    :commands exwm-edit--compose
    :init
    (map! :map exwm-mode-map
          "C-c '" #'exwm-edit--compose)
    :config
    (setq! exwm-edit-split "below"))

  ;; Fixes an error which locks up Emacs. This error is caused by a bad
  ;; interaction with Doom's hack for distinguishing `C-i' and `TAB'.
  (defun +exwm-input--translate-a (oldfun &rest args)
    (let ((key-translation-map
           (copy-keymap key-translation-map)))
      (define-key key-translation-map [9] nil)
      (apply oldfun args)))
  (advice-add #'exwm-input--translate :around #'+exwm-input--translate-a)

  (use-package! exwm-mff
    :defer t
    :init (add-hook 'exwm-init-hook #'exwm-mff-mode)))
