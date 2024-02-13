;;; private/exwm/config.el -*- lexical-binding: t; -*-

(when (and (eq 'x (framep (selected-frame)))
           (not (or (getenv "EXWM_RUNNING")
                    (getenv "RATPOISON")
                    (getenv "I3SOCK")
                    (getenv "KDE_FULL_SESSION")
                    (getenv "GNOME_DESKTOP_SESSION_ID")
                    (getenv "XDG_CURRENT_DESKTOP") ; Let me know if this one
                                        ; causes problems.
                    (getenv "WAYLAND_DISPLAY"))))

  ;; Prevent nested Emacs sessions from loading EXWM.
  (defun +exwm-flag-as-enabled () (setenv "EXWM_RUNNING" "true"))
  (add-hook 'doom-after-init-hook #'+exwm-flag-as-enabled)

  (defun +exwm-rename-buffer-to-title ()
    "Rename the buffer to its `exwm-title'."
    (when (and (not (string-prefix-p "sun-awt-X11-" exwm-instance-name))
               (not (string= "gimp" exwm-instance-name ))
               (or (persp-contain-buffer-p (current-buffer) (get-current-persp))
                   (not (cl-loop for workspace in (+workspace-list)
                                 if (+workspace-contains-buffer-p (current-buffer) workspace)
                                 return t))))
      (exwm-workspace-rename-buffer exwm-title)))
  (add-hook 'exwm-update-title-hook #'+exwm-rename-buffer-to-title)

  (use-package! exwm
    :config
    (require 'exwm-randr)
    (require 'exwm-systemtray)
    (exwm-randr-enable)

    (setq exwm-systemtray-height (line-pixel-height))
    (exwm-systemtray-enable)

    (cond ((modulep! :ui workspaces)
           (setq exwm-input-global-keys
                 `(;; Bind "s-r" to exit char-mode and fullscreen mode.
                   ([?\s-r] . exwm-reset)
                   ;; Bind "s-w" to switch workspace interactively.
                   ([?\s-w] . +workspace/switch-to)
                   ;; Bind "s-&" to launch applications ('M-&' also works if the output
                   ;; buffer does not bother you).
                   ([?\s-&] . (lambda (command)
                                (interactive (list (read-shell-command "$ ")))
                                (start-process-shell-command command nil command)))
                   ([?\s-1] . +workspace/switch-to-0)
                   ([?\s-2] . +workspace/switch-to-1)
                   ([?\s-3] . +workspace/switch-to-2)
                   ([?\s-4] . +workspace/switch-to-3)
                   ([?\s-5] . +workspace/switch-to-4)
                   ([?\s-6] . +workspace/switch-to-5)
                   ([?\s-7] . +workspace/switch-to-6)
                   ([?\s-8] . +workspace/switch-to-7)
                   ([?\s-9] . +workspace/switch-to-8)
                   ([?\s-0] . +workspace/switch-to-final))))
          (t
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
                   ,@(when (or (modulep! :cae helm)
                               (modulep! :completion helm))
                       '(([?\s-d] . helm-run-external-command)))))))

    (map! :map exwm-mode-map
          :localleader
          (:prefix ("d" . "debug")
           "l" #'xcb-debug:clearn
           "m" #'xcb-debug:mark
           "t" #'exwm-debug)
          "f" #'exwm-layout-toggle-fullscreen
          "h" #'exwm-floating-hide
          "q" #'exwm-input-send-next-key
          "SPC" #'exwm-floating-toggle-floating
          "m" #'exwm-layout-toggle-mode-line)

    ;; A few `ido' fixes.
    (use-package! exwm-config
      :config
      (exwm-config--fix/ido-buffer-window-other-frame))

    ;; Using `helm-display-buffer-in-own-frame' causes EXWM to emit an error.
    (after! helm
      (when (eq helm-default-display-buffer-functions
                #'helm-display-buffer-in-own-frame)
        (setq helm-default-prompt-display-function #'helm-default-display-buffer)))

    ;; I never use the <mouse-movement> commands and are being triggered in some
    ;; strange scenarios involving EXWM so it's easier for me to just turn that
    ;; off.
    (add-hook! 'post-command-hook
      (defun cae-disable-mouse-tracking-h ()
        (setq track-mouse nil)))

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


    ;; Show EXWM buffers in buffer switching prompts.
    (add-hook 'exwm-mode-hook #'doom-mark-buffer-as-real-h)

    ;; For people who run nested Emacs instances within EXWM.
    (setq exwm-replace (when (not (getenv "EXWM_RUNNING")) 'ask))

    (when (modulep! :ui popup)
      (cl-pushnew ?\C-` exwm-input-prefix-keys))
    (cl-pushnew ?\C-g exwm-input-prefix-keys)

    (after! tooltip
      (add-to-list 'tooltip-frame-parameters '(parent-frame . nil) t))

    ;; Prevent `window-live-p' and `frame-live-p' from producing errors when
    ;; their argument is nil.
    ;;(defun +exwm-select-window-a (oldfun window &rest args)
    ;;  (when window (apply oldfun window args)))
    ;;(advice-add #'select-window :around #'+exwm-select-window-a)

    ;; Remove invalid face errors
    (setq-hook! exwm-mode
      outline-minor-mode-highlight nil)

    (after! posframe
      (setq posframe-mouse-banish-function #'posframe-mouse-banish-simple)
      (define-advice posframe-show (:filter-return (frame) exwm-deparent)
        (set-frame-parameter frame 'parent-frame nil)
        frame)))

  ;; Fixes an error which locks up Emacs. This error is caused by a bad
  ;; interaction with Doom's hack for distinguishing `C-i' and `TAB'.
  ;;(defun +exwm-input--translate-a (oldfun &rest args)
  ;;  (let ((key-translation-map
  ;;         (copy-keymap key-translation-map)))
  ;;    (define-key key-translation-map [9] nil)
  ;;    (apply oldfun args)))
  ;;(advice-add #'exwm-input--translate :around #'+exwm-input--translate-a)

  (use-package! exwm-mff
    :defer t :init (add-hook 'exwm-init-hook #'exwm-mff-mode))

  (when (modulep! :editor evil +everywhere)
    (load! "+evil"))

  (when (modulep! :ui workspaces)
    (unless (bound-and-true-p cae-config-finished-loading)
      (load! "+auto-persp"))))

;;Local Variables:
;;eval: (unless (modulep! :cae exwm) (remove-hook 'write-file-functions #'eval-buffer t))
;;End:
