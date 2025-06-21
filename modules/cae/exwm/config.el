;;; cae/exwm/config.el -*- lexical-binding: t; -*-

(require 'cae-lib)

(defvar cae-exwm-enabled-p (and (eq 'x (framep (selected-frame)))
                                (not (or (getenv "INSIDE_EXWM")
                                         (getenv "RATPOISON")
                                         (getenv "I3SOCK")
                                         (getenv "KDE_FULL_SESSION")
                                         (getenv "GNOME_DESKTOP_SESSION_ID")
                                         (getenv "XDG_CURRENT_DESKTOP")
                                         (getenv "WAYLAND_DISPLAY"))))
  "Whether EXWM is enabled.")

(when cae-exwm-enabled-p
  (defun cae-exwm-rename-buffer-to-title ()
    "Rename the buffer to its `exwm-title'."
    (when (and (not (string-prefix-p "sun-awt-X11-" exwm-instance-name))
               (not (string= "gimp" exwm-instance-name ))
               (or (persp-contain-buffer-p (current-buffer) (get-current-persp))
                   (not (cl-loop for workspace in (+workspace-list)
                                 if (+workspace-contains-buffer-p
                                     (current-buffer) workspace)
                                 return t))))
      (exwm-workspace-rename-buffer exwm-title)))
  (add-hook 'exwm-update-title-hook #'cae-exwm-rename-buffer-to-title)

  (use-package! exwm
    :defer nil :config
    (when init-file-debug
      (exwm-debug +1))
    (exwm-randr-mode +1)
    (exwm-wm-mode +1)

    (cond ((modulep! :ui workspaces)
           (setq exwm-input-global-keys
                 `(;; Bind "s-r" to exit char-mode and fullscreen mode.
                   ([?\s-r] . exwm-reset)
                   ;; Bind "s-w" to switch workspace interactively.
                   ([?\s-w] . +workspace/switch-to)
                   ;; Bind "s-," and "s-." to EXWM workspace switching.
                   ([?\s-,] . cae-exwm-workspace-switch-previous)
                   ([?\s-.] . cae-exwm-workspace-switch-next)
                   ;; Bind "s-&" to launch applications ('M-&' also works if the
                   ;; output buffer does not bother you).
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
                   ;; Bind "s-&" to launch applications ('M-&' also works if the
                   ;; output buffer does not bother you).
                   ([?\s-&] . (lambda (command)
                                (interactive (list (read-shell-command "$ ")))
                                (start-process-shell-command command nil command)))
                   ,@(when (or (modulep! :cae helm)
                               (modulep! :completion helm))
                       '(([?\s-d] . helm-run-external-command)))))))

    ;; Update the above keys when reloading this file.
    (when (bound-and-true-p cae-config-finished-loading)
      (dolist (i exwm-input-global-keys)
        (exwm-input--set-key (car i) (cdr i))))

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

    ;; Testing out this fix to see if it works
    ;; https://github.com/ch11ng/exwm/issues/889#issuecomment-1874977844
    (setq x-no-window-manager t)

    ;; Testing out a recommendation from here:
    ;; https://github.com/emacs-exwm/exwm/issues/18#issuecomment-2030366532
    (setq exwm-layout-auto-iconify nil)

    ;; Using `helm-display-buffer-in-own-frame' causes EXWM to emit an error.
    (after! helm
      (when (eq helm-default-display-buffer-functions
                #'helm-display-buffer-in-own-frame)
        (setq helm-default-prompt-display-function #'helm-default-display-buffer)))

    ;; Nested Emacs sessions break when `exwm-xim' is used.
    ;; Configure emacs input methods in all X windows.
    ;; There are two ways to work around this:
    ;; 1. Either do not enable `exwm-xim'
    ;; 2. or compile Emacs `--without-xim'.
    ;; 3. You can also disable `xim' through `Xresources', though I have not
    ;;    tested this approach.
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

    ;; Disable mouse tracking support in LSP and Dap UIs. I don't use them and
    ;; they can cause problems with `repeat-mode'.
    (add-hook! 'repeat-mode-hook
      (defun cae-exwm-disable-mouse-tracking-h ()
        (advice-add #'lsp-ui-doc--setup-mouse :override #'ignore)
        (advice-add #'lsp-ui-doc--disable-mouse-on-prefix :override #'ignore)
        (advice-add #'dap-tooltip-update-mouse-motions-if-enabled :override
                    #'ignore)
        (remove-hook 'repeat-mode-hook #'cae-exwm-disable-mouse-tracking-h)))

    ;; Never suspend Emacs when using EXWM. Doing so locks up Emacs.
    (map! [remap suspend-frame] #'undefined)

    ;; Show EXWM buffers in buffer switching prompts.
    (add-hook 'exwm-mode-hook #'doom-mark-buffer-as-real-h)

    ;; My preference.
    (add-hook 'exwm-mode-hook #'hide-mode-line-mode)
    (defadvice! cae-exwm-handle-hide-mode-line-mode-a ()
      :before #'exwm-layout-show-mode-line
      (hide-mode-line-mode -1))

    (when (modulep! :ui popup)
      (cl-pushnew ?\C-` exwm-input-prefix-keys))
    (cl-pushnew ?\C-g exwm-input-prefix-keys)

    (after! tooltip
      (add-to-list 'tooltip-frame-parameters '(parent-frame . nil) t))

    ;; Remove invalid face errors
    (setq-hook! exwm-mode
      outline-minor-mode-highlight nil)

    (after! posframe
      (setq posframe-mouse-banish-function #'posframe-mouse-banish-simple)
      (define-advice posframe-show (:filter-return (frame) exwm-deparent)
        (set-frame-parameter frame 'parent-frame nil)
        frame))

    ;; Do not handle EXWM buffers.
    (set-popup-rule! "^\\*exwm" :ignore t)

    (when (modulep! :editor evil)
      (if (or (modulep! :completion helm)
              (modulep! :cae helm))
          (map! :leader :desc "Run external command" "$" #'helm-run-external-command)
        ;; The problem with `app-launcher' is that it only shows applications
        ;; with desktop files.
        (map! :leader :desc "Run external command" "$" #'app-launcher-run-app)))
    (map! :map exwm-mode-map
          :localleader
          (:prefix ("d" . "debug")
           :desc "Clear debug buffer" "l" #'xcb-debug:clear
           :desc "Insert mark into the debug buffer" "m" #'xcb-debug:mark
           :desc "Enable debug logging" "t" #'exwm-debug)
          :desc "Toggle fullscreen" "f" #'exwm-layout-toggle-fullscreen
          :desc "Hide floating window" "h" #'exwm-floating-hide
          :desc "Send next key" "q" #'exwm-input-send-next-key
          :desc "Toggle floating" "SPC" #'exwm-floating-toggle-floating
          :desc "Send escape" "e" (cmd! (exwm-evil-send-key 1 'escape))
          :desc "Toggle modeline" "m" #'exwm-layout-toggle-mode-line))

  ;; Fixes an error which locks up Emacs. This error is caused by a bad
  ;; interaction with Doom's hack for distinguishing `C-i' and `TAB'.
  (defun cae-exwm-input--translate-a (oldfun &rest args)
    (let ((key-translation-map
           (copy-keymap key-translation-map)))
      (define-key key-translation-map [?\C-i] nil)
      (apply oldfun args)))
  (cae-advice-add #'exwm-input--translate :around #'cae-exwm-input--translate-a)

  ;; See https://github.com/emacs-exwm/exwm/issues/18. Addresses a focus issue
  ;; with EXWM but can cause increased power consumption.
  (cae-defadvice! cae-exwm-disable-net-wm-state-hidden-a (id)
    :after #'exwm-layout--hide
    (when-let ((buffer (exwm--id->buffer id))
               (_ (buffer-live-p buffer)))
      (with-current-buffer buffer
        (setq exwm--ewmh-state
              (delq xcb:Atom:_NET_WM_STATE_HIDDEN
                    exwm--ewmh-state))
        (exwm-layout--set-ewmh-state id)
        (xcb:flush exwm--connection))))

  (use-package! exwm-mff
    :defer t :init (add-hook 'exwm-init-hook #'exwm-mff-mode))

  (when (modulep! :editor evil +everywhere)
    (after! evil
      (load! "+evil")))

  (when (modulep! :completion corfu)
    (cae-advice-add 'corfu--make-frame :around #'cae-advise-corfu-make-frame-with-monitor-awareness)
    (after! corfu
      (load! "+corfu")))

  (when (modulep! :completion vertico)
    (after! consult
      (add-to-list 'consult-preview-excluded-buffers
                   "vmware")))

  (seq-filter (lambda (buf) (buffer-match-p "vmware" buf))
              (buffer-list))

  (when (modulep! :ui workspaces)
    (cae-advice-add #'+workspace-switch :after #'cae-exwm-persp--focus-workspace-app)
    (cae-advice-add #'browse-url-generic :before #'cae-exwm-browse-url-generic-a)
    (cae-advice-add #'consult-gh-embark-open-in-browser :before #'cae-exwm-browse-url-generic-a)
    (after! persp-mode
      (load! "+auto-persp"))))

;;Local Variables:
;;eval: (unless (modulep! :cae exwm) (remove-hook 'write-file-functions #'eval-buffer t))
;;End:
