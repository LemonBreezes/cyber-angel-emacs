;;; lisp/exwm-auto-persp.el -*- lexical-binding: t; -*-

(after! (:all exwm dash persp-mode)
  (defvar +exwm-workspaces ()
    "The list of EXWM workspaces created up to now.")

  (defvar +exwm-floating-apps '("..." "virtualbox" "discord")
    "A list of class-names for EXWM applications which should stay floating.")

  (defvar +exwm-workspace-name-replacements '()
    "An alist whose for which a key is an EXWM class name and a value is the name
of the corresponding workspace that will be created.")

  (defun exwm--disable-floating ()
    "Tile the current application unless its class is in `+exwm-floating-apps'."
    (unless (member exwm-class-name +exwm-floating-apps)
      (exwm-floating--unset-floating exwm--id)))

  (defun +exwm-get-workspace-name (buffer)
    "Get the name of the workspace assigned to the current buffer, or
nil if its not an EXWM buffer."
    (let ((class (buffer-local-value 'exwm-class-name buffer)))
      (alist-get class +exwm-workspace-name-replacements
                 class nil #'cl-equalp)))

  (defun +exwm-persp--after-match (buffer &rest _)
    "Creates workspace for a new EXWM buffer and switches to that workspace"
    (let* ((buffer (alist-get 'buffer state))
           (application-name (+exwm-get-workspace-name buffer)))
      (when (minibufferp)
        (minibuffer-keyboard-quit))
      (when (not (string= application-name (+workspace-current-name)))
        (persp-remove-buffer buffer))
      (cl-pushnew application-name +exwm-workspaces :test #'string=)
      (+workspace-switch application-name t)
      (+workspace/display)
      (when (-some->> (doom-visible-buffers)
              (--first (string= (+exwm-get-workspace-name it) application-name))
              (get-buffer-window)
              (select-window))
        (switch-to-buffer buffer))
      (when (and (modulep! :ui popup)
                 (+popup-window-p))
        (other-window 1)
        (switch-to-buffer buffer))))

  (defun +exwm-persp--get-name (state)
    "Gets the name of our new EXWM workspace."
    (setf (alist-get 'persp-name state)
          (+exwm-get-workspace-name (alist-get 'buffer state)))
    state)

  (defun +exwm-persp--predicate (buffer &optional state)
    "Determines whether to create a workspace for this new EXWM buffer."
    (and (stringp (+exwm-get-workspace-name buffer))
         (not (cl-member (buffer-local-value 'exwm-class-name buffer)
                         +exwm-floating-apps
                         :test #'cl-equalp))
         (or state t)))

  (defun +exwm-persp--focus-workspace-app (&rest _)
    "Focuses the EXWM application assigned to our workspace, if any."
    (when (and (cl-member (+workspace-current-name)
                          +exwm-workspaces
                          :test #'cl-equalp)
               (or (not (boundp 'org-capture-mode))
                   (--none? (and (bufferp (window-buffer it))
                                 (buffer-local-value 'org-capture-mode
                                                     (window-buffer it)))
                            (cl-union (and (modulep! :ui popup)
                                           (+popup-windows))
                                      (doom-visible-windows)))))
      (let ((app-buffer
             (--first (and (cl-equalp (+exwm-get-workspace-name it)
                                      (+workspace-current-name))
                           (string= (+exwm-get-workspace-name it)
                                    (persp-name (get-current-persp))))
                      (cl-union (+workspace-buffer-list)
                                (buffer-list)))))
        (unless (window-live-p (get-buffer-window app-buffer))
          (if (and (modulep! :ui popup)
                     (+popup-window-p))
            (other-window 1))
          (switch-to-buffer app-buffer)))))

  (defun +exwm-persp-cleanup-workspace ()
    "Deletes the current EXWM workspace if it has no more EXWM
buffers of that class."
    (when-let ((exwm-workspace-p (cl-member (+workspace-current-name)
                                            +exwm-workspaces
                                            :test #'cl-equalp))
               (workspace (+exwm-get-workspace-name (current-buffer))))
      (when (persp-p (persp-get-by-name workspace))
        (let ((buffers
               (--filter (and (buffer-live-p it)
                              (not (eq it (current-buffer)))
                              (string= (+exwm-get-workspace-name it) workspace))
                         (persp-buffers (persp-get-by-name workspace)))))
          (unless buffers
            (+workspace-delete (+workspace-current))
            (+workspace/other))))))

  (add-hook 'exwm-floating-setup-hook #'exwm--disable-floating)

  (persp-def-auto-persp "EXWM"
                        :parameters '((dont-save-to-file . t))
                        :hooks '(exwm-manage-finish-hook)
                        :dyn-env '(after-switch-to-buffer-functions ;; prevent recursion
                                   (persp-add-buffer-on-find-file nil)
                                   persp-add-buffer-on-after-change-major-mode)
                        :switch 'window
                        :predicate #'+exwm-persp--predicate
                        :after-match #'+exwm-persp--after-match
                        :get-name #'+exwm-persp--get-name)

  (advice-add #'+workspace-switch :after #'+exwm-persp--focus-workspace-app)

  (add-hook! 'exwm-mode-hook
    (add-hook 'kill-buffer-hook #'+exwm-persp-cleanup-workspace)))
