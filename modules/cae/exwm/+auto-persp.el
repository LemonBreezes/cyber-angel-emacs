;;; lisp/exwm-auto-persp.el -*- lexical-binding: t; -*-

;; I wrote this code many years ago but I found it in one of my old archives and
;; it still works just fine.

(defvar cae-exwm-workspaces ()
  "The list of EXWM workspaces created up to now.")

(defvar cae-exwm-floating-apps '("..." "virtualbox" "discord" "main.py" "setup.tmp")
  "A list of class-names for EXWM applications which should stay floating.")

(defvar cae-exwm-workspace-name-replacements
  '(("google-chrome-unstable" . "Chrome")
    ("chromium-browser" . "Chrome")
    ("chromium" . "Chrome")
    ("Google-chrome" . "Chrome")
    ("Pavucontrol" . "Pavucontrol")
    ("tiled" . "Tiled")
    ("kitty" . "Kitty")
    ("mpv" . "MPV")
    ("firefox developer edition" . "Firefox")
    ("\"firefox developer edition\"" . "Firefox")
    ("\"firefoxdeveloperedition\"" . "Firefox")
    ("firefoxdeveloperedition" . "Firefox")
    ("firefox" . "Firefox")
    ("kdeconnect." . "KDE Connect")
    ("libreoffice" . "Libreoffice")
    ("libreoffice-startcenter" . "Libreoffice")
    ("Soffice" . "Libreoffice")
    ("libreoffice-writer" . "Libreoffice")
    ("libreoffice-calc" . "Libreoffice")
    ("libreoffice-base" . "Libreoffice")
    ("libreoffice-draw" . "Libreoffice")
    ("libreoffice-impress" . "Libreoffice")
    ("libreoffice-math" . "Libreoffice")
    ("libreoffice-writer" . "Libreoffice")
    ("wineboot.exe" . "Wine")
    ("control.exe" . "Wine")
    ("plover" . "Plover")
    (".blueman-manager-wrapped" . "Blueman")
    ("discord" . "Discord")
    ("com-azefsw-audioconnect-desktop-app-MainKt" . "AudioRelay")
    ("qutebrowser" . "Qutebrowser")
    ("signal beta" . "Signal")
    ("gnome-control-center" . "Gnome CC")
    ("microsoft teams" . "Teams")
    ("teams-for-linux" . "Teams")
    ("virtualbox" . "VirtualBox")
    ("virtualbox manager" . "VirtualBox")
    ("virtualboxvm" . "VirtualBox")
    ("virtualbox machine" . "VirtualBox")
    ("discord1" . "Discord")
    ("minecraft" . "Minecraft")
    ("snes9x-gtk" . "Snes9x")
    (".epsxe-wrapped" . "ePSXe")
    ("net-runelite-client-runelite" . "RuneLite")
    ("wow.exe" . "WoW")
    ("battle.net.exe" . "Battle.net")
    ("hakuneko-desktop" . "Hakuneko")
    ("runescape" . "RuneScape")
    ("Anydesk" . "Windows"))
  "An alist whose for which a key is an EXWM class name and a value is the name
of the corresponding workspace that will be created.")

(defun exwm--disable-floating ()
  "Tile the current application unless its class is in `cae-exwm-floating-apps'."
  (unless (or (not exwm--floating-frame)
              (member exwm-class-name cae-exwm-floating-apps))
    (exwm-floating--unset-floating exwm--id)))

(defun cae-exwm-get-workspace-name (buffer)
  "Get the name of the workspace assigned to the current buffer, or
nil if its not an EXWM buffer."
  (let ((class (buffer-local-value 'exwm-class-name buffer)))
    (alist-get class cae-exwm-workspace-name-replacements
               class nil #'cl-equalp)))

(defun cae-exwm-persp--after-match (buffer &rest _)
  "Creates workspace for a new EXWM buffer and switches to that workspace"
  (let* ((buffer (alist-get 'buffer state))
         (application-name (cae-exwm-get-workspace-name buffer)))
    (when (minibufferp nil t)
      (minibuffer-keyboard-quit))
    (when (not (string= application-name (+workspace-current-name)))
      (persp-remove-buffer buffer))
    (cl-pushnew application-name cae-exwm-workspaces :test #'string=)
    (+workspace-switch application-name t)
    (+workspace/display)
    (when (-some->> (doom-visible-buffers)
            (--first (string= (cae-exwm-get-workspace-name it) application-name))
            (get-buffer-window)
            (select-window))
      (switch-to-buffer buffer))
    (when (and (modulep! :ui popup)
               (+popup-window-p))
      (other-window 1)
      (switch-to-buffer buffer))
    (delete-other-windows)))

(defun cae-exwm-persp--get-name (state)
  "Gets the name of our new EXWM workspace."
  (setf (alist-get 'persp-name state)
        (cae-exwm-get-workspace-name (alist-get 'buffer state)))
  state)

(defun cae-exwm-persp--predicate (buffer &optional state)
  "Determines whether to create a workspace for this new EXWM buffer."
  (and (stringp (cae-exwm-get-workspace-name buffer))
       (not (and exwm--floating-frame
                 (cl-member (buffer-local-value 'exwm-class-name buffer)
                            cae-exwm-floating-apps
                            :test #'cl-equalp)))
       (or state t)))

(defun cae-exwm-persp--focus-workspace-app (&rest _)
  "Focuses the EXWM application assigned to our workspace, if any."
  (when (and (cl-member (+workspace-current-name)
                        cae-exwm-workspaces
                        :test #'cl-equalp)
             (or (not (boundp 'org-capture-mode))
                 (--none? (and (bufferp (window-buffer it))
                               (buffer-local-value 'org-capture-mode
                                                   (window-buffer it)))
                          (cl-union (and (modulep! :ui popup)
                                         (+popup-windows))
                                    (doom-visible-windows)))))
    (let ((app-buffer
           (--first (and (cl-equalp (cae-exwm-get-workspace-name it)
                                    (+workspace-current-name))
                         (string= (cae-exwm-get-workspace-name it)
                                  (persp-name (get-current-persp))))
                    (cl-union (+workspace-buffer-list)
                              (buffer-list)))))
      (unless (window-live-p (get-buffer-window app-buffer))
        (when (and (modulep! :ui popup)
                   (+popup-window-p))
          (other-window 1))
        (switch-to-buffer app-buffer)))))

(defun cae-exwm-persp-cleanup-workspace ()
  "Deletes the current EXWM workspace if it has no more EXWM
buffers of that class."
  (when-let* ((exwm-workspace-p (cl-member (+workspace-current-name)
                                           cae-exwm-workspaces
                                           :test #'cl-equalp))
              (workspace (cae-exwm-get-workspace-name (current-buffer))))
    (when (persp-p (persp-get-by-name workspace))
      (let ((buffers
             (--filter (and (buffer-live-p it)
                            (not (eq it (current-buffer)))
                            (string= (cae-exwm-get-workspace-name it) workspace))
                       (persp-buffers (persp-get-by-name workspace)))))
        (unless buffers
          (+workspace-kill (+workspace-current))
          (unless (string= (+workspace-current-name) +workspace--last)
            (+workspace/other)))))))

(add-hook 'exwm-floating-setup-hook #'exwm--disable-floating)

(defun cae-exwm-reload-workspaces ()
  "Reloads the EXWM workspaces."
  (interactive)
  (persp-def-auto-persp "EXWM"
                        :parameters '((dont-save-to-file . t))
                        :hooks '(exwm-manage-finish-hook)
                        :dyn-env '(after-switch-to-buffer-functions ;; prevent recursion
                                   (persp-add-buffer-on-find-file nil)
                                   persp-add-buffer-on-after-change-major-mode)
                        :switch 'window
                        :predicate #'cae-exwm-persp--predicate
                        :after-match #'cae-exwm-persp--after-match
                        :get-name #'cae-exwm-persp--get-name))

(unless (featurep '+exwm-auto-persp)
  (cae-exwm-reload-workspaces))

(advice-add #'+workspace-switch :after #'cae-exwm-persp--focus-workspace-app)

(defadvice! cae-exwm-browse-url-generic-a (&rest _)
  :before #'browse-url-generic
  (when-let* ((workspace
               (alist-get (string-join
                           (cl-find-if (lambda (l)
                                         (setq l (string-join l "-"))
                                         (alist-get l cae-exwm-workspace-name-replacements nil nil #'cl-equalp))
                                       (nreverse (cdr (-inits (string-split (file-name-base browse-url-generic-program) "-")))))
                           "-")
                          cae-exwm-workspace-name-replacements nil nil #'cl-equalp)))
    (+workspace-switch workspace t)
    (+workspace/display)))
(advice-add #'consult-gh-embark-open-in-browser :before #'cae-exwm-browse-url-generic-a)

(add-hook 'kill-buffer-hook #'cae-exwm-persp-cleanup-workspace)

(provide '+exwm-auto-persp)
