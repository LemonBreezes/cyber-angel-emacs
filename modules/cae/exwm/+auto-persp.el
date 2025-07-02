;;; cae/exwm/+auto-persp.el -*- lexical-binding: t; -*-

;; I wrote this code many years ago but I found it in one of my old archives and
;; it still works just fine.

(require 'dash)

(defvar cae-exwm-workspaces ()
  "The list of EXWM workspaces created up to now.")

(defvar cae-exwm-floating-apps
  '("..." "virtualbox" "discord" "main.py" "setup.tmp" "xclicker" "Soffice"
    "Xclicker" "SimpleScreenRecorder")
  "A list of class-names for EXWM applications which should stay floating.")

(defvar cae-exwm-floating-titles '("TigerVNC options")
  "A list of window titles for EXWM applications which should stay floating.")

(defvar cae-exwm-workspace-name-replacements
  '(("google-chrome-unstable" . "Google Chrome")
    ("chromium-browser" . "Chrome")
    ("chromium" . "Chrome")
    ("google-chrome" . "Google Chrome")
    ("Chromium-browser-chromium" . "Chrome")
    ("Cursor" . "Cursor")
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
    ("soffice" . "Libreoffice")
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
    ("runescape" . "RuneScape"))
  "Alist mapping EXWM class names to workspace names.
The key is the class name from EXWM and the value is the
name of the workspace that will be created for that application.")

(defvar cae-exwm-persp-loaded-p nil
  "Whether EXWM persp has been loaded.")

(defun exwm--disable-floating ()
  "Tile the current application unless its class is in `cae-exwm-floating-apps' or its title is in `cae-exwm-floating-titles'."
  (unless (or (not exwm--floating-frame)
              (member exwm-class-name cae-exwm-floating-apps)
              (cl-member exwm-title cae-exwm-floating-titles :test #'cl-equalp))
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
    (when-let* ((visible-buffers (doom-visible-buffers))
                (matching-buffer (cl-find-if (lambda (buffer)
                                               (string= (cae-exwm-get-workspace-name buffer) application-name))
                                             visible-buffers))
                (buffer-window (get-buffer-window matching-buffer)))
      (select-window buffer-window))
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
                 (or (cl-member (buffer-local-value 'exwm-class-name buffer)
                                cae-exwm-floating-apps
                                :test #'cl-equalp)
                     (cl-member (buffer-local-value 'exwm-title buffer)
                                cae-exwm-floating-titles
                                :test #'cl-equalp))))
       (or state t)))

(defun cae-exwm-persp--focus-workspace-app (&rest _)
  "Focuses the EXWM application assigned to our workspace, if any."
  (when (and (cl-member (+workspace-current-name)
                        cae-exwm-workspaces
                        :test #'cl-equalp)
             (or (not (boundp 'org-capture-mode))
                 (cl-notany (lambda (window)
                              (and (bufferp (window-buffer window))
                                   (buffer-local-value 'org-capture-mode
                                                       (window-buffer window))))
                            (cl-union (and (modulep! :ui popup)
                                           (+popup-windows))
                                      (doom-visible-windows)))))
    (let ((app-buffer
           (cl-find-if (lambda (buffer)
                         (and (cl-equalp (cae-exwm-get-workspace-name buffer)
                                         (+workspace-current-name))
                              (string= (cae-exwm-get-workspace-name buffer)
                                       (persp-name (get-current-persp)))))
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
             (cl-remove-if-not (lambda (buffer)
                                 (and (buffer-live-p buffer)
                                      (not (eq buffer (current-buffer)))
                                      (string= (cae-exwm-get-workspace-name buffer) workspace)))
                               (persp-buffers (persp-get-by-name workspace)))))
        (unless buffers
          (+workspace-kill (+workspace-current))
          (unless (string= (+workspace-current-name) +workspace--last)
            (+workspace/other)))))))

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

(unless cae-exwm-persp-loaded-p
  (cae-exwm-reload-workspaces)
  (setq cae-exwm-persp-loaded-p t))

(defun cae-exwm-find-workspace-for-program (program-name)
  "Find the appropriate workspace name for PROGRAM-NAME.
Returns nil if no matching workspace is found."
  (let* ((program-parts (string-split (file-name-base program-name) "-"))
         (possible-names (nreverse
                          (cl-loop for i from 1 to (length program-parts)
                                   collect (string-join 
                                            (cl-subseq program-parts 0 i) "-"))))
         (matching-key (cl-find-if
                        (lambda (name)
                          (assoc-string name cae-exwm-workspace-name-replacements t))
                        possible-names)))
    (when matching-key
      (alist-get matching-key cae-exwm-workspace-name-replacements nil nil #'cl-equalp))))
;; Use interactive function `cae-exwm-test-workspace-matching' to test this.

(defadvice! cae-exwm-browse-url-generic-a (&rest _)
  :before #'browse-url-generic
  (when-let* ((workspace (cae-exwm-find-workspace-for-program browse-url-generic-program)))
    (+workspace-switch workspace t)
    (+workspace/display)))

(advice-add #'consult-gh-embark-open-in-browser :before #'cae-exwm-browse-url-generic-a)
(add-hook 'exwm-floating-setup-hook #'exwm--disable-floating)
(add-hook! 'exwm-mode-hook
  (defun cae-exwm-persp-set-up-cleanup-hook-h ()
    (add-hook 'kill-buffer-hook #'cae-exwm-persp-cleanup-workspace nil t)))
(advice-add #'+workspace-switch :after #'cae-exwm-persp--focus-workspace-app)

(map! :map exwm-mode-map
      :localleader
      "b" #'cae-exwm-switch-to-workspace-buffer
      "n" #'cae-exwm-switch-next-workspace-buffer
      "p" #'cae-exwm-switch-previous-workspace-buffer)
