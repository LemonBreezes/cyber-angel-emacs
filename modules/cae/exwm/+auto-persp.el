;;; cae/exwm/+auto-persp.el --- EXWM workspace management -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module provides automatic workspace management for EXWM applications.
;; It creates dedicated workspaces for different applications and manages
;; their lifecycle.
;;
;;; Code:

(require 'cl-lib)
(require 'exwm)
(require 'persp-mode)

;;; Configuration variables

(defvar cae-exwm-workspaces ()
  "List of EXWM workspace names that have been created.")

(defvar cae-exwm-floating-apps '("..." "virtualbox" "discord" "main.py" "setup.tmp")
  "List of EXWM class names for applications that should remain floating.")

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
  "Alist mapping EXWM class names to workspace names.
The key is the class name from EXWM and the value is the
name of the workspace that will be created for that application.")

;;; Core functions

(defun cae-exwm-get-workspace-name (buffer)
  "Get the workspace name for BUFFER based on its EXWM class.
Returns nil if BUFFER is not an EXWM buffer."
  (let ((class (buffer-local-value 'exwm-class-name buffer)))
    (alist-get class cae-exwm-workspace-name-replacements
               class nil #'cl-equalp)))

(defun cae-exwm--disable-floating ()
  "Tile the current application unless its class is in `cae-exwm-floating-apps'."
  (unless (or (not exwm--floating-frame)
              (member exwm-class-name cae-exwm-floating-apps))
    (exwm-floating--unset-floating exwm--id)))

;;; Workspace management

(defun cae-exwm-persp--predicate (buffer &optional state)
  "Determine whether to create a workspace for BUFFER.
Returns non-nil if a dedicated workspace should be created.
Optional STATE is passed from persp-mode."
  (and (stringp (cae-exwm-get-workspace-name buffer))
       (not (and exwm--floating-frame
                 (cl-member (buffer-local-value 'exwm-class-name buffer)
                            cae-exwm-floating-apps
                            :test #'cl-equalp)))
       (or state t)))

(defun cae-exwm-persp--get-name (state)
  "Get the name for a new EXWM workspace from STATE."
  (setf (alist-get 'persp-name state)
        (cae-exwm-get-workspace-name (alist-get 'buffer state)))
  state)

(defun cae-exwm--select-application-window (application-name buffer)
  "Select window for APPLICATION-NAME and BUFFER."
  (when-let* ((visible-buffers (doom-visible-buffers))
              (matching-buffer (cl-find-if 
                               (lambda (buf)
                                 (string= (cae-exwm-get-workspace-name buf) 
                                          application-name))
                               visible-buffers))
              (buffer-window (get-buffer-window matching-buffer)))
    (select-window buffer-window))
  
  (when (and (modulep! :ui popup)
             (+popup-window-p))
    (other-window 1)
    (switch-to-buffer buffer))
  
  (delete-other-windows))

(defun cae-exwm-persp--after-match (buffer &rest _)
  "Create and switch to a workspace for a new EXWM BUFFER."
  (let* ((buffer (alist-get 'buffer state))
         (application-name (cae-exwm-get-workspace-name buffer)))
    ;; Exit minibuffer if active
    (when (minibufferp nil t)
      (minibuffer-keyboard-quit))
    
    ;; Handle workspace switching
    (when (not (string= application-name (+workspace-current-name)))
      (persp-remove-buffer buffer))
    (cl-pushnew application-name cae-exwm-workspaces :test #'string=)
    (+workspace-switch application-name t)
    (+workspace/display)
    
    ;; Select appropriate window
    (cae-exwm--select-application-window application-name buffer)))

(defun cae-exwm--no-org-capture-active-p ()
  "Return non-nil if no org-capture is currently active in any visible window."
  (or (not (boundp 'org-capture-mode))
      (cl-notany (lambda (window)
                   (and (bufferp (window-buffer window))
                        (buffer-local-value 'org-capture-mode
                                           (window-buffer window))))
                 (cl-union (and (modulep! :ui popup)
                                (+popup-windows))
                           (doom-visible-windows)))))

(defun cae-exwm--find-matching-buffer ()
  "Find the EXWM buffer matching the current workspace."
  (cl-find-if (lambda (buffer)
                (and (cl-equalp (cae-exwm-get-workspace-name buffer)
                                (+workspace-current-name))
                     (string= (cae-exwm-get-workspace-name buffer)
                              (persp-name (get-current-persp)))))
              (cl-union (+workspace-buffer-list)
                        (buffer-list))))

(defun cae-exwm-persp--focus-workspace-app (&rest _)
  "Focus the EXWM application assigned to the current workspace."
  (when (and (cl-member (+workspace-current-name)
                        cae-exwm-workspaces
                        :test #'cl-equalp)
             (cae-exwm--no-org-capture-active-p))
    (let ((app-buffer (cae-exwm--find-matching-buffer)))
      (when (and app-buffer (not (window-live-p (get-buffer-window app-buffer))))
        (when (and (modulep! :ui popup)
                   (+popup-window-p))
          (other-window 1))
        (switch-to-buffer app-buffer)))))

(defun cae-exwm--get-matching-live-buffers (workspace)
  "Get live buffers matching WORKSPACE excluding current buffer."
  (cl-remove-if-not 
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (not (eq buffer (current-buffer)))
          (string= (cae-exwm-get-workspace-name buffer) workspace)))
   (persp-buffers (persp-get-by-name workspace))))

(defun cae-exwm-persp-cleanup-workspace ()
  "Delete the current EXWM workspace if it has no more EXWM buffers of that class."
  (when-let* ((exwm-workspace-p (cl-member (+workspace-current-name)
                                           cae-exwm-workspaces
                                           :test #'cl-equalp))
              (workspace (cae-exwm-get-workspace-name (current-buffer))))
    (when (persp-p (persp-get-by-name workspace))
      (let ((buffers (cae-exwm--get-matching-live-buffers workspace)))
        (unless buffers
          (+workspace-kill (+workspace-current))
          (unless (string= (+workspace-current-name) +workspace--last)
            (+workspace/other)))))))

;;; URL handling

(defun cae-exwm--get-browser-workspace-name ()
  "Get the workspace name for the current browser."
  (when-let* ((browser-parts (string-split (file-name-base browse-url-generic-program) "-"))
              (browser-combinations 
               (nreverse 
                (cdr
                 (cl-loop for i from 1 to (length browser-parts)
                          collect (cl-subseq browser-parts 0 i)))))
              (matching-combo
               (cl-find-if 
                (lambda (parts)
                  (let ((browser-name (string-join parts "-")))
                    (alist-get browser-name 
                              cae-exwm-workspace-name-replacements 
                              nil nil #'cl-equalp)))
                browser-combinations))
              (browser-key (string-join matching-combo "-"))
              (workspace (alist-get browser-key 
                                   cae-exwm-workspace-name-replacements 
                                   nil nil #'cl-equalp)))
    workspace))

(defun cae-exwm-browse-url-generic-a (&rest _)
  "Switch to the appropriate workspace before opening a URL."
  (when-let ((workspace (cae-exwm--get-browser-workspace-name)))
    (+workspace-switch workspace t)
    (+workspace/display)))

;;; Setup and initialization

(defun cae-exwm-reload-workspaces ()
  "Reload the EXWM workspaces configuration."
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

(defun cae-exwm-setup-auto-persp ()
  "Set up EXWM workspace management."
  ;; Set up hooks
  (add-hook 'exwm-floating-setup-hook #'cae-exwm--disable-floating)
  (add-hook 'kill-buffer-hook #'cae-exwm-persp-cleanup-workspace)
  
  ;; Set up advice
  (advice-add #'+workspace-switch :after #'cae-exwm-persp--focus-workspace-app)
  (advice-add #'browse-url-generic :before #'cae-exwm-browse-url-generic-a)
  (advice-add #'consult-gh-embark-open-in-browser :before #'cae-exwm-browse-url-generic-a)
  
  ;; Initialize workspaces
  (cae-exwm-reload-workspaces))

;; Initialize if not already loaded
(unless (featurep 'cae-exwm-auto-persp)
  (cae-exwm-setup-auto-persp))

(provide 'cae-exwm-auto-persp)
;;; cae/exwm/+auto-persp.el ends here
