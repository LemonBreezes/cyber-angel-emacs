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

(defconst cae-exwm-floating-apps '("..." "discord" "main.py" "setup.tmp")
  "List of EXWM class names for applications that should remain floating.")

(defconst cae-exwm-workspace-name-replacements
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
    ("runescape" . "RuneScape"))
  "Alist mapping EXWM class names to workspace names.
The key is the class name from EXWM and the value is the
name of the workspace that will be created for that application.")

;;; Debugging

(defvar cae-exwm-auto-persp-debug nil
  "When non-nil, enable debugging output for EXWM auto-persp functionality.")

;;; Caching variables

;; Pre-allocate hash tables with appropriate sizes
(defvar cae-exwm--workspace-name-cache
  (let ((ht (make-hash-table :test 'equal :size 64)))
    (dolist (mapping cae-exwm-workspace-name-replacements)
      (puthash (car mapping) (cdr mapping) ht))
    ht)
  "Cache for workspace names to avoid repeated lookups.")

(defvar cae-exwm--browser-workspace-cache-table (make-hash-table :test 'equal :size 12)
  "Cache table for browser workspace names indexed by browser program.")

;; Create a set for faster membership testing
(defvar cae-exwm--floating-apps-set
  (let ((set (make-hash-table
              :test 'equal
              :size (expt 2 (ceiling (log (length cae-exwm-floating-apps) 2))))))
    (dolist (app cae-exwm-floating-apps)
      (puthash app t set))
    set)
  "Hash table for fast lookup of floating apps.")

(defvar cae-exwm--auto-persp-initialized nil
  "Flag to track if auto-persp has been initialized.")

(defvar cae-exwm--workspace-names-set (make-hash-table :test 'equal :size 30)
  "Hash table for fast lookup of workspace names.")

;;; Core functions

(defsubst cae-exwm-get-workspace-name (buffer)
  "Get the workspace name for BUFFER based on its EXWM class.
Returns nil if BUFFER is not an EXWM buffer."
  (let ((class (buffer-local-value 'exwm-class-name buffer)))
    (gethash class cae-exwm--workspace-name-cache class)))

(defsubst cae-exwm--disable-floating ()
  "Tile the current application unless its class is in `cae-exwm-floating-apps'."
  (unless (or (not exwm--floating-frame)
              (gethash exwm-class-name cae-exwm--floating-apps-set))
    (exwm-floating--unset-floating exwm--id)))

(defun cae-exwm-clear-caches ()
  "Clear all EXWM caches."
  (interactive)
  (clrhash cae-exwm--workspace-name-cache)
  (clrhash cae-exwm--browser-workspace-cache-table)
  (clrhash cae-exwm--workspace-names-set)

  ;; Rebuild the workspace name cache
  (dolist (mapping cae-exwm-workspace-name-replacements)
    (puthash (car mapping) (cdr mapping) cae-exwm--workspace-name-cache)))

;;; Workspace management

(defsubst cae-exwm-persp--predicate (buffer &optional state)
  "Determine whether to create a workspace for BUFFER.
Returns non-nil if a dedicated workspace should be created.
Optional STATE is passed from persp-mode."
  ;;(+log (buffer-local-value 'exwm-class-name buffer)
  ;;      (gethash (buffer-local-value 'exwm-class-name buffer)
  ;;               cae-exwm--floating-apps-set)
  ;;      (cae-exwm-get-workspace-name buffer))
  (let* ((class-name (buffer-local-value 'exwm-class-name buffer))
         (workspace-name (cae-exwm-get-workspace-name buffer))
         (is-floating (and exwm--floating-frame
                          (gethash class-name cae-exwm--floating-apps-set)))
         (result (and (stringp workspace-name)
                     ;; Special case for VirtualBox applications
                     (not is-floating)
                     (or state t))))
    
    (when cae-exwm-auto-persp-debug
      (message "[EXWM-DEBUG] Predicate for %s (class: %s): workspace-name=%s, is-floating=%s, result=%s"
               (buffer-name buffer)
               class-name
               workspace-name
               is-floating
               result))
    
    result))

(defsubst cae-exwm-persp--get-name (state)
  "Get the name for a new EXWM workspace from STATE."
  (setf (alist-get 'persp-name state)
        (cae-exwm-get-workspace-name (alist-get 'buffer state)))
  state)

(defun cae-exwm--select-application-window (application-name buffer)
  "Select window for APPLICATION-NAME and BUFFER."
  (let ((found nil)
        (visible-buffers (doom-visible-buffers)))
    ;; Try to find and select an existing window first - use faster loop
    (catch 'found-window
      (dolist (buf visible-buffers)
        (when (string= (cae-exwm-get-workspace-name buf) application-name)
          (when-let ((win (get-buffer-window buf)))
            (select-window win)
            (setq found t)
            (throw 'found-window t)))))

    ;; If no window found, handle popup case
    (unless found
      (when (and (modulep! :ui popup)
                 (+popup-window-p))
        (other-window 1)
        (switch-to-buffer buffer)))

    (delete-other-windows)))

(defun cae-exwm-persp--after-match (state &rest _)
  "Create and switch to a workspace for a new EXWM BUFFER."
  (let* ((buffer (alist-get 'buffer state))
         (application-name (cae-exwm-get-workspace-name buffer)))
    ;; Exit minibuffer if active
    (when (minibufferp nil t)
      (minibuffer-keyboard-quit))

    ;; Handle workspace switching
    (when (not (string= application-name (+workspace-current-name)))
      (persp-remove-buffer buffer))

    ;; Use hash table for faster membership testing
    (unless (gethash application-name cae-exwm--workspace-names-set)
      (puthash application-name t cae-exwm--workspace-names-set)
      (push application-name cae-exwm-workspaces))

    (+workspace-switch application-name t)
    (+workspace/display)

    ;; Select appropriate window
    (cae-exwm--select-application-window application-name buffer)))

(defun cae-exwm--no-org-capture-active-p ()
  "Return non-nil if no org-capture is currently active in any visible window."
  (or (not (boundp 'org-capture-mode))
      (catch 'found-capture
        (let ((popup-windows (and (modulep! :ui popup) (+popup-windows))))
          (dolist (window (if popup-windows
                              (append popup-windows (doom-visible-windows))
                            (doom-visible-windows)))
            (when (and (bufferp (window-buffer window))
                       (buffer-local-value 'org-capture-mode (window-buffer window)))
              (throw 'found-capture nil))))
        t)))

(defun cae-exwm--find-matching-buffer ()
  "Find the EXWM buffer matching the current workspace."
  (let ((current-workspace-name (+workspace-current-name))
        (current-persp-name (persp-name (get-current-persp))))
    ;; First try workspace buffers which is usually smaller
    (or (catch 'found
          (dolist (buffer (+workspace-buffer-list))
            (let ((ws-name (cae-exwm-get-workspace-name buffer)))
              (when (and (cl-equalp ws-name current-workspace-name)
                         (string= ws-name current-persp-name))
                (throw 'found buffer)))))
        ;; Fall back to all buffers if not found
        (catch 'found
          (dolist (buffer (buffer-list))
            (let ((ws-name (cae-exwm-get-workspace-name buffer)))
              (when (and (cl-equalp ws-name current-workspace-name)
                         (string= ws-name current-persp-name))
                (throw 'found buffer))))))))

(defun cae-exwm-persp--focus-workspace-app (&rest _)
  "Focus the EXWM application assigned to the current workspace."
  (when (and (gethash (+workspace-current-name) cae-exwm--workspace-names-set)
             (cae-exwm--no-org-capture-active-p))
    (let ((app-buffer (cae-exwm--find-matching-buffer)))
      (when (and app-buffer (not (window-live-p (get-buffer-window app-buffer))))
        (when (and (modulep! :ui popup)
                   (+popup-window-p))
          (other-window 1))
        (switch-to-buffer app-buffer)))))

(defun cae-exwm--get-matching-live-buffers (workspace)
  "Get live buffers matching WORKSPACE excluding current buffer."
  (let ((current-buffer (current-buffer))
        (result nil))
    (dolist (buffer (persp-buffers (persp-get-by-name workspace)) result)
      (when (and (buffer-live-p buffer)
                 (not (eq buffer current-buffer))
                 (string= (cae-exwm-get-workspace-name buffer) workspace))
        (push buffer result)))))

(cl-defun cae-exwm-persp-cleanup-workspace ()
  "Delete the current EXWM workspace if it has no more EXWM buffers of that class."
  (when cae-exwm-auto-persp-debug
    (message "[EXWM-DEBUG] Cleanup called for workspace: %s, buffer: %s"
             (+workspace-current-name)
             (buffer-name (current-buffer))))
  
  (unless (gethash (+workspace-current-name) cae-exwm--workspace-names-set)
    (when cae-exwm-auto-persp-debug
      (message "[EXWM-DEBUG] Workspace %s not in workspace-names-set, skipping cleanup"
               (+workspace-current-name)))
    (cl-return-from cae-exwm-persp-cleanup-workspace))

  (let ((workspace (cae-exwm-get-workspace-name (current-buffer))))
    (unless workspace
      (when cae-exwm-auto-persp-debug
        (message "[EXWM-DEBUG] No workspace name for current buffer, skipping cleanup"))
      (cl-return-from cae-exwm-persp-cleanup-workspace))

    (let ((persp (persp-get-by-name workspace)))
      (unless (persp-p persp)
        (when cae-exwm-auto-persp-debug
          (message "[EXWM-DEBUG] No perspective found for workspace %s, skipping cleanup" workspace))
        (cl-return-from cae-exwm-persp-cleanup-workspace))

      (let ((matching-buffers (cae-exwm--get-matching-live-buffers workspace)))
        (when cae-exwm-auto-persp-debug
          (message "[EXWM-DEBUG] Matching buffers for workspace %s: %s"
                   workspace
                   (mapcar #'buffer-name matching-buffers)))
        
        (unless matching-buffers
          (when cae-exwm-auto-persp-debug
            (message "[EXWM-DEBUG] No matching buffers, killing workspace %s" workspace))
          (+workspace-kill (+workspace-current))
          (unless (string= (+workspace-current-name) +workspace--last)
            (+workspace/other)))))))

;;; URL handling

;; Pre-compute browser name parts for common browsers
(defvar cae-exwm--browser-name-parts-cache (make-hash-table :test 'equal :size 10)
  "Cache for browser name parts to avoid repeated splitting.")

(defun cae-exwm--get-browser-workspace-name ()
  "Get the workspace name for the current browser."
  (when browse-url-generic-program
    (or (gethash browse-url-generic-program cae-exwm--browser-workspace-cache-table)
        (let* ((browser-name (file-name-base browse-url-generic-program))
               (browser-parts (or (gethash browser-name cae-exwm--browser-name-parts-cache)
                                  (let ((parts (string-split browser-name "-")))
                                    (puthash browser-name parts cae-exwm--browser-name-parts-cache)
                                    parts)))
               (browser-combinations
                (nreverse
                 (cdr
                  (cl-loop for i from 1 to (length browser-parts)
                           collect (cl-subseq browser-parts 0 i)))))
               (workspace-name nil))

          ;; Use faster loop with early return
          (catch 'found
            (dolist (parts browser-combinations)
              (let* ((browser-key (string-join parts "-"))
                     (name (gethash browser-key cae-exwm--workspace-name-cache)))
                (when name
                  (setq workspace-name name)
                  (throw 'found t)))))

          ;; Cache the result for future lookups
          (puthash browse-url-generic-program workspace-name cae-exwm--browser-workspace-cache-table)
          workspace-name))))

(defun cae-exwm-browse-url-generic-a (&rest _)
  "Switch to the appropriate workspace before opening a URL."
  (when-let ((workspace (cae-exwm--get-browser-workspace-name)))
    (+workspace-switch workspace t)
    (+workspace/display)))

;;; Setup and initialization

(defun cae-exwm-reload-workspaces ()
  "Reload the EXWM workspaces configuration."
  (interactive)
  (cae-exwm-clear-caches)

  ;; Rebuild workspace names set
  (dolist (name cae-exwm-workspaces)
    (puthash name t cae-exwm--workspace-names-set))

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
  (unless cae-exwm--auto-persp-initialized
    ;; Ensure caches are initialized
    (cae-exwm-clear-caches)

    ;; Set up hooks
    (add-hook 'exwm-floating-setup-hook #'cae-exwm--disable-floating)
    (add-hook 'kill-buffer-hook #'cae-exwm-persp-cleanup-workspace)

    ;; Initialize workspaces
    (cae-exwm-reload-workspaces)

    (setq cae-exwm--auto-persp-initialized t)))

;; Initialize if not already loaded
(unless (featurep 'cae-exwm-auto-persp)
  (cae-exwm-setup-auto-persp))

(provide 'cae-exwm-auto-persp)
;;; cae/exwm/+auto-persp.el ends here
