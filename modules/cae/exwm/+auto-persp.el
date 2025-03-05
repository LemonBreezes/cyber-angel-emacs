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

;;; Caching variables

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
  (and (stringp (cae-exwm-get-workspace-name buffer))
       (not (and exwm--floating-frame
                 (gethash (buffer-local-value 'exwm-class-name buffer)
                          cae-exwm--floating-apps-set)))
       (or state t)))

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
  (unless (gethash (+workspace-current-name) cae-exwm--workspace-names-set)
    (cl-return-from cae-exwm-persp-cleanup-workspace))

  (let ((workspace (cae-exwm-get-workspace-name (current-buffer))))
    (unless workspace
      (cl-return-from cae-exwm-persp-cleanup-workspace))

    (let ((persp (persp-get-by-name workspace)))
      (unless (persp-p persp)
        (cl-return-from cae-exwm-persp-cleanup-workspace))

      (unless (cae-exwm--get-matching-live-buffers workspace)
        (+workspace-kill (+workspace-current))
        (unless (string= (+workspace-current-name) +workspace--last)
          (+workspace/other))))))

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
