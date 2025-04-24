;;; cae/exwm/autoload/auto-persp.el -*- lexical-binding: t; -*-

(require 'persp-mode)

;;;###autoload
(defun cae-exwm-browse-url-generic-a (&rest _)
  "Switch to the appropriate workspace before opening a URL."
  (when-let ((workspace cae-generic-browser-name))
    (+workspace-switch workspace t)
    (+workspace/display)))

;;;###autoload
(defun cae-exwm-switch-to-workspace-buffer ()
  "Switch to the EXWM buffer associated with the current workspace, if any.
Otherwise, fall back to the command mapped to `persp-switch-to-buffer'."
  (interactive)
  (let ((current-ws-name (+workspace-current-name)))
    (if (member current-ws-name cae-exwm-workspaces)
        ;; EXWM Workspace: Interactive selection filtered to matching EXWM buffers
        (let* ((filter-fn (lambda (buf)
                            (and (buffer-live-p (cdr buf)) ; Ensure buffer is live
                                 (string= (cae-exwm-get-workspace-name (cdr buf)) current-ws-name))))
               ;; persp-buffers-completing-read already filters by current perspective
               (target-buffer-name (read-buffer "Switch to EXWM buffer: " nil t filter-fn)))
          (when target-buffer-name
            (switch-to-buffer (get-buffer target-buffer-name))))
      ;; Non-EXWM Workspace: Use the standard interactive switcher for the workspace
      (call-interactively (command-remapping #'persp-switch-to-buffer)))))

(defun cae-exwm--get-workspace-buffers (workspace-name)
  "Return a list of live buffers matching WORKSPACE-NAME in the current perspective."
  (cl-remove-if-not (lambda (buf)
                      (and (buffer-live-p buf)
                           (string= (cae-exwm-get-workspace-name buf) workspace-name)))
                    (+workspace-buffer-list)))

;;;###autoload
(defun cae-exwm-switch-next-workspace-buffer ()
  "Switch to the next EXWM buffer belonging to the current EXWM workspace.
Cycles through the matching buffers. Does nothing if not in an
EXWM workspace or if only one matching buffer exists."
  (interactive)
  (let ((current-ws-name (+workspace-current-name)))
    (if (not (cl-member current-ws-name cae-exwm-workspaces :test #'string=))
        (message "Not in an EXWM workspace")
      (let* ((candidate-buffers (cae-exwm--get-workspace-buffers current-ws-name))
             (num-candidates (length candidate-buffers)))
        (when (> num-candidates 1)  ; Only cycle if there's more than one buffer
          (let ((current-index (cl-position (current-buffer) candidate-buffers)))
            (when current-index ; Ensure current buffer is one of the candidates
              (let* ((next-index (% (1+ current-index) num-candidates))
                     (next-buffer (nth next-index candidate-buffers)))
                (switch-to-buffer next-buffer)))))))))

;;;###autoload
(defun cae-exwm-switch-previous-workspace-buffer ()
  "Switch to the previous EXWM buffer belonging to the current EXWM workspace.
Cycles through the matching buffers. Does nothing if not in an
EXWM workspace or if only one matching buffer exists."
  (interactive)
  (let ((current-ws-name (+workspace-current-name)))
    (if (not (cl-member current-ws-name cae-exwm-workspaces :test #'string=))
        (message "Not in an EXWM workspace")
      (let* ((candidate-buffers (cae-exwm--get-workspace-buffers current-ws-name))
             (num-candidates (length candidate-buffers)))
        (when (> num-candidates 1)  ; Only cycle if there's more than one buffer
          (let ((current-index (cl-position (current-buffer) candidate-buffers)))
            (when current-index ; Ensure current buffer is one of the candidates
              (let* ((prev-index (% (+ current-index -1 num-candidates) num-candidates)) ; Modulo arithmetic for previous
                     (prev-buffer (nth prev-index candidate-buffers)))
                (switch-to-buffer prev-buffer)))))))))
