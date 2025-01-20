;;; cae/misc-applications/autoload/mpc.el -*- lexical-binding: t; -*-

(defvar cae-mpc--wconf nil)
(defvar cae-mpc-buf-pos-alist nil)
(defvar cae-mpc--prev-buf nil)

;;;###autoload
(defun cae-mpc (&optional arg)
  (interactive "P")
  (setq cae-mpc--old-wconf (current-window-configuration))
  (let ((ignore-window-parameters t))
    (delete-other-windows))
  (cl-letf (((symbol-function #'goto-char)
             (symbol-function #'ignore))))
  (if (and cae-mpc--wconf
           (not (cl-find-if-not #'buffer-live-p
                                (mapcar #'cdr (if mpc-proc (process-get mpc-proc 'buffers))))))
      (set-window-configuration cae-mpc--wconf)
    (call-interactively #'mpc))
  (let ((inhibit-redisplay t))
    (when (and cae-mpc--prev-buf (get-buffer-window cae-mpc--prev-buf))
      (select-window (get-buffer-window cae-mpc--prev-buf))
      (cae-mpc-jump-to-previous-position)
      (hl-line-highlight)
      (mpc-playlist)))
  (setq cae-mpc--wconf (current-window-configuration)))

;;;###autoload
(defun cae-mpc-quit ()
  (interactive)
  (dolist (buf (mapcar #'cdr (if mpc-proc (process-get mpc-proc 'buffers))))
    (setf (alist-get buf cae-mpc-buf-pos-alist)
          (with-current-buffer buf
            (point-marker))))
  (setq cae-mpc--prev-buf (buffer-name (current-buffer)))
  (if cae-mpc--old-wconf
      (progn
        (set-window-configuration cae-mpc--old-wconf)
        (setq cae-mpc--old-wconf nil))
    (mpc-quit)))

;;;###autoload
(defun cae-mpc-reload ()
  (interactive)
  (let ((inhibit-redisplay t))
    (mpc-quit)
    (mpc)
    (setq cae-mpc--old-wconf (current-window-configuration))
    (dolist (buf (mapcar #'cdr (if mpc-proc (process-get mpc-proc 'buffers))))
      (setf (alist-get buf cae-mpc-buf-pos-alist)
            (with-current-buffer buf
              (point-marker))))))

(defun cae-mpc-jump-to-previous-position ()
  (when-let* ((marker (alist-get (window-buffer (selected-window)) cae-mpc-buf-pos-alist))
              (pos (marker-position marker)))
    (run-at-time 0.02 nil #'goto-char pos)))

;;;###autoload
(defun cae-mpc-other-window-previous ()
  (interactive)
  (setf (alist-get (window-buffer (selected-window))
                   cae-mpc-buf-pos-alist)
        (point-marker))
  (cl-loop do (call-interactively #'other-window-previous)
           until (and (not (string= (buffer-name (current-buffer)) "*MPC-Status*"))
                      (not (string= (buffer-name (current-buffer)) "*MPC-Songs*")))
           finally (goto-char (marker-position
                               (alist-get (window-buffer (selected-window))
                                          cae-mpc-buf-pos-alist)))))

;;;###autoload
(defun cae-mpc-other-window ()
  (interactive)
  (setf (alist-get (window-buffer (selected-window))
                   cae-mpc-buf-pos-alist)
        (point-marker))
  (cl-loop do (call-interactively #'other-window)
           until (and (not (string= (buffer-name (current-buffer)) "*MPC-Status*"))
                      (not (string= (buffer-name (current-buffer)) "*MPC-Songs*")))
           finally (goto-char (marker-position
                               (alist-get (window-buffer (selected-window))
                                          cae-mpc-buf-pos-alist)))))

;;;###autoload
(defun cae-mpc-play ()
  (interactive)
  (mpc-select)
  (mpc-play))
