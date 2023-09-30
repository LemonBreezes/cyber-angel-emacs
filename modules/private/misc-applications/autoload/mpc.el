;;; private/misc-applications/autoload/mpc.el -*- lexical-binding: t; -*-

(defvar +mpc--wconf nil)
(defvar +mpc-buf-pos-alist nil)
(defvar +mpc--prev-buf nil)

;;;###autoload
(defun +mpc (&optional arg)
  (interactive "P")
  (setq +mpc--old-wconf (current-window-configuration))
  (let ((ignore-window-parameters t))
    (delete-other-windows))
  (cl-letf (((symbol-function #'goto-char)
             (symbol-function #'ignore))))
  (if (and +mpc--wconf
           (not (cl-find-if-not #'buffer-live-p
                                (mapcar #'cdr (if mpc-proc (process-get mpc-proc 'buffers))))))
      (set-window-configuration +mpc--wconf)
    (call-interactively #'mpc))
  (when (and +mpc--prev-buf (get-buffer-window +mpc--prev-buf))
    (select-window (get-buffer-window +mpc--prev-buf)))
  (+mpc-jump-to-previous-position)
  (setq +mpc--wconf (current-window-configuration)))

;;;###autoload
(defun +mpc-quit ()
  (interactive)
  (dolist (buf (mapcar #'cdr (if mpc-proc (process-get mpc-proc 'buffers))))
    (setf (alist-get buf +mpc-buf-pos-alist)
          (with-current-buffer buf
            (point-marker))))
  (setq +mpc--prev-buf (buffer-name (current-buffer)))
  (if +mpc--old-wconf
      (progn
        (set-window-configuration +mpc--old-wconf)
        (setq +mpc--old-wconf nil))
    (mpc-quit)))

;;;###autoload
(defun +mpc-reload ()
  (interactive)
  (let ((inhibit-redisplay t))
    (mpc-quit)
    (mpc)
    (setq +mpc--old-wconf (current-window-configuration))
    (dolist (buf (mapcar #'cdr (if mpc-proc (process-get mpc-proc 'buffers))))
      (setf (alist-get buf +mpc-buf-pos-alist)
            (with-current-buffer buf
              (point-marker))))))

(defun +mpc-jump-to-previous-position ()
  (when-let* ((marker (alist-get (window-buffer (selected-window)) +mpc-buf-pos-alist))
              (pos (marker-position marker)))
    (run-at-time 0.02 nil #'goto-char pos)))

;;;###autoload
(defun +mpc-other-window-previous ()
  (interactive)
  (setf (alist-get (window-buffer (selected-window))
                   +mpc-buf-pos-alist)
        (point-marker))
  (cl-loop do (call-interactively #'other-window-previous)
           until (and (not (string= (buffer-name (current-buffer)) "*MPC-Status*"))
                      (not (string= (buffer-name (current-buffer)) "*MPC-Songs*")))
           finally (goto-char (marker-position
                               (alist-get (window-buffer (selected-window))
                                          +mpc-buf-pos-alist)))))

;;;###autoload
(defun +mpc-other-window ()
  (interactive)
  (setf (alist-get (window-buffer (selected-window))
                   +mpc-buf-pos-alist)
        (point-marker))
  (cl-loop do (call-interactively #'other-window)
           until (and (not (string= (buffer-name (current-buffer)) "*MPC-Status*"))
                      (not (string= (buffer-name (current-buffer)) "*MPC-Songs*")))
           finally (goto-char (marker-position
                               (alist-get (window-buffer (selected-window))
                                          +mpc-buf-pos-alist)))))
