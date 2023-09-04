;;; private/misc-applications/autoload/mpc.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+mpc-hydra/body "private/misc-applications/autoload/mpc" nil t)
(eval
 `(defhydra +mpc-hydra (:color pink :hint nil)
    ("<f6>" nil "Exit" :exit t)
    ("q" +mpc-quit nil :exit t)
    ("<" mpc-prev "Previous song" :column "Navigate")
    (">" mpc-next "Next song" :column "Navigate")
    (,(if (modulep! :editor evil) "t" "s") mpc-toggle-play "Toggle play" :column "Toggle")
    ,@(when (modulep! :editor evil)
        '(("r" mpc-toggle-repeat "Toggle repeat" :column "Toggle")
          ("s" mpc-toggle-shuffle "Toggle shuffle" :column "Toggle")
          ("c" mpc-toggle-consume "Toggle consume" :column "Toggle")
          ("p" mpc-playlist "Show playlist" :column "Playlist")
          ("a" mpc-playlist-add "Add to playlist" :column "Playlist")
          ("x" mpc-play-at-point "Play at point" :column "Playlist")))
    ("p" mpc-pause "Pause" :column "Toggle")
    ("g" mpc-seek-current "Seek current" :column "Navigate")
    ("o" mpc-goto-playing-song "Goto playing song" :column "Movement"))
 t)


(defvar +mpc--wconf nil)

;;;###autoload
(defun +mpc (&optional arg)
  (interactive "P")
  (setq +mpc--old-wconf (current-window-configuration))
  (delete-other-windows)
  (if (and +mpc--wconf
           (not (cl-find-if-not #'buffer-live-p
                                (mapcar #'cdr (if mpc-proc (process-get mpc-proc 'buffers))))))
      (set-window-configuration +mpc--wconf)
    (call-interactively #'mpc))
  (+mpc-jump-to-previous-position)
  (setq +mpc--wconf (current-window-configuration)))

;;;###autoload
(defun +mpc-quit ()
  (interactive)
  (dolist (buf (mapcar #'cdr (if mpc-proc (process-get mpc-proc 'buffers))))
    (setf (alist-get buf +mpc-buf-pos-alist)
          (with-current-buffer buf
            (point-marker))))
  (if +mpc--old-wconf
      (progn
        (set-window-configuration +mpc--old-wconf)
        (setq +mpc--old-wconf nil))
    (mpc-quit)))

;; This is a hack that should be unncessary but for some reason restoring the
;; window configuration doesn't work properly for MPC. This is a workaround.

(defun +mpc-jump-to-previous-position ()
  (when-let (pos (alist-get (window-buffer (selected-window))
                            +mpc-buf-pos-alist))
    (goto-char (marker-position pos))))

;;;###autoload
(defun +mpc-other-window-previous ()
  (interactive)
  (setf (alist-get (window-buffer (selected-window))
                   +mpc-buf-pos-alist)
        (point-marker))
  (cl-loop do (call-interactively #'other-window-previous)
           until (not (string= (buffer-name (current-buffer)) "*MPC-Status*"))
           finally (goto-char (marker-position
                               (alist-get (window-buffer (selected-window))
                                          +mpc-buf-pos-alist)))))

;;;###autoload
(defun +mpc-other-window ()
  (interactive)
  (setf (alist-get (window-buffer (selected-window))
                   +mpc-buf-pos-alist)
        (point-marker))
  (cl-loop do (call-interactively #'other-windou)
           until (not (string= (buffer-name (current-buffer)) "*MPC-Status*"))
           finally (goto-char (marker-position
                               (alist-get (window-buffer (selected-window))
                                          +mpc-buf-pos-alist)))))
