;;; cae/misc-applications/autoload/hackernews.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-hackernews ()
  (interactive)
  (let ((buf (get-buffer "*hackernews top stories*")))
    (if (buffer-live-p buf)
        (pop-to-buffer buf)
      (call-interactively #'hackernews))))
