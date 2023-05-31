;;; private/misc-applications/autoload/elfeed.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +elfeed-quit ()
  (interactive)
  (elfeed-db-save)
  (+workspace/delete +rss-workspace-name)
  (when (buffer-live-p elfeed-log-buffer-name)
    (kill-buffer elfeed-log-buffer-name)))
