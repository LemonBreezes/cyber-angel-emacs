;;; cae/rss/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +elfeed-quit ()
  (interactive)
  (elfeed-db-save)
  (+workspace/delete +rss-workspace-name)
  (when (buffer-live-p elfeed-log-buffer-name)
    (kill-buffer elfeed-log-buffer-name)))

;;;###autoload
(defun +elfeed-toggle-log-buffer ()
  (interactive)
  (if (get-buffer-window elfeed-log-buffer-name)
      (delete-window (get-buffer-window elfeed-log-buffer-name))
    (pop-to-buffer elfeed-log-buffer-name)
    (local-set-key (kbd "q") #'quit-window)
    (when (featurep 'evil)
      (evil-local-set-key 'normal (kbd "q") #'quit-window))))

;;;###autoload
(defun +elfeed-regenerate-db ()
  (interactive)
  (let ((buf (current-buffer)))
    (elfeed-db-unload)
    (when (buffer-live-p buf)
      (set-buffer buf)
      (set-window-buffer (selected-window) buf)))
  (when (file-exists-p elfeed-db-directory)
    (delete-directory elfeed-db-directory t))
  (=rss)
  (elfeed-update))
