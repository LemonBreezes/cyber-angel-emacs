;;; private/misc-applications/autoload/empv.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +empv--youtube-show-tabulated-results (candidates)
  (with-current-buffer (get-buffer-create "*empv-yt-results*")
    (empv-youtube-results-mode)
    (setq tabulated-list-entries
          (seq-map-indexed
           (lambda (it index)
             (let* ((video-info (cdr it))
                    (video-title (propertize (alist-get 'title video-info) 'empv-youtube-item it))
                    (video-view (format "%0.2fk" (/ (alist-get 'viewCount video-info) 1000.0)))
                    (video-length (format "%0.2f" (/ (alist-get 'lengthSeconds video-info) 60.0))))
               (list index (vector "<THUMBNAIL>" video-title video-length video-view))))
           candidates))
    (tabulated-list-init-header)
    (when empv-youtube-thumbnail-quality
      (empv--youtube-tabulated-load-thumbnails candidates))
    (tabulated-list-print)
    (back-to-indentation)
    (pop-to-buffer-same-window (current-buffer))))
