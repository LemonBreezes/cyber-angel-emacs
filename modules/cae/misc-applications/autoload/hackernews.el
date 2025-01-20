;;; cae/misc-applications/autoload/hackernews.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'cae-hackernews "cae/misc-applications/autoload/hackernews" nil t)
;;;###autoload (autoload 'cae-hackernews-quit "cae/misc-applications/autoload/hackernews" nil t)

(cae-define-launcher
 cae-hackernews
 :launch-fn (lambda ()
              (interactive)
              (let ((buf (get-buffer "*hackernews top stories*")))
                (if (buffer-live-p buf)
                    (pop-to-buffer buf)
                  (call-interactively #'hackernews))))
 :workspace-name cae-hackernews-workspace-name)
