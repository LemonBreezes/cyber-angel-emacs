;;; private/misc-applications/autoload/empv.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +empv-lazy-load-keymap ()
  (interactive)
  (require 'empv)
  (setq unread-command-events
        (listify-key-sequence
         (kbd (concat +misc-applications-prefix
                      +misc-applications-music-prefix
                      "m"))))
  (cae-which-key-show-map 'empv-map))
