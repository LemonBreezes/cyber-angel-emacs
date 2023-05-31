;;; private/dired/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-dired-set-layout-h ()
  (when (and (derived-mode-p 'dired-mode)
             (one-window-p)
             (not (memq this-command '(next-buffer previous-buffer))))
    (dirvish-layout-toggle)))

;;;###autoload
(defun cae-dired-load-dirvish-h (dir)
  (require 'dirvish)
  (setq find-directory-functions
        (delq '+load-dirvish find-directory-functions))
  (appendq! find-directory-functions '(dired-noselect))
  (dired-noselect dir))
