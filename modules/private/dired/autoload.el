;;; private/dired/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-dired-set-layout-h ()
  (when (and (derived-mode-p 'dired-mode)
             (one-window-p))
    (dirvish-layout-toggle)))

;;;###autoload
(defun cae-dired-load-dirvish-h (dir)
  (require 'dirvish)
  (setq find-directory-functions
        (delq '+load-dirvish find-directory-functions))
  (appendq! find-directory-functions '(dired-noselect))
  (dired-noselect dir))

;;;###autoload
(defun cae-dired-find-file (file &optional wildcards)
  "Like `find-file', but might exit the current Dirvish session."
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((dir (file-name-directory file)))
    (unless (file-equal-p dir default-directory)
      (find-file dir)))
  (unless (file-directory-p file)
    ;; Copied from `dirvish-find-entry-a'
    (let* ((dv (dirvish-curr)) (fn (nth 4 (dv-type dv))))
      (if fn (funcall fn) (dirvish-kill dv)))
    (find-file file)))
