
;;; ~/.doom.d/autoload/cae-ui.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-apply-ansi-color-to-buffer-h ()
  (interactive)
  (unless (> (buffer-size) (* 1024 32))
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))
