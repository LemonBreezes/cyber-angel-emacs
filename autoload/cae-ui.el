
;;; ~/.doom.d/autoload/cae-ui.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-ui-topsy-rjsx-fn ()
  (when (> (window-start) 1)
    (save-excursion
      (goto-char (window-start))
      (while (save-excursion
               (forward-line -1)
               (looking-at-p "^\\s-*//"))
        (forward-line -1))
      (ignore-errors
        (js2-beginning-of-defun))
      (back-to-indentation)
      (font-lock-ensure (point) (point-at-eol))
      (buffer-substring (point) (point-at-eol)))))

;;;###autoload
(defun cae-apply-ansi-color-to-buffer-h ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
