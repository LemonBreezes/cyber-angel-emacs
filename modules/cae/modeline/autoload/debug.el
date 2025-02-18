;;; cae/modeline/autoload/debug.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-show-mode-line-segments ()
  "Display all mode-line segments in a dedicated buffer.
Each segment (an element of `mode-line-format’) is printed along with its evaluated result,
as returned by `format-mode-line'. This makes it easier to inspect segments that may be
clipped in your normal mode line display."
  (interactive)
  ;; Make sure we have a list of segments (sometimes mode-line-format might be a symbol)
  (let* ((segments (if (listp mode-line-format)
                       mode-line-format
                     (list mode-line-format)))
         (output-buffer (get-buffer-create "*Mode-Line Segments*")))
    (with-current-buffer output-buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert "Dump of mode-line segments:\n\n")
      (dolist (seg segments)
        (condition-case err
            (let ((formatted (format-mode-line seg)))
              (insert (format "Segment code: %S\nEvaluated:    %s\n\n" seg formatted)))
          (error (insert (format "Segment code: %S\nError evaluating: %s\n\n" seg err)))))
      (goto-char (point-min))
      (special-mode))
    (display-buffer output-buffer)))
