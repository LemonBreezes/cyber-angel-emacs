;;; cae/misc-applications/autoload/trashed.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-trashed-revert-buffer-a (oldfun)
  (when (prog1 (buffer-live-p trashed-buffer)
          (funcall oldfun))
    (revert-buffer)))
