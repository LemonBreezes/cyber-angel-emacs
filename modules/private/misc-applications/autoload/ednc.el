;;; private/misc-applications/autoload/ednc.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ednc-show-notifications ()
  (interactive)
  (let ((buf (get-buffer-create ednc-log-name)))
    (pop-to-buffer buf)))
