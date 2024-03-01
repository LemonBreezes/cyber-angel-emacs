;;; autoload/cae-debug.el -*- lexical-binding: t; -*-

;;;###autoload
(defun search-in-dabbrev-buffers (search-string)
  "Search for SEARCH-STRING in all buffers returned by dabbrev--select-buffers."
  (let ((buffers (dabbrev--select-buffers)))
    (multi-occur buffers search-string)))
