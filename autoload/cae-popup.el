;;; autoload/cae-popup.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-popup-resize-help-buffer (win)
    (when (let ((i 0))
            (cl-loop for win the windows
                     if (window-in-direction 'right win t)
                     do (cl-incf i)
                     finally return (> i 1)))
      ;; resize the help buffer to 1/3 of the screen
      (with-selected-window win
        (enlarge-window (- (/ (frame-width) 3)
                           (window-width win))
                        t))))
