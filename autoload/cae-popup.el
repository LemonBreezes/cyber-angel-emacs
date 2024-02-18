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

;;;###autoload
(defun cae-popup-shrink-to-fit (&optional window)
  "Shrinks WINDOW to fit the buffer contents, if the buffer isn't empty.

Uses `shrink-window-if-larger-than-buffer'."
  (run-at-time
   0.0 nil
   (lambda (window)
     (unless window
       (setq window (selected-window)))
     (unless (= (- (point-max) (point-min)) 0)
       (shrink-window-if-larger-than-buffer window))
     ;;(enlarge-window-horizontally
     ;; (- (+ (cadr (buffer-line-statistics (window-buffer window))) 3)
     ;;    (window-width window)))
     )
   window))
