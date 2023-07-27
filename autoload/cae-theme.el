;;; autoload/cae-theme.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-dark-theme-p ()
  (eq (frame-parameter nil 'background-mode) 'dark))

;;;###autoload
(defun cae-night-time-p ()
  (when-let ((now (reverse (cl-subseq (decode-time) 0 3)))
             (sunset (or (doom-store-get 'circadian-sunset)
                         (require 'circadian nil t)
                         (circadian-sunset)))
             (sunrise (or (doom-store-get 'circadian-sunrise)
                          (require 'circadian nil t)
                          (circadian-sunrise))))
    (doom-store-put 'circadian-sunset sunset)
    (doom-store-put 'circadian-sunrise sunrise)
    (or (and (>= (cl-first now) (cl-first sunset)))
        (and (< (cl-first now) (cl-first sunrise)))
        (and (= (cl-first now) (cl-first sunset))
             (>= (cl-second now) (cl-second sunset)))
        (and (= (cl-first now) (cl-first sunrise))
             (< (cl-second now) (cl-second sunrise))))))
