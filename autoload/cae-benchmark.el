;;; autoload/cae-benchmark.el -*- lexical-binding: t; -*-

(defun cae-compute-optimal-jit-lock-chunk-size ()
  "Calculate an estimated optimal value for `jit-lock-chunk-size' based on window dimensions."
  (save-excursion
    (goto-char (window-start))
    ;; Calculate optimal chunk size: window height * average line length (with spacing)
    ;; Then, increase the estimated size by 10% to provide a buffer.
    (floor (* 1.1 (window-height)
              (floor (/ (line-end-position) (line-number-at-pos)))
              (1+ (or line-spacing 0))))))

;;;###autoload
(defun cae-set-jit-lock-chunk-size-to-optimal ()
  "Set `jit-lock-chunk-size' to the computed optimal value."
  (interactive)
  (set-variable 'jit-lock-chunk-size (compute-optimal-jit-lock-chunk-size))
  (message "jit-lock-chunk-size set to optimal value of %d" jit-lock-chunk-size))

(defun cae-convert-size-to-bytes (size)
  "Convert human-readable size (e.g., '1G', '1M', '512K') to bytes."
  (interactive "sEnter size (e.g., 1G, 1M, 512K): ")
  (let ((unit-multiplier `((?K . 1024)
                          (?M . ,(* 1024 1024))
                          (?G . ,(* 1024 1024 1024))
                          (?T . ,(* 1024 1024 1024 1024))))
        (number (string-to-number size))
        (unit (upcase (aref size (1- (length size))))))
    (if (and (> number 0) (assoc unit unit-multiplier))
        (let ((multiplier (cdr (assoc unit unit-multiplier))))
          (message "%s is %d bytes" size (* number multiplier)))
      (error "Invalid format"))))

;;;###autoload
(defun cae-benchmark-gc (&optional threshold)
  (interactive)
  (let ((start-time (current-time))
        (gcs-done-old gcs-done)
        (gc-cons-threshold (or threshold gc-cons-threshold)))
    ;; generate garbage until `gc-cons-threshold' is exceeded
    (while (>= gcs-done-old gcs-done)
      ;; generate varied types of garbage
      (cl-loop for i from 0 to 1000 do
               (make-string 1000 ?a)
               (make-vector 1000 0)
               (make-hash-table :test 'eq)
               (make-symbol "a")))
    (message "Garbage collection completed in %.06f seconds"
             (float-time (time-subtract (current-time) start-time)))))

(cae-benchmark-gc (* 8 1024 1024 1024))
