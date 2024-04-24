;;; autoload/cae-benchmark.el -*- lexical-binding: t; -*-

(defun cae-average-line-length-excluding-empty-lines ()
  "Compute the average line length in the current buffer, excluding empty lines."
  (interactive)
  (let (res)
    (save-excursion
      (goto-char (point-min))
      (let ((total-length 0)
            (line-count 0))
        (while (not (eobp))             ; Iterate until the end of the buffer
          (unless (looking-at "^$")     ; Check if the line is empty
            (setq total-length (+ total-length (- (line-end-position) (line-beginning-position))))
            (setq line-count (1+ line-count)))
          (forward-line 1))             ; Move to the next line
        (setq res (if (eq line-count 0) 0 (/ (float total-length) line-count)))))
    (if (called-interactively-p)
        (message "Average line length (excluding empty lines) is %.02f" res)
      res)))

(defun cae-compute-optimal-jit-lock-chunk-size ()
  "Calculate an estimated optimal value for `jit-lock-chunk-size' based on window dimensions."
  (save-excursion
    (goto-char (window-start))
    ;; Calculate optimal chunk size: window height * average line length (with spacing)
    ;; Then, increase the estimated size by 10% to provide a buffer.
    (floor (* (window-height)
              (cae-average-line-length-excluding-empty-lines)
              (1+ (or line-spacing 0))))))

;;;###autoload
(defun cae-set-jit-lock-chunk-size-to-optimal ()
  "Set `jit-lock-chunk-size' to the computed optimal value."
  (interactive)
  (set-variable 'jit-lock-chunk-size (cae-compute-optimal-jit-lock-chunk-size))
  (message "jit-lock-chunk-size set to optimal value of %d" jit-lock-chunk-size))

(defun cae-benchmark-gc (&optional threshold)
  (let ((start-time (current-time))
        (gcs-done-old gcs-done)
        (gc-cons-threshold (or threshold gc-cons-threshold)))
    ;; Generate garbage until `gc-cons-threshold' is exceeded.
    (while (>= gcs-done-old gcs-done)
      (cl-loop for i from 0 to 1000 do
               (make-string 1000 ?a)
               (make-vector 1000 0)
               (make-hash-table :test 'eq)
               (make-symbol "a")))
    (message "Garbage collection completed in %.06f seconds"
             (float-time (time-subtract (current-time) start-time)))))

;;(cae-benchmark-gc (* 32 1024 1024 1024))
