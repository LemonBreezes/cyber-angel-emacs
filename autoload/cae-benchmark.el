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


;;;###autoload
(defun cae-benchmark-garbage-creation ()
  "Generate garbage and measure the time taken to trigger a single GC at the current `gc-cons-threshold`."
  (interactive)
  ;; First, we ensure garbage collection is turned off to prevent it from interfering
  ;; while we're generating garbage.
  (let ((gc-cons-threshold most-positive-fixnum)
        (start-time (current-time))
        (stop-time nil)
        garbage-list)

    ;; We generate garbage: in this case, add a lot of cons cells to a list.
    (while (< (garbage-collect)
              (list (cons 'conses gc-cons-threshold)))
      (setq garbage-list (cons nil garbage-list)))

    ;; Stop the timer as we reached the gc threshold limit
    (setq stop-time (current-time))

    ;; Now we manually trigger garbage collection and measure the time it takes.
    (message "Triggering garbage collection...")
    (let ((gc-start-time (current-time))
          gc-duration)
      (garbage-collect)

      (setq gc-duration (float-time (time-subtract (current-time) gc-start-time)))
      (message "Garbage collection took %s seconds" gc-duration))

    ;; Output how long it took to generate enough garbage to reach the threshold.
    (let ((duration (float-time (time-subtract stop-time start-time))))
      (message "Time to generate garbage: %s seconds" duration))))
