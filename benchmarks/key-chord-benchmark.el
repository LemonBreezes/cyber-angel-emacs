;;; key-chord-benchmark.el --- Benchmark for key-chord typing simulation -*- lexical-binding: t; -*-

(require 'key-chord)
(require 'benchmark)

(defun cae-key-chord-benchmark (&optional iterations)
  "Benchmark key-chord by simulating random alpha-numeric key presses.
Runs ITERATIONS times (default 20)."
  (interactive)
  (unless iterations
    (setq iterations 20))
  
  (let ((alphanumeric-chars "abcdefghijklmnopqrstuvwxyz0123456789")
        (key-chord-safety-interval-wait 0.0)
        (results nil)
        (unread-command-events-original unread-command-events)
        (key-chord-last-unmatched-original key-chord-last-unmatched))
    
    ;; Enable key-chord mode for the benchmark
    (unless key-chord-mode
      (key-chord-mode 1))
    
    ;; Run the benchmark
    (setq results
          (benchmark-run iterations
            (let* ((random-index (random (length alphanumeric-chars)))
                   (current-key (aref alphanumeric-chars random-index)))
              ;; Simulate key-chord processing by calling the input method
              ;; with a random character
              (key-chord-input-method current-key))))
    
    ;; Restore original state
    (setq unread-command-events unread-command-events-original
          key-chord-last-unmatched key-chord-last-unmatched-original)
    
    ;; Display results
    (message "Key-chord random typing benchmark (%d iterations):" iterations)
    (message "  Time elapsed: %f seconds" (car results))
    (message "  Garbage collections: %d" (cadr results))
    (message "  GC time: %f seconds" (caddr results))
    
    results))

(provide 'key-chord-benchmark)
;;; key-chord-benchmark.el ends here
