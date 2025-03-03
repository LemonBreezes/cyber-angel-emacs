;;; benchmarks/key-chord-benchmark.el -*- lexical-binding: t; -*-

(defun cae-key-chord--random-alnum-char ()
  "Generate a random alpha-numeric character."
  (let* ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
         (len (length chars))
         (idx (random len)))
    (aref chars idx)))

;;;###autoload
(defun cae-key-chord-benchmark (&optional iterations)
  "Benchmark key-chord performance without modifying any active buffers.
Optional argument ITERATIONS specifies how many simulated key chords to process.
Default is 1000 iterations."
  (interactive "p")
  (let* ((iterations (or iterations 1000))
         (temp-buffer (generate-new-buffer " *key-chord-benchmark*"))
         (key-chord-mode-was-on key-chord-mode)
         (start-time (current-time))
         (elapsed 0))
    (unwind-protect
        (with-current-buffer temp-buffer
          (unless key-chord-mode-was-on
            (key-chord-mode 1))
          (emacs-lisp-mode)

          ;; Simulate processing key chords with random keys
          (dotimes (_ iterations)
            (let* ((first-char (cae-key-chord--random-alnum-char))
                   (second-char (cae-key-chord--random-alnum-char))
                   (chord (string first-char second-char)))

              ;; Define a test chord with these random characters
              (key-chord-define-local chord #'ignore)

              ;; Process the chord
              (key-chord-input-method first-char)
              ;; Simulate the second keypress arriving within the delay
              (let ((input-method-function nil)
                    (key-chord-last-unmatched nil))
                (key-chord-input-method second-char))))

          (setq elapsed (float-time (time-subtract (current-time) start-time)))
          (message "Key-chord benchmark: processed %d simulated random chords in %.3f seconds (%.1f chords/sec)"
                   iterations elapsed (/ iterations elapsed)))

      ;; Cleanup
      (kill-buffer temp-buffer)
      (unless key-chord-mode-was-on
        (key-chord-mode -1))))
  nil)
