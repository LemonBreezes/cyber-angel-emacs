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
