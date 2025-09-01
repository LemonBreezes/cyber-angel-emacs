;;; autoload/cae-tools.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-ping-status ()
  (interactive)
  (let ((buffer (generate-new-buffer "*internet*")))
    (make-process
     :name "internet"
     :connection-type 'pipe
     :buffer buffer
     :command (list "ping" "-q" "-c" "1" "8.8.8.8")
     :sentinel `(lambda (p e)
                  (with-current-buffer ',buffer
                    (goto-char (point-min))
                    (if (or (search-forward "unreachable" nil t)
                            (search-forward "errors" nil t))
                        (progn (unless (string-match "ping-status" (format "%s" timer-list))
                                 (cae-run-with-timer 5 5 "cae-ping-status"
                                                     'cae-ping-status)
                                 (message "no internet")))
                      (when (string-match "ping-status" (format "%s" timer-list))
                        (cancel-function-timers 'ping-status))
                      (message "internet working"))
                    (kill-buffer))))))

;; Custom lpr-print-region function to print to PDF and open in Emacs
(defun cae-lpr-print-region-to-pdf (start end &optional switches display)
  "Print region from START to END to PDF and open in Emacs.
SWITCHES and DISPLAY are ignored for PDF output."
  (interactive "r")
  (let* ((temp-file (make-temp-file "emacs-print-" nil ".ps"))
         (pdf-file (concat (file-name-sans-extension temp-file) ".pdf"))
         (gs-command (format "gs -q -dNOPAUSE -dBATCH -sOutputFile=%s -sDEVICE=pdfwrite %s -c quit" pdf-file temp-file)))
    (message "=== LPR-PRINT-REGION-TO-PDF ===")
    (message "Creating temporary PS file: %s" temp-file)
    (message "PDF will be: %s" pdf-file)
    (message "Ghostscript command: %s" gs-command)

    ;; First create the PostScript file using ps-print
    (message "Calling ps-print-region...")
    (ps-print-region start end temp-file)
    (message "PS file created, size: %d bytes" (file-attribute-size (file-attributes temp-file)))

    ;; Check if PS file was actually created and has content
    (if (and (file-exists-p temp-file)
             (> (file-attribute-size (file-attributes temp-file)) 0))
        (progn
          (message "PS file exists and has content, converting to PDF...")
          ;; Convert PS to PDF using Ghostscript
          (let ((exit-code (shell-command gs-command)))
            (message "Ghostscript conversion completed, exit code: %d" exit-code)

            ;; Check if PDF was created and open it
            (run-at-time 0.01 nil #'find-file pdf-file)))
      (message "ERROR: PS file was not created or is empty"))

    ;; Clean up temporary PS file
    (when (file-exists-p temp-file)
      (delete-file temp-file)
      (message "Cleaned up temporary PS file"))
    (message "=== LPR-PRINT-REGION-TO-PDF COMPLETED ===")))
