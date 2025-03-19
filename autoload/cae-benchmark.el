;;; autoload/cae-benchmark.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-benchmark-config-modules ()
  "Benchmark loading time of different config modules.
Creates a separate Doom Emacs process to test performance impact."
  (interactive)
  (let* ((temp-file (make-temp-file "emacs-benchmark-"))
         (error-file (make-temp-file "emacs-benchmark-error-"))
         (benchmark-file (make-temp-file "doom-benchmark-" nil ".el"))
         ;; Define the benchmark form as a proper Lisp form
         (benchmark-form 
          `(progn
             ;; Start timing before anything else happens
             (defvar benchmark-start-time (current-time))
             
             ;; Set up advice to capture the benchmark results
             (defun doom-display-benchmark-h-with-capture (&optional return-p)
               "Capture benchmark results and write to temp file."
               (let* ((output (doom-display-benchmark-h t))
                      (full-output (format "Benchmark results:\n%s" output)))
                 (with-temp-file ,temp-file
                   (insert full-output))
                 (unless return-p
                   (message output))))
             
             (setq noninteractive nil)
             (setq debug-on-error t)
             
             ;; Load Doom normally
             (condition-case-unless-debug err
                 (load (expand-file-name "lisp/init.el" ,doom-emacs-dir) nil t)
               (error
                (with-temp-file ,error-file
                  (insert "Error during Doom initialization:\n\n"
                          (format "%S\n\n" err)
                          "Backtrace:\n"
                          (with-output-to-string
                            (backtrace))))))

             ;; Add our advice to capture the results
             (with-eval-after-load 'doom-start
               (advice-add 'doom-display-benchmark-h :around #'doom-display-benchmark-h-with-capture))

             (kill-emacs))))

    ;; Write the benchmark form to a file
    (with-temp-file benchmark-file
      (pp benchmark-form (current-buffer)))

    ;; Run a fresh Emacs process that loads our benchmark file first
    (message "Running benchmark...")
    (call-process
     (expand-file-name invocation-name invocation-directory)
     nil nil nil
     "-Q" 
     "--load" benchmark-file
     "--load" (expand-file-name "init.el" doom-emacs-dir))

    ;; Display the results
    (cond
     ((file-exists-p error-file)
      (with-current-buffer (get-buffer-create "*Doom Benchmark Error*")
        (erase-buffer)
        (insert-file-contents error-file)
        (display-buffer (current-buffer))
        (message "Benchmark encountered errors. See *Doom Benchmark Error* buffer for details.")))
     
     ((file-exists-p temp-file)
      (with-current-buffer (get-buffer-create "*Doom Benchmark*")
        (erase-buffer)
        (insert-file-contents temp-file)
        (display-buffer (current-buffer))
        (message "Benchmark complete. See *Doom Benchmark* buffer for results.")))
     
     (t
      (message "Benchmark failed. No output was generated.")))

    ;; Clean up temporary files
    (when (file-exists-p temp-file)
      (ignore-errors (delete-file temp-file)))
    (when (file-exists-p error-file)
      (ignore-errors (delete-file error-file)))
    (when (file-exists-p benchmark-file)
      (ignore-errors (delete-file benchmark-file)))))
