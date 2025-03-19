;;; autoload/cae-benchmark.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-benchmark-config-modules ()
  "Benchmark loading time of different config modules.
Creates a separate Doom Emacs process to test performance impact."
  (interactive)
  (let* ((temp-file (make-temp-file "emacs-benchmark-"))
         (doom-private-dir-path (expand-file-name doom-private-dir))
         (doom-emacs-dir-path (expand-file-name doom-emacs-dir))
         (benchmark-file (make-temp-file "doom-benchmark-" nil ".el")))

    ;; Write a minimal bootstrap file that will run before Doom loads
    (with-temp-file benchmark-file
      (insert 
       (format "
;; Start timing before anything else happens
(defvar benchmark-start-time (current-time))

;; Set up advice to capture the benchmark results
(defun doom-display-benchmark-h-with-capture (&optional return-p)
  \"Capture benchmark results and write to temp file.\"
  (let* ((output (doom-display-benchmark-h t))
         (full-output (format \"Benchmark results:\\n%%s\" output)))
    (with-temp-file %S
      (insert full-output))
    (unless return-p
      (message output))))

(setq noninteractive nil)

;; Load Doom normally
(load (expand-file-name \"early-init.el\" %S) nil t)

;; Add our advice to capture the results
(with-eval-after-load 'doom-start
  (advice-add 'doom-display-benchmark-h :around #'doom-display-benchmark-h-with-capture))

(kill-emacs)
" 
              (prin1-to-string temp-file)
              (prin1-to-string doom-emacs-dir-path))))

    ;; Run a fresh Emacs process that loads our benchmark file first
    (message "Running benchmark...")
    (call-process
     (expand-file-name invocation-name invocation-directory)
     nil nil nil
     "-Q" 
     "--load" benchmark-file
     "--load" (expand-file-name "init.el" doom-emacs-dir-path))

    ;; Display the results
    (if (file-exists-p temp-file)
        (with-current-buffer (get-buffer-create "*Doom Benchmark*")
          (erase-buffer)
          (insert-file-contents temp-file)
          (display-buffer (current-buffer))
          (message "Benchmark complete. See *Doom Benchmark* buffer for results."))
      (message "Benchmark failed. No output was generated."))

    ;; Clean up temporary files
    (when (file-exists-p temp-file)
      (ignore-errors (delete-file temp-file)))
    (when (file-exists-p benchmark-file)
      (ignore-errors (delete-file benchmark-file)))))
