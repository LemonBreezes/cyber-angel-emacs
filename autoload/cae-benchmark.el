;;; autoload/cae-benchmark.el -*- lexical-binding: t; -*-

;; FIXME Does not work for benchmarking our config.
;;;###autoload
(defun cae-benchmark-config-modules ()
  "Benchmark loading time of different config modules.
Creates separate Doom Emacs processes to test performance impact
of loading files defined by *-init-*-enabled-p variables."
  (interactive)
  (let* ((temp-file (make-temp-file "emacs-benchmark-"))
         (early-init-file (expand-file-name "early-init.el" doom-emacs-dir))
         (form
          `(progn
             ;; Set noninteractive to nil to load full config
             (setq noninteractive nil)
             (setq command-line-args nil
                   command-line-args-left nil)
             (setq inhibit-redisplay nil)
             (setq window-system 'x)

             (advice-add #'y-or-n-p :override #'ignore)

             ;; Load the early init file
             (load ,early-init-file nil t)

             ;; Enable messages
             (setq inhibit-message nil)

             (let ((debug-on-error nil)
                   (debug-on-signal nil))
               (condition-case err
                   (progn
                     (run-hooks 'doom-first-input-hook)
                     (run-hooks 'doom-first-buffer-hook)
                     (run-hooks 'doom-first-file-hook)

                     ;; Run all startup hooks to complete initialization
                     (doom-run-all-startup-hooks-h))
                 (error
                  (require 'pp)
                  (let ((trace (mapconcat #'pp-to-string (backtrace-frames) "")))
                    (message "Error in startup hooks: %s\nBacktrace:\n%s"
                             (error-message-string err)
                             trace)))))

             ;; Write the result to our temp file
             (with-temp-file ,temp-file
               (insert
                (with-current-buffer messages-buffer-name
                  (widen)
                  (buffer-substring-no-properties (point-min) (point-max)))))
             (kill-emacs)))
         (benchmark-file (make-temp-file "doom-benchmark-" nil ".el")))

    ;; Write the benchmark code to a file
    (with-temp-file benchmark-file
      (prin1 form (current-buffer)))

    ;; Run Emacs with the benchmark code
    (call-process
     (expand-file-name invocation-name invocation-directory)
     nil nil nil
     "--load" benchmark-file)

    ;; Display the results
    (when (file-exists-p temp-file)
      (with-current-buffer (get-buffer-create "*Doom Benchmark*")
        (erase-buffer)
        (insert-file-contents temp-file)
        (display-buffer (current-buffer))))

    ;; Clean up temporary files
    (when (file-exists-p temp-file)
      (ignore-errors (delete-file temp-file)))
    (when (file-exists-p benchmark-file)
      (ignore-errors (delete-file benchmark-file)))))
