;;; autoload/cae-benchmark.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-benchmark-config-modules ()
  "Benchmark loading time of different config modules.
Creates separate Doom Emacs processes to test performance impact
of loading files defined by *-init-*-enabled-p variables."
  (interactive)
  (let* ((temp-file (make-temp-file "emacs-benchmark-"))
         (doom-init-file (expand-file-name "init.el" doom-private-dir))
         (early-init-file (expand-file-name "early-init.el" doom-emacs-dir))
         (form
          `(progn
             ;; Set noninteractive to nil to load full config
             (setq noninteractive nil)
             (setq command-line-args nil
                   command-line-args-left nil)
             (setq inhibit-redisplay nil)
             (setq window-system 'x)

             ;; Fix for the Doom benchmark display function using advice-add pattern
             (defvar benchmark-start-time (current-time)
               "Start time for tracking benchmark duration.")
             (defun doom-benchmark-display-with-timing-h (&optional return-p)
               "Advice to override doom-display-benchmark-h with accurate timing.
           If RETURN-P, return the message as a string instead of displaying it."
               (let* ((package-count (- (length load-path)
                                        (or (length (get 'load-path 'initial-value)) 0)))
                      (module-count (if (and (boundp 'doom-modules) doom-modules)
                                        (hash-table-count doom-modules)
                                      -1))
                      ;; Calculate elapsed time properly
                      (init-time (or (and (boundp 'doom-init-time)
                                          (numberp doom-init-time)
                                          (not (= doom-init-time 0))
                                          doom-init-time)
                                     (float-time (time-subtract (current-time) benchmark-start-time)))))
                 (funcall (if return-p #'format #'message)
                          "Doom loaded %d packages across %d modules in %.03fs"
                          package-count
                          module-count
                          init-time)))
             (add-hook 'doom-after-init-hook
                       (lambda ()
                         (setq doom-init-time
                               (float-time (time-subtract (current-time) benchmark-start-time))))
                       80)
             (with-eval-after-load 'doom-start
               ;; Use override advice for the benchmark display function
               (advice-add 'doom-display-benchmark-h :override
                           #'doom-benchmark-display-with-timing-h))

             (advice-add #'y-or-n-p :override #'ignore)

             ;; Load the early init file
             (load ,early-init-file nil t)

             ;; Enable messages
             (setq inhibit-message nil)

             (let ((debug-on-error nil)
                   (debug-on-signal nil))
               (condition-case err
                   (progn
                     ;; Load the core Doom functionality
                     (load (expand-file-name "doom.el" doom-core-dir) nil t)

                     ;; Explicitly load the init file
                     (load ,doom-init-file nil t)

                     ;; Initialize Doom properly
                     (doom-initialize)

                     ;; Run all startup hooks to complete initialization
                     (require 'doom-lib)
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
     "-Q" "--batch" "--load" benchmark-file)

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
