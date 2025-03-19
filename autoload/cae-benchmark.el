;;; autoload/cae-benchmark.el -*- lexical-binding: t; -*-

;; FIXME The problem is that by the time this loads, the config has already been
;; loaded!
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
             (message "Hello! %s" doom-init-time)
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
