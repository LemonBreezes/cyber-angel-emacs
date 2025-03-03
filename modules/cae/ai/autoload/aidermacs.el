;;; cae/ai/autoload/aidermacs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun aidermacs-buffer-name ()
  "Generate the aidermacs buffer name based on project root or current directory.
Prefers existing sessions closer to current directory."
  (let* ((root (aidermacs-project-root))
         (current-dir (file-truename default-directory))
         ;; Get all existing aidermacs buffers
         (aidermacs-buffers
          (cl-remove-if-not
           (lambda (buf)
             (string-match-p "^\\*aidermacs:" (buffer-name buf)))
           (buffer-list)))
         ;; Extract directory paths and subtree status from buffer names
         (buffer-dirs
          (mapcar
           (lambda (buf)
             (when (string-match "^\\*aidermacs:\\(.*?\\)\\*$"
                                 (buffer-name buf))
               (cons (match-string 1 (buffer-name buf))
                     (match-string 2 (buffer-name buf)))))
           aidermacs-buffers))
         ;; Find closest parent directory that has an aidermacs session
         (closest-parent
          (car
           (car
            (sort
             (cl-remove-if-not
              (lambda (dir-info)
                (and (car dir-info)
                     (string-prefix-p (car dir-info) current-dir)
                     (file-exists-p (car dir-info))))
              buffer-dirs)
             (lambda (a b)
               ;; Sort by path length (deeper paths first)
               (> (length (car a)) (length (car b))))))))
         (display-root (cond
                        ;; Use current directory for new subtree session
                        (aidermacs-subtree-only current-dir)
                        ;; Use closest parent if it exists
                        (closest-parent closest-parent)
                        ;; Fall back to project root for new non-subtree session
                        (t root))))
    (format "*aidermacs:%s*"
            (file-truename display-root))))
