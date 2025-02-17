;;; autoload/cae-hotloading.el -*- lexical-binding: t; -*-

;;; autoload/cae-hotloading.el -*- lexical-binding: t; -*-

(require 'cl-lib)


;; ---[ 1. Cache Lookup & Cleanup ]--------------------------------------

(defun cae-hotloading--cleanup-dir-locals-cache ()
  "Remove duplicates and invalid entries from `dir-locals-directory-cache'."
  (setq dir-locals-directory-cache
        (cl-remove-duplicates dir-locals-directory-cache :test #'equal)))

(defun cae-hotlodaing-dir-locals-cache-lookup (file)
  "Return the best matching `dir-locals-directory-cache' entry for FILE,
 ignoring any .dir-locals file checks.
The returned value is an entry of the form:
  (\"/path/to/dir\" CLASS MTIME ...)

If no matching directory is found in `dir-locals-directory-cache',
return nil.  This function does not parse `.dir-locals.el' nor
does it attempt to verify cache validity."
  (when file
    (setq file (expand-file-name file))
    (let ((best nil))
      ;; Iterate over each cached entry in dir-locals-directory-cache.
      (dolist (entry dir-locals-directory-cache)
        ;; entry is typically ("DIRECTORY" CLASS MTIME ...)
        ;; We check if "DIRECTORY" is a prefix of FILE and pick the longest match.
        (when (and (string-prefix-p (car entry) file
                                    (memq system-type '(windows-nt cygwin ms-dos)))
                   (or (null best)
                       (> (length (car entry)) (length (car best)))))
          (setq best entry)))
      best)))


;; ---[ 2. Reload for All Buffers ]--------------------------------------

;;;###autoload
(defun cae-hotloading-reload-all-dir-locals ()
  "Reload directory-local variables for all buffers that match any entry in the cache."
  (interactive)
  (cae-hotloading--cleanup-dir-locals-cache)
  (dolist (buf (buffer-list))
    (when (cae-hotlodaing-dir-locals-cache-lookup (buffer-file-name buf))
      (with-current-buffer buf
        (hack-dir-local-variables-non-file-buffer)))))


;; ---[ 3. Reload for Buffers of a Specific Class ]----------------------

;;;###autoload
(defun cae-hotloading-reload-dir-locals-for-class (class)
  "Reload directory-local variables for all buffers whose dir-local class is CLASS."
  (cae-hotloading--cleanup-dir-locals-cache)
  (dolist (buf (buffer-list))
    (when-let ((entry (cae-hotlodaing-dir-locals-cache-lookup (buffer-file-name buf))))
      (when (eq (nth 1 entry) class)
        (with-current-buffer buf
          (hack-dir-local-variables-non-file-buffer))))))
