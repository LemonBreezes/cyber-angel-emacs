;;; autoload/cae-editor.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-kill-buffer-a (orig-func &optional buffer-or-name)
  (setq buffer-or-name (or buffer-or-name (current-buffer)))
  (catch 'quit
    (save-window-excursion
      (with-current-buffer buffer-or-name
        (let (done (buf (current-buffer)))
          (when (and buffer-file-name (buffer-modified-p))
            (while (not done)
              (let ((response (read-char-choice
                               (format "Save file %s? (y, n, d, q) " (buffer-file-name buf))
                               '(?y ?n ?d ?q))))
                (setq done (cond
                            ((eq response ?q) (throw 'quit nil))
                            ((eq response ?y) (save-buffer) t)
                            ((eq response ?n) (set-buffer-modified-p nil) t)
                            ((eq response ?d) (diff-buffer-with-file) nil))))))
          (funcall orig-func buffer-or-name))))))

;;;###autoload
(defun cae-sp-delete-char (&optional arg)
  (interactive "*P")
  (cond ((and delete-active-region
              (region-active-p))
         (sp-delete-region (region-beginning) (region-end)))
        ;; Only call `delete-char' if the parens are unbalanced.
        ((condition-case error
             (scan-sexps (point-min) (point-max))
           (scan-error t))
         (delete-char (or arg 1)))
        ((sp-delete-char arg))))

;;;###autoload
(defun cae-auto-sudoedit-maybe-h ()
  (unless (let ((path (or (buffer-file-name) list-buffers-directory)))
            (string= (file-attribute-user-id
                      (file-attributes path 'string))
                     (if (and (featurep 'tramp)
                              (tramp-tramp-file-p path))
                         (tramp-get-remote-uid (tramp-dissect-file-name path)
                                               'string)
                       (user-login-name))))
    (require 'auto-sudoedit)
    (auto-sudoedit)))

;;;###autoload
(defun cae-toggle-sudo ()
  (interactive)
  (let* ((file (or buffer-file-name
                   (when (or (derived-mode-p 'dired-mode)
                             (derived-mode-p 'wdired-mode))
                     default-directory)))
         (file-localname (file-remote-p file 'localname))
         (tramp-prefix (and file-localname
                            (string-remove-suffix file-localname file)))
         (sudo-prefix (format "/sudo:root@%s:" (file-remote-p file 'host))))
    (if (string-suffix-p sudo-prefix tramp-prefix)
        (progn (advice-add #'auto-sudoedit :override #'ignore)
               (unwind-protect (find-file (concat (string-remove-suffix sudo-prefix tramp-prefix)
                                                  (tramp-file-local-name file)))
                 (advice-remove #'auto-sudoedit #'ignore)))
      (doom/sudo-this-file))))

(defun cae-auto-sudoedit-exempt-p ()
  (let ((path (or (buffer-file-name) list-buffers-directory)))
    (or (file-directory-p path)
        (string-prefix-p (thread-last lisp-directory
                                      (file-name-parent-directory)
                                      (file-name-parent-directory))
                         path))))
