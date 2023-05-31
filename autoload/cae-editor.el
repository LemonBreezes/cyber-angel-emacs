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

(defun cae-auto-sudoedit-exempt-p ()
  (let ((path (or (buffer-file-name) list-buffers-directory)))
    (or ;; (file-directory-p path)
     (string-prefix-p (thread-last lisp-directory
                                   (file-name-parent-directory)
                                   (file-name-parent-directory))
                      path))))

;;;###autoload
(defun cae-toggle-sudo ()
  (interactive)
  (let* ((curr-path (or (buffer-file-name) list-buffers-directory))
         (tramp-path (if (tramp-tramp-file-p curr-path)
                         ;; doesn't work for remote files
                         (tramp-file-name-localname (tramp-dissect-file-name curr-path))
                       (concat "/sudo::" curr-path))))
    (when buffer-file-name
      (set-visited-file-name tramp-path t))
    (when dired-directory
      (dired-unadvertise dired-directory)
      (setq list-buffers-directory tramp-path)
      (setq dired-directory tramp-path)
      (setq default-directory tramp-path)
      (dired-advertise))
    (when (string= (car recentf-list) curr-path)
      (pop recentf-list))
    (revert-buffer t t)))
