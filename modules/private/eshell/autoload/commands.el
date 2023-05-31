;;; private/eshell/autoload/commands.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-eshell-buffer-contents (buffer)
  "Return fontified buffer contents for BUFFER."
  (with-current-buffer buffer
    (font-lock-ensure (point-min) (point-max))
    (buffer-string)))

;;;###autoload
(defun cae-eshell-file-contents (file)
  "Return fontified file contents for FILE."
  (let ((buffer (get-file-buffer file)))
    (if buffer
        (cae-eshell-buffer-contents buffer)
      (unwind-protect
          (cae-eshell-buffer-contents
           (setq buffer
                 (let ((inhibit-message t)
                       (non-essential t)
                       (enable-dir-local-variables nil)
                       (enable-local-variables (and enable-local-variables :safe)))
                   (find-file-noselect file))))
        (when buffer
          (kill-buffer buffer))))))

;;;###autoload
(defun eshell/b (regexp)
  "Output buffer content of buffer matching REGEXP."
  (cl-loop for buf in (buffer-list)
           thereis
           (and (string-match-p regexp (buffer-name buf))
                (with-current-buffer buf
                  (buffer-substring-no-properties (point-min) (point-max))))))

;; By LemonBreezes https://github.com/LemonBreezes/.doom.d/blob/master/modules/private/eshell/autoload.el
;; Originally from Aweshell https://github.com/manateelazycat/aweshell/blob/master/aweshell.el.
;;;###autoload
(defun eshell/hat (&rest files)
  "Output FILES with highlighting."
  (dolist (f files)
    (eshell-print (cae-eshell-file-contents f))))

;;;###autoload
(defun eshell/swap (file1 file2)
  "Swap the contents of FILE1 and FILE2."
  (let ((temp (make-temp-name temporary-file-directory)))
    (rename-file file1 temp)
    (rename-file file2 file1)
    (rename-file temp file2)))

;; Made obsolete by `eshell-elecslash'.
;;;;; https://github.com/dakra/dmacs/blob/master/init.org#eshell
;;;;;###autoload
;;(defun eshell/rcd (&optional directory)
;;  "Like regular 'cd' but don't jump out of a tramp directory.
;;When on a remote directory with tramp don't jump 'out' of the server.
;;So if we're connected with sudo to 'remotehost'
;;'$ rcd /etc' would go to '/sudo:remotehost:/etc' instead of just
;;'/etc' on localhost."
;;  (unless (file-remote-p default-directory)
;;    (error "Not in a remote location"))
;;  (with-parsed-tramp-file-name default-directory nil
;;    (eshell/cd
;;     (tramp-make-tramp-file-name
;;      method user nil host nil (or directory "") hop))))

;; https://github.com/howardabrams/hamacs/blob/main/ha-eshell.org
;;;###autoload
(defun eshell/set (&rest args)
  "Creates a buffer local variables."
  (dolist (arg-pair (seq-partition args 2))
    (seq-let (var val) arg-pair
      (let ((var-sym (make-symbol var)))
        (set (make-local-variable var-sym) val)))))

;; https://github.com/OrgTangle/ntangle/blob/de3ec97c4650cc7b5875ee2614e4c1df5ccb5388/tests/dmacs/init.org#L2435
;;;###autoload
(defun eshell/f (filename &optional dir try-count)
  "Searches for files matching FILENAME in either DIR or the
current directory. Just a typical wrapper around the standard
`find' executable.

Since any wildcards in FILENAME need to be escaped, this wraps
the shell command.

If not results were found, it calls the `find' executable up to
two more times, wrapping the FILENAME pattern in wildcat
matches. This seems to be more helpful to me."
  (let* ((cmd (concat
               (executable-find "find")
               " " (or dir ".")
               "      -not -path '*/.git*'"
               " -and -not -path '*node_modules*'"
               " -and -not -path '*classes*'"
               " -and "
               " -type f -and "
               "-iname '" filename "'"))
         (results (shell-command-to-string cmd)))

    (if (not (s-blank-str? results))
        results
      (cond
       ((or (null try-count) (= 0 try-count))
        (eshell/f (concat filename "*") dir 1))
       ((or (null try-count) (= 1 try-count))
        (eshell/f (concat "*" filename) dir 2))
       (t "")))))

;; https://github.com/OrgTangle/ntangle/blob/de3ec97c4650cc7b5875ee2614e4c1df5ccb5388/tests/dmacs/init.org#L2435
;;;###autoload
(defun eshell/ef (filename &optional dir)
  "Searches for the first matching filename and loads it into a
file to edit."
  (let* ((files (eshell/f filename dir))
         (file (car (s-split "\n" files))))
    (find-file file)))

;; https://www.n16f.net/blog/clearing-the-eshell-buffer/
;;;###autoload
(defun cae-eshell-clear ()
  (interactive)
  (let ((input (eshell-get-old-input)))
    (eshell/clear t)
    (eshell-emit-prompt)
    (insert input)))

;;;###autoload
(defun eshell/u (&optional n)
  (setq n (or n 1))
  (let* ((ignored-bufs)
         (buf
          (cl-find-if (lambda (buf)
                        (and (get-buffer-window)
                             (not (+popup-buffer-p buf))
                             (doom-real-buffer-p buf)
                             (or (buffer-local-value 'default-directory buf)
                                 (buffer-local-value 'list-buffers-directory buf))
                             (<= (cl-decf n) 0)))
                      (cdr (buffer-list)))))
    (when buf
      (eshell/cd (or (buffer-local-value 'default-directory buf)
                     (buffer-local-value 'list-buffers-directory buf))))))

;;;###autoload
(defun eshell/uu ()
  (eshell/u 2))

;;;###autoload
(defun eshell/doom (&rest args)
  (let ((doom (cond ((executable-find "doom")
                     "doom")
                    ((file-exists-p "~/.emacs.d/bin/doom")
                     "~/.emacs.d/bin/doom")
                    ((file-exists-p "~/.config/emacs/bin/doom")
                     "~/.config/emacs/bin/doom"))))
    (apply #'call-process doom nil t nil
           (flatten-list args))))
