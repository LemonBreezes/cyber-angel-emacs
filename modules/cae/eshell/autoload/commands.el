;;; cae/eshell/autoload/commands.el -*- lexical-binding: t; -*-

;;; Buffer and File Operations

;;;###autoload
(defun cae-eshell-buffer-contents (buffer)
  "Return fontified contents of BUFFER as a string.
This ensures syntax highlighting is applied before returning the content."
  (with-current-buffer buffer
    (font-lock-ensure (point-min) (point-max))
    (buffer-string)))

;;;###autoload
(defun cae-eshell-file-contents (file)
  "Return fontified contents of FILE as a string.
If FILE is already open in a buffer, use that buffer.
Otherwise, open FILE temporarily with minimal settings."
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
  "Output content of the first buffer whose name matches REGEXP.
Returns nil if no matching buffer is found."
  (cl-loop for buf in (buffer-list)
           thereis
           (and (string-match-p regexp (buffer-name buf))
                (with-current-buffer buf
                  (buffer-substring-no-properties (point-min) (point-max))))))

;;;###autoload
(defun eshell/hat (&rest files)
  "Output FILES with syntax highlighting.
Originally from Aweshell: https://github.com/manateelazycat/aweshell"
  (dolist (f files)
    (if (file-exists-p f)
        (eshell-print (cae-eshell-file-contents f))
      (eshell-error (format "File not found: %s\n" f)))))

;;;###autoload
(defun eshell/swap (file1 file2)
  "Swap the contents of FILE1 and FILE2.
Both files must exist and be writable."
  (unless (and (file-exists-p file1) (file-exists-p file2))
    (error "Both files must exist"))
  (unless (and (file-writable-p file1) (file-writable-p file2))
    (error "Both files must be writable"))
  (let ((temp (make-temp-name (expand-file-name "eshell-swap-" temporary-file-directory))))
    (rename-file file1 temp)
    (rename-file file2 file1)
    (rename-file temp file2)))

;;; Directory Navigation

;;;###autoload
(defun eshell/u (&optional n)
  "Change to the directory of the Nth most recently used buffer.
N defaults to 1, meaning the most recently used buffer."
  (setq n (or n 1))
  (let* ((buf
          (cl-find-if (lambda (buf)
                        (and (get-buffer-window buf)
                             (not (+popup-buffer-p buf))
                             (doom-real-buffer-p buf)
                             (or (buffer-local-value 'default-directory buf)
                                 (buffer-local-value 'list-buffers-directory buf))
                             (<= (cl-decf n) 0)))
                      (cdr (buffer-list)))))
    (if buf
        (eshell/cd (or (buffer-local-value 'default-directory buf)
                       (buffer-local-value 'list-buffers-directory buf)))
      (eshell-print "No suitable buffer found\n"))))

;;;###autoload
(defun eshell/uu ()
  "Change to the directory of the second most recently used buffer.
Equivalent to calling `eshell/u' with argument 2."
  (eshell/u 2))

;;; File Finding and Searching

;;;###autoload
(defun eshell/f (filename &optional dir try-count)
  "Search for files matching FILENAME in DIR or current directory.
A wrapper around the standard `find' executable with some filters.

If no results are found, it tries up to two more times with wildcard
patterns around FILENAME.

Arguments:
  FILENAME: The name pattern to search for
  DIR: Directory to search in (default: current directory)
  TRY-COUNT: Internal parameter for recursive calls"
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

;;;###autoload
(defun eshell/ef (filename &optional dir)
  "Find and edit the first file matching FILENAME in DIR.
Uses `eshell/f' to find the file and opens it for editing."
  (let* ((files (eshell/f filename dir))
         (file (car (s-split "\n" files))))
    (if (and file (not (string-empty-p file)))
        (find-file file)
      (message "No matching files found"))))

;;; Utility Commands

;;;###autoload
(defun eshell/set (&rest args)
  "Create buffer-local variables from pairs of arguments.
Example: set var1 value1 var2 value2"
  (unless (cl-evenp (length args))
    (error "Arguments must be in pairs"))
  (dolist (arg-pair (seq-partition args 2))
    (seq-let (var val) arg-pair
      (let ((var-sym (make-symbol var)))
        (set (make-local-variable var-sym) val)))))

;;;###autoload
(defun eshell/help (cmd)
  "Get help for command CMD from tldr, man or helpful."
  (cae-eshell-help-run-help cmd))

;;; Interactive Commands

;;;###autoload
(defun cae-eshell-clear ()
  "Clear the eshell buffer while preserving current input.
Based on https://www.n16f.net/blog/clearing-the-eshell-buffer/"
  (interactive)
  (let ((input (eshell-get-old-input)))
    (eshell/clear t)
    (insert input)))

;;;###autoload
(defun cae-sudo-toggle ()
  "Toggle sudo prefix for the current command.
If the command starts with sudo, remove it; otherwise, add it.
Originally from Aweshell: https://github.com/manateelazycat/aweshell"
  (interactive)
  (let ((pt (point))
        (commands (buffer-substring-no-properties
                   (progn (beginning-of-line) (point)) (point-max))))
    (if (string-match-p "^sudo " commands)
        (progn
          (while (re-search-forward "sudo " nil t)
            (replace-match "" t nil))
          (goto-char (- pt (length "sudo "))))
      (progn
        (insert "sudo ")
        (goto-char (+ pt (length "sudo ")))))))

;;;###autoload
(defun cae-eshell-quit-or-delete-char (arg)
  "Delete character ahead or quit eshell if at end of line.
With numeric prefix ARG, delete that many characters."
  (interactive "p")
  (if (eolp)
      (eshell-life-is-too-much)
    (delete-char arg)))
