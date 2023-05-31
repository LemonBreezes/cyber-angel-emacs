;;; autoload/cae-bookmarks.el -*- lexical-binding: t; -*-

(defvar cae-bookmarks-downloads-directory (expand-file-name "~/Downloads/"))

;;;###autoload
(defun cae-bookmarks-jump-to-newest-download (_)
  (let ((newest-file (-max-by #'file-newer-than-file-p
                              (cl-remove-if
                               (lambda (file)
                                 (or (string-prefix-p "." (file-name-nondirectory file))
                                     (file-directory-p file)))
                               (cl-union (directory-files "~/Downloads/" t)
                                         (directory-files "~/" t))))))
    (dired (file-name-directory newest-file))
    (dired-goto-file newest-file)))
