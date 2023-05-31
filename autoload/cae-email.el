;;; autoload/cae-email.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-notmuch-quit ()
  (interactive)
  (+workspace/delete (or (bound-and-true-p +notmuch-workspace-name)
                         "*notmuch*")))
