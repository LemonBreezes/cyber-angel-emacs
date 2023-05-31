;;; autoload/cae-email.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-notmuch-quit ()
  ((interactive))
  (+workspace/delete +notmuch-workspace-name))
