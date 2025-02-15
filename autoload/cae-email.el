;;; autoload/cae-email.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-notmuch-quit ()
  (interactive)
  (if (bound-and-true-p +notmuch-workspace-name)
      (+workspace/kill +notmuch-workspace-name)
    (call-interactively #'notmuch-bury-or-kill-this-buffer)))

;;;###autoload
(defun cae-mu-init ()
  (interactive)
  (compilation-start
   (format "mu init --my-address=%s --maildir=%s && mu index"
           user-mail-address
           mail-source-directory)))
