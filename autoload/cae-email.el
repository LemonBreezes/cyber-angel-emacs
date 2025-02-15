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
  (compilation-start "mu init --my-address=look@strawberrytea.xyz --maildir=/home/st/.mail && mu index"))
