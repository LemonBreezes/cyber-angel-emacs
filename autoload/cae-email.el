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
  (let* ((emails
          (mapconcat
           (lambda (context)
             (concat "--my-address="
                     (cdr (assoc 'user-mail-address (mu4e-context-vars context)))))
           mu4e-contexts
           " ")))
    (compilation-start
     (format "mu init %s --maildir=%s && mu index"
             emails
             mail-source-directory))))

(cdr (assoc 'user-mail-address (mu4e-context-vars (car mu4e-contexts))))
