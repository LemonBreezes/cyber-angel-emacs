;;; private/misc-applications/autoload/vuiet.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +vuiet-buffer-hook ()
  (when (bound-and-true-p flyspell-mode) (flyspell-mode -1))
  (when (bound-and-true-p writegood-mode) (writegood-mode -1))
  (local-set-key (kbd "j") (cmd! (org-next-link)
                                 (font-lock-flush)))
  (local-set-key (kbd "k") (cmd! (org-previous-link)
                                 (font-lock-flush)))
  (local-set-key (kbd ";") #'ace-link-org))
