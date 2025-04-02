;;; cae/org/autoload/tecosaur.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-org-export-remove-zero-width-space (text _backend _info)
  "Remove zero width spaces from TEXT."
  (unless (org-export-derived-backend-p 'org)
    (replace-regexp-in-string "\u200B" "" text)))
