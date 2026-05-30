;;; autoload/cae-languages.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-languages-align-markdown-tables ()
  (save-excursion
    (condition-case nil
        (while (re-search-forward markdown-table-line-regexp nil t)
          (org-table-align)
          (re-search-forward "^[ 	]*[^| 	]"))
      (search-failed nil))))
