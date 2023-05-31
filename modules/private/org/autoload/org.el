;;; private/org/autoload/org.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-org-set-created-timestamp (&rest _args)
  "Add a creation timestamp to the current Org entry.
If the current command is run with a prefix argument, prevent
from running."
  (unless (or current-prefix-arg
              (string-prefix-p "README.org" (buffer-name)))
    (org-set-property "CREATED_TIME"
                      (org-timestamp-format
                       (org-timestamp-from-time (current-time) t t)
                       (org-time-stamp-format t t)))))
