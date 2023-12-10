;;; private/unpackaged/autoload/org-fix-blank-lines.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-unpackaged-org-fix-blank-lines (&optional prefix)
  "Ensure that blank lines exist between headings and between headings and their contents.
With prefix, operate on whole buffer. Ensures that blank lines
exist after each headings's drawers."
  (interactive "P")
  ;; For some reason if I don't do this, Emacs hangs when running this function.
  ;;  I was finally able to reproduce this issue while creating my Elfeed Org
  ;;  file.
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward org-heading-regexp nil t)
      (goto-char (match-beginning 0))
      (while (not (looking-back "\n\n" nil))
        ;; Insert blank lines before heading.
        (insert "\n")))
    (goto-char (point-max))

    (org-map-entries (lambda ()
                       (org-with-wide-buffer
                        ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                        ;; newlines before the current heading, so we do this part widened.
                        (while (not (looking-back "\n\n" nil))
                          ;; Insert blank lines before heading.
                          (insert "\n")))
                       (let ((end (org-entry-end-position)))
                         ;; Insert blank lines before entry content
                         (forward-line)
                         (while (and (org-at-planning-p)
                                     (< (point) (point-max)))
                           ;; Skip planning lines
                           (forward-line))
                         (while (re-search-forward org-drawer-regexp end t)
                           ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
                           ;; for some reason it doesn't work correctly when operating on hidden text.
                           ;; This works, taken from `org-agenda-get-some-entry-text'.
                           (re-search-forward "^[ \t]*:END:.*\n?" end t)
                           (goto-char (match-end 0)))
                         (unless (or (= (point) (point-max))
                                     (org-at-heading-p)
                                     (looking-at-p "\n"))
                           (insert "\n"))))
                     t (if prefix
                           nil
                         'tree))))

(defun cae-unpackaged-org-fix-all-blank-lines ()
  (unless (derived-mode-p 'doom-docs-org-mode)
    (ignore-errors (cae-unpackaged-org-fix-blank-lines '(4)))))

;;;###autoload
(defun cae-unpackaged-org-fix-blank-lines-before-save ()
  (add-hook 'before-save-hook #'cae-unpackaged-org-fix-all-blank-lines nil t))
