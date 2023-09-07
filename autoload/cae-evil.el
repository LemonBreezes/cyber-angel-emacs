;;; autoload/cae-evil.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-forward-page (&optional count)
  (interactive "p")
  (if (> count 0)
      (when (and (eq (point) (progn (forward-page 1) (1- (point)))))
        (forward-page count))
    (forward-page count)))

;;;###autoload
(defun cae-show-normal-state-bindings ()
  (interactive)
  (if-let ((map
            (or (evil-get-auxiliary-keymap
                 (cond ((bound-and-true-p git-timemachine-mode)
                        git-timemachine-mode-map)
                       (t (current-local-map)))
                 'normal))))
      (which-key--show-keymap "Normal state bindings" map nil nil nil)
    (message "No %s normal state bindings are defined." major-mode)))

(defun evil-collection-unimpaired--encode (beg end fn)
  "Apply FN from BEG to END."
  (save-excursion
    (goto-char beg)
    (let* ((end (if (eq evil-this-type 'line) (1- end) end))
           (text (buffer-substring-no-properties beg end)))
      (delete-region beg end)
      (insert (funcall fn text)))))

;;;###autoload (autoload 'evil-collection-unimpaired-b64-encode "autoload/cae-evil" nil t)
(evil-define-operator cae-unimpaired-b64-encode (count &optional beg end)
  "Encode a base64 string."
  (interactive "<c><r>")
  (ignore count)
  (evil-collection-unimpaired--encode beg end #'base64-encode-string))

;;;###autoload (autoload 'evil-collection-unimpaired-b64-decode "autoload/cae-evil" nil t)
(evil-define-operator cae-unimpaired-b64-decode (count &optional beg end)
  "Decode a base64 string."
  (interactive "<c><r>")
  (ignore count)
  (evil-collection-unimpaired--encode beg end #'base64-decode-string))

;;;###autoload
(defun cae-unimpaired-paste-above ()
  "Paste above current line with preserving indentation."
  (interactive)
  (let ((indent (current-indentation))
        (column (current-column)))
    (evil-insert-newline-above)
    (indent-to indent)
    (evil-paste-after 1)
    (move-to-column column)))

;;;###autoload
(defun cae-unimpaired-paste-below ()
  "Paste below current line with preserving indentation."
  (interactive)
  (let ((indent (current-indentation))
        (column (current-column)))
    (evil-insert-newline-below)
    (indent-to indent)
    (evil-paste-after 1)
    (move-to-column column)))

;;;###autoload
(defun cae-comint-delchar-or-maybe-eof ()
  (interactive)
  (if (and (derived-mode-p 'comint-mode)
           (eobp)
           (not (bolp)))
      (progn (comint-send-eof)
             (call-interactively #'kill-current-buffer))
    (lookup-key evil-insert-state-map [delete])))

;;;###autoload
(evil-define-command cae-evil-org-delete-back-to-indentation ()
  "Delete back to the first non-whitespace character.
If point is before the first non-whitespace character of a
current line then delete from the point to the beginning of the
current line.  If point is on the beginning of the line, behave
according to `evil-backspace-join-lines'."
  (let ((beg (if (<= (current-column) (current-indentation))
                 (line-beginning-position)
               (save-excursion (evil-first-non-blank) (point))))
        (org-beg (save-excursion (org-beginning-of-line) (point))))
    (evil-delete (max beg org-beg)
                 (point))))

;;;###autoload
(defun cae-evil-org-insert-heading ()
  (interactive)
  (call-interactively #'org-insert-heading)
  (when (eq evil-state 'normal)
    (evil-append-line 1)))

;;;###autoload
(defun cae-evil-org-insert-todo-heading ()
  (interactive)
  (call-interactively #'org-insert-todo-heading)
  (+log evil-state)
  (when (eq evil-state 'normal)
    (evil-append-line 1)))
