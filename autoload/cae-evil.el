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
