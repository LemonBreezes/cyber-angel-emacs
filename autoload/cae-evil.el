;;; autoload/cae-evil.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-forward-page (&optional count)
  (interactive "p")
  (if (> count 0)
      (when (and (eq (point) (progn (forward-page 1) (1- (point)))))
        (forward-page count))
    (forward-page count)))

(defvar cae-show-normal-state--map nil)

;;;###autoload
(defun cae-show-normal-state-bindings ()
  (interactive)
  (setq cae-show-normal-state--map
        (or (evil-get-auxiliary-keymap
             (cond ((bound-and-true-p git-timemachine-mode)
                    git-timemachine-mode-map)
                   (t (current-local-map)))
             'normal)))
  (if cae-show-normal-state--map
      (let ((which-key-idle-delay 0))
        (funcall (cae-oneshot-keymap cae-show-normal-state--map nil)))
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
(defun cae-evil-org-delete-back-to-indentation ()
  (interactive)
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

;;;###autoload (autoload '+evil-buffer-org-new "autoload/cae-evil" nil t)
(evil-define-command +evil-buffer-org-new (count file)
  "Creates a new ORG buffer replacing the current window, optionally
   editing a certain FILE"
  :repeat nil
  (interactive "P<f>")
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new org*")))
      (set-window-buffer nil buffer)
      (with-current-buffer buffer
        (org-mode)
        (setq-local doom-real-buffer-p t)))))

;;;###autoload
(defun cae-evil-mu4e-enter-insert-mode ()
  (when (eq evil-state 'normal)
    (call-interactively #'evil-append)))
