;;; autoload/cae-evil.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-forward-page (&optional count)
  (interactive "p")
  (if (> count 0)
      (when (and (eq (point) (progn (forward-page 1) (1- (point)))))
        (forward-page count))
    (forward-page count)))

(defun cae-current-state-keymap ()
  (let ((map (copy-keymap
              (make-composed-keymap
               (list (let ((map (evil-get-auxiliary-keymap
                                 (cond ((bound-and-true-p git-timemachine-mode)
                                        git-timemachine-mode-map)
                                       (t (current-local-map)))
                                 evil-state t t)))
                       (and (keymapp map)
                            (> (length map) 2)
                            map))
                     (evil-get-auxiliary-keymap
                      (make-composed-keymap
                       (thread-last (current-minor-mode-maps)
                                    (delq doom-leader-map)
                                    (delq general-override-mode-map)
                                    (delq evil-snipe-local-mode-map)
                                    ;; This is because the `evil-collection' module for
                                    ;; `beginend' defines its keybindings in `normal-state'
                                    ;; rather than `motion-state'.
                                    (delq (let ((mode (cl-find-if (lambda (x)
                                                                    (string-prefix-p "beginend-"
                                                                                     (symbol-name x)))
                                                                  local-minor-modes)))
                                            (when mode
                                              (symbol-value (intern (concat (symbol-name mode) "-map")))))))
                       t)
                      evil-state))))))
    ;; Do not show the remaps that are mapped to `ignore'.
    (map-keymap (lambda (key binding)
                  (when (eq key 'remap)
                    (map-keymap (lambda (key binding)
                                  (when (eq binding 'ignore)
                                    (define-key map (vector 'remap key) nil t)))
                                binding)))
                map)
    map))

;;;###autoload
(defun cae-embark-state-bindings ()
  (interactive)
  (embark-bindings-in-keymap (cae-current-state-keymap)))

;;;###autoload
(defun cae-which-key-show-state-keymap ()
  (interactive)
  (let ((keymap (cae-current-state-keymap))
        (which-key-replacement-alist
         (append '((("" . "emms-\\(.*\\)") . (nil . "\\1")))
                 which-key-replacement-alist)))
    (which-key--show-keymap nil keymap nil t t)))

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

;;;###autoload (autoload 'cae-evil-buffer-org-new "autoload/cae-evil" nil t)
(evil-define-command cae-evil-buffer-org-new (count file)
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
(defun cae-evil-append-buffer-or-code ()
  (interactive)
  (save-restriction
    (narrow-to-page)
    (funcall (key-binding "G"))
    (call-interactively #'evil-append)))
