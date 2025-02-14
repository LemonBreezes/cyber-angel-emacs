;;; autoload/cae-evil.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-forward-page (&optional count)
  (interactive "p")
  (if (> count 0)
      (let ((start (point)))
        (forward-page 1)
        (when (eq start (1- (point)))
          (forward-page count)))
    (forward-page count)))

(defun cae--strip-ignored-remaps (map)
  "Remove keys whose remap sub-keys have an 'ignore binding from MAP."
  (map-keymap
   (lambda (key binding)
     (when (eq key 'remap)
       (map-keymap
        (lambda (subkey subbinding)
          (when (eq subbinding 'ignore)
            (define-key map (vector 'remap subkey) nil t)))
        binding)))
   map)
  map)

(defun cae-current-state-keymap ()
  (let* ((minor-keymap (if (bound-and-true-p git-timemachine-mode)
                           (evil-get-minor-mode-keymap 'normal 'git-timemachine-mode)
                         (evil-get-auxiliary-keymap (current-local-map)
                                                    evil-state t t)))
         (aux-keymap (let ((filtered-minor-maps
                            (thread-last (current-minor-mode-maps)
                                         (delq doom-leader-map)
                                         (delq general-override-mode-map)
                                         (delq evil-snipe-local-mode-map)
                                         (delq (let ((mode (cl-find-if (lambda (x)
                                                                         (string-prefix-p "beginend-"
                                                                                          (symbol-name x)))
                                                                       local-minor-modes)))
                                                 (when mode
                                                   (symbol-value (intern (concat (symbol-name mode) "-map")))))))))
                       (evil-get-auxiliary-keymap (make-composed-keymap filtered-minor-maps t)
                                                  evil-state)))
         (combined (make-composed-keymap (list minor-keymap aux-keymap)))
         (map (copy-keymap combined)))
    (cae--strip-ignored-remaps map)
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

(defun cae--unimpaired-paste (newline-fn)
  "Helper to insert a newline (via NEWLINE-FN) and then paste with the current indentation and column."
  (let ((indent (current-indentation))
        (column (current-column)))
    (funcall newline-fn)
    (indent-to indent)
    (evil-paste-after 1)
    (move-to-column column)))

;;;###autoload
(defun cae-unimpaired-paste-above ()
  "Paste above current line with preserving indentation."
  (interactive)
  (cae--unimpaired-paste #'evil-insert-newline-above))

;;;###autoload
(defun cae-unimpaired-paste-below ()
  "Paste below current line with preserving indentation."
  (interactive)
  (cae--unimpaired-paste #'evil-insert-newline-below))

;;;###autoload
(defun cae-comint-delchar-or-maybe-eof ()
  (interactive)
  (if (and (derived-mode-p 'comint-mode)
           (eobp)
           (not (bolp)))
      (progn (comint-send-eof)
             (kill-current-buffer))
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
    (cond
     ((derived-mode-p 'eshell-mode 'comint-mode)
      (end-of-buffer))
     (t (call-interactively (key-binding "G"))))
    (call-interactively #'evil-append)))

;; Allow passing the current point to Evil operators.
(after! evil
  (setf (alist-get "d" evil-interactive-alist)
        '((list (point)))))

;;;###autoload (autoload 'cae-evil-edit-indirect "autoload/cae-evil" nil t)
(evil-define-operator cae-evil-edit-indirect (beg end type pos)
  "Edit the region indirectly."
  (interactive "<R>d")
  (edit-indirect-region beg end t)
  (goto-char (- pos beg)))
