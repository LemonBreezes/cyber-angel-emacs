;;; autoload/cae-evil.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-forward-page (&optional count)
  "Move forward COUNT pages, handling edge case at page boundaries.
When at a page boundary, avoid getting stuck by checking if we moved."
  (interactive "p")
  (if (> count 0)
      (let ((start (point)))
        (forward-page 1)
        (when (eq start (1- (point)))
          (forward-page count)))
    (forward-page count)))

(defun cae--strip-ignored-remaps (map)
  "Remove keys whose remap sub-keys have an 'ignore binding from MAP.
Returns the modified MAP."
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
  "Get the current Evil state keymap with appropriate filtering.
Combines local keymaps and minor mode maps for the current Evil state."
  (let* ((minor-keymap (if (bound-and-true-p git-timemachine-mode)
                           (evil-get-minor-mode-keymap 'normal 'git-timemachine-mode)
                         (evil-get-auxiliary-keymap (current-local-map)
                                                    evil-state t t)))
         (filtered-minor-maps (thread-last
                                (current-minor-mode-maps)
                                (delq doom-leader-map)
                                (delq general-override-mode-map)
                                (delq evil-snipe-local-mode-map)
                                (delq (let ((mode (cl-find-if (lambda (x)
                                                                (string-prefix-p "beginend-"
                                                                                 (symbol-name x)))
                                                              local-minor-modes)))
                                        (when mode
                                          (symbol-value (intern (concat (symbol-name mode) "-map"))))))))
         (aux-keymap (evil-get-auxiliary-keymap 
                      (make-composed-keymap filtered-minor-maps t)
                      evil-state))
         (combined (make-composed-keymap (list minor-keymap aux-keymap)))
         (map (copy-keymap combined)))
    (cae--strip-ignored-remaps map)))

;;;###autoload
(defun cae-embark-state-bindings ()
  "Show Embark bindings for the current Evil state keymap."
  (interactive)
  (embark-bindings-in-keymap (cae-current-state-keymap)))

;;;###autoload
(defun cae-which-key-show-state-keymap ()
  "Display the current Evil state keymap using which-key."
  (interactive)
  (let ((keymap (cae-current-state-keymap))
        (which-key-replacement-alist
         (append '((("" . "emms-\\(.*\\)") . (nil . "\\1")))
                 which-key-replacement-alist)))
    (which-key--show-keymap nil keymap nil t t)))

(defun cae--unimpaired-encode-region (beg end fn)
  "Apply encoding/decoding function FN to region from BEG to END."
  (save-excursion
    (goto-char beg)
    (let* ((end (if (eq evil-this-type 'line) (1- end) end))
           (text (buffer-substring-no-properties beg end)))
      (delete-region beg end)
      (insert (funcall fn text)))))

;;;###autoload (autoload 'evil-collection-unimpaired-b64-encode "autoload/cae-evil" nil t)
(evil-define-operator cae-unimpaired-b64-encode (count &optional beg end)
  "Encode the selected region using base64."
  (interactive "<c><r>")
  (ignore count)
  (cae--unimpaired-encode-region beg end #'base64-encode-string))

;;;###autoload (autoload 'evil-collection-unimpaired-b64-decode "autoload/cae-evil" nil t)
(evil-define-operator cae-unimpaired-b64-decode (count &optional beg end)
  "Decode the selected region from base64."
  (interactive "<c><r>")
  (ignore count)
  (cae--unimpaired-encode-region beg end #'base64-decode-string))

(defun cae--unimpaired-paste (newline-fn)
  "Insert a newline using NEWLINE-FN and paste with preserved indentation.
Maintains the current indentation and column position."
  (let ((indent (current-indentation))
        (column (current-column)))
    (funcall newline-fn)
    (indent-to indent)
    (evil-paste-after 1)
    (move-to-column column)))

;;;###autoload
(defun cae-unimpaired-paste-above ()
  "Paste above current line while preserving indentation."
  (interactive)
  (cae--unimpaired-paste #'evil-insert-newline-above))

;;;###autoload
(defun cae-unimpaired-paste-below ()
  "Paste below current line while preserving indentation."
  (interactive)
  (cae--unimpaired-paste #'evil-insert-newline-below))

;;;###autoload
(defun cae-comint-delchar-or-maybe-eof ()
  "Delete char or send EOF in comint mode at end of buffer.
In comint-mode at the end of buffer, send EOF and kill the buffer.
Otherwise, perform the normal delete action."
  (interactive)
  (if (and (derived-mode-p 'comint-mode)
           (eobp)
           (not (bolp)))
      (progn (comint-send-eof)
             (kill-current-buffer))
    (lookup-key evil-insert-state-map [delete])))

;;;###autoload
(defun cae-evil-org-delete-back-to-indentation ()
  "Delete from point back to indentation or org beginning of line."
  (interactive)
  (let ((beg (if (<= (current-column) (current-indentation))
                 (line-beginning-position)
               (save-excursion (evil-first-non-blank) (point))))
        (org-beg (save-excursion (org-beginning-of-line) (point))))
    (evil-delete (max beg org-beg)
                 (point))))

;;;###autoload
(defun cae-evil-org-insert-heading ()
  "Insert a new org heading and switch to append mode if in normal mode."
  (interactive)
  (call-interactively #'org-insert-heading)
  (when (eq evil-state 'normal)
    (evil-append-line 1)))

;;;###autoload
(defun cae-evil-org-insert-todo-heading ()
  "Insert a new org TODO heading and switch to append mode if in normal mode."
  (interactive)
  (call-interactively #'org-insert-todo-heading)
  (+log evil-state)
  (when (eq evil-state 'normal)
    (evil-append-line 1)))

;;;###autoload (autoload 'cae-evil-buffer-org-new "autoload/cae-evil" nil t)
(evil-define-command cae-evil-buffer-org-new (count file)
  "Create a new org buffer replacing the current window.
With FILE argument, edit that file instead."
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
  "Move to end of buffer or page and enter append mode.
In shell modes, moves to end of buffer. Otherwise uses Evil's G command."
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
  "Edit the region from BEG to END indirectly, preserving cursor position."
  (interactive "<R>d")
  (edit-indirect-region beg end t)
  (goto-char (- pos beg)))
