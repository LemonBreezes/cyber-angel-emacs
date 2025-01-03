;;; autoload/cae-editor.el -*- lexical-binding: t; -*-

(defun cae-strip-top-level-indentation (str)
  "Strip the top-level leading indentation for every line in STR.
The least indented line will have 0 leading whitespace. Convert tabs to spaces
using the tab-width variable."
  (let* ((lines (replace-regexp-in-string "\t" (make-string tab-width ?\s)
                                          (split-string str "\n")))
         (indentations (mapcar (lambda (line)
                                 (string-match "^[[:space:]]*" line)
                                 (match-end 0))
                               lines))
         (min-indentation (apply #'min (delq 0 indentations))))
    (mapconcat
     (lambda (line)
       (if (string-match "^[[:space:]]+" line)
           (let* ((indent (match-string 0 line))
                  (actual-indent (substring indent
                                            0 (min min-indentation
                                                   (length indent))))
                  (stripped-indent (replace-regexp-in-string
                                    (regexp-quote actual-indent) "" line)))
             stripped-indent)
         line))
     lines "\n")))

;;;###autoload
(defun cae-copy-for-reddit ()
  "Copy and indent active region or current defun with 4 spaces.
This is the format used on Reddit for code blocks."
  (interactive)
  (when-let* ((bounds (if (region-active-p)
                          (cons (region-beginning) (region-end))
                        (bounds-of-thing-at-point 'defun)))
              (text (cae-strip-top-level-indentation
                     (buffer-substring-no-properties (car bounds)
                                                     (cdr bounds)))))
    (setq deactivate-mark t)
    (kill-new (replace-regexp-in-string "^" "    " text))
    (message "Copied!")))

;;;###autoload
(defun cae-browse-url-generic-bookmark-handler (bookmark)
  "Bookmark handler for opening URLs with `browse-url-generic'."
  (require 'ffap)
  (let ((url (bookmark-prop-get bookmark 'filename)))
    (if (ffap-url-p url)
        (browse-url-generic url)
      (message "Bookmark does not have a valid FILENAME property."))))

;;;###autoload
(defun cae-dos2unix ()
  "Automate M-% C-q C-m RET C-q C-j RET"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match "" nil t))))

;;;###autoload
(defun cae-pop-mark ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (setq this-command 'set-mark-command
          real-this-command 'set-mark-command)
    (call-interactively #'set-mark-command)))

;;;###autoload
(defun cae-exchange-point-and-mark ()
  (interactive)
  (let ((current-prefix-arg
         (if (region-active-p)
             current-prefix-arg
           (pcase current-prefix-arg
             (`(4) nil)
             (_ '(4))))))
    (call-interactively #'exchange-point-and-mark)))

;;;###autoload
(defun cae-bind-C-z-to-abort-a (oldfun &rest args)
  (minibuffer-with-setup-hook
      (lambda ()
        (local-set-key (kbd "C-z") #'abort-recursive-edit))
    (apply oldfun args)))

;;;###autoload
(defun cae-embark-act-with-completing-read (&optional arg)
  (interactive "P")
  (require 'embark)
  (let* ((embark-prompter #'embark-completing-read-prompter)
         (act (propertize "Act" 'face 'highlight))
         (embark-indicators '())
         (posframe (cl-find-if
                    (lambda (frame)
                      (eq (frame-parameter frame 'posframe-hidehandler)
                          #'vertico-posframe-hidehandler))
                    (nreverse (visible-frame-list))))
         (vertico-posframe-size-function)
         (vertico-multiform-commands
          `((cae-embark-act-with-completing-read
             ,(if (>= (frame-width) 120)
                  'grid 'buffer)
             ,@(delq 'vertico-flat-mode (car vertico-multiform--stack))))))
    (when (featurep 'vertico-posframe)
      (setf vertico-posframe-size-function
            `(lambda (_)
               '(:height ,(frame-height posframe)
                 :width ,(frame-width posframe)
                 :min-height nil
                 :min-width nil))))
    (embark-act arg)))

;;;###autoload
(cl-defun cae-avy-embark-act-on-region ()
  (interactive)
  (require 'avy)
  (save-window-excursion
    (let* ((initial-window (selected-window))
           (avy-indent-line-overlay t)  ;my preference
           (avy-action #'identity)
           (beg (avy--line))
           (end (if beg (avy--line)
                  (cl-return-from cae-avy-embark-act-on-region))))
      (unless end
        (cl-return-from cae-avy-embark-act-on-region))
      (when (> beg end)
        (cl-rotatef beg end))
      (setq beg (save-excursion
                  (goto-char beg)
                  (line-beginning-position)))
      (setq end (save-excursion
                  (goto-char end)
                  (1+ (line-end-position))))
      (save-mark-and-excursion
        (goto-char beg)
        (set-mark end)
        (activate-mark)
        (embark-act)))))

;;;###autoload
(defun cae-delete-duplicate-bookmarks ()
  (interactive)
  (let ((bookmarks (cl-remove-if-not (lambda (x)
                                       (string-match-p "<[0-9]+>\\'" x))
                                     (bookmark-all-names))))
    (dolist (bookmark bookmarks)
      (bookmark-delete bookmark))))

;;;###autoload
(defun cae-make-new-buffer ()
  (interactive)
  (let ((buffer (generate-new-buffer "*new*")))
    (set-window-buffer nil buffer)
    (with-current-buffer buffer
      (funcall (default-value 'major-mode))
      (setq doom-real-buffer-p t))))

;;;###autoload
(defun cae-narrow-to-page ()
  (interactive)
  (save-mark-and-excursion
    (end-of-line)
    (deactivate-mark)
    (logos-narrow-dwim)))

(defvar cae-bookmark-downloads-directory (expand-file-name "~/Downloads/"))

;;;###autoload
(defun cae-bookmark-jump-to-newest-download (_)
  ;; For backwards compatibility with my bookmarks file.
  (let ((newest-file (-max-by #'file-newer-than-file-p
                              (cl-remove-if
                               (lambda (file)
                                 (or (string-prefix-p "." (file-name-nondirectory file))
                                     (file-directory-p file)))
                               (cl-union (directory-files "~/Downloads/" t)
                                         (directory-files "~/" t))))))
    (dired (file-name-directory newest-file))
    (dired-goto-file newest-file)))

;;;###autoload
(defun cae-yank-indent-a (&rest _)
  (let ((this-command 'yank)
        (real-this-command 'yank))
    (yank-indent--post-command-hook)))

;;;###autoload
(defun cae-kill-region ()
  (interactive)
  (call-interactively #'kill-region)
  (delete-blank-lines))

;;;###autoload
(defun cae-avy-parrot-rotate-forward-action (pt)
  (save-mark-and-excursion
    (goto-char pt)
    (call-interactively #'parrot-rotate-next-word-at-point)))

;;;###autoload
(defun cae-avy-parrot-rotate-backward-action (pt)
  (save-mark-and-excursion
    (goto-char pt)
    (call-interactively #'parrot-rotate-prev-word-at-point)))

;;;###autoload
(defun cae-avy-rotate ()
  (interactive)
  (require 'parrot)
  (setq avy-action #'cae-avy-parrot-rotate-forward-action)
  (when-let* ((candidates
               (let ((res))
                 (cl-loop for words in parrot-rotate-dict
                          do (dolist (window (window-list) res)
                               (with-selected-window window
                                 (save-excursion
                                   (goto-char (window-start))
                                   (while (re-search-forward
                                           (regexp-opt (plist-get words :rot) 'symbols)
                                           (window-end nil t) t)
                                     (push (cons (bounds-of-thing-at-point 'symbol)
                                                 (selected-window))
                                           res))))))
                 res)))
    (avy-process candidates)))

;;;###autoload
(defun cae-mark-comment ()
  "Mark the entire comment around point. Like `er/mark-comment' but
also marks comment with leading whitespace"
  (interactive)
  (when (save-excursion
          (skip-syntax-forward "\s")
          (er--point-is-in-comment-p))
    (let ((p (point)))
      (skip-syntax-backward "\s")
      (while (and (or (er--point-is-in-comment-p)
                      (looking-at "[[:space:]]"))
                  (not (eobp)))
        (forward-char 1))
      (skip-chars-backward "\n\r")
      (set-mark (point))
      (goto-char p)
      (while (or (er--point-is-in-comment-p)
                 (looking-at "[[:space:]]"))
        (forward-char -1))
      (forward-char 1))))

;;;###autoload
(defun cae-embrace-with-prefix-function ()
  (let ((fname (read-string "Function: ")))
    (cons (format "(%s " (or fname "")) ")")))

(defun cae-modeline--rotate-word-at-point (rotate-function)
  (save-excursion
    (when-let* ((beg (car-safe (bounds-of-thing-at-point 'symbol))))
      (goto-char beg))
    (skip-syntax-forward "^w" (line-end-position))
    (condition-case err
        (call-interactively rotate-function)
      (error
       (skip-syntax-backward "^w" (line-beginning-position))
       (call-interactively rotate-function)))))

;;;###autoload
(defun cae-modeline-rotate-forward-word-at-point ()
  (interactive)
  (cae-modeline--rotate-word-at-point #'parrot-rotate-next-word-at-point))

;;;###autoload
(defun cae-modeline-rotate-backward-word-at-point ()
  (interactive)
  (cae-modeline--rotate-word-at-point #'parrot-rotate-prev-word-at-point))

(defvar cae-exwm-workspace-process-alist nil)

;;;###autoload
(defun cae-exwm-start-app (app workspace &optional arg)
  (when (modulep! :ui workspaces)
    (+workspace-switch workspace t))
  (when arg
    (kill-process (alist-get workspace cae-exwm-workspace-process-alist nil nil #'cl-equalp)))
  (unless (process-live-p (alist-get workspace cae-exwm-workspace-process-alist nil nil #'cl-equalp))
    (setf (alist-get workspace cae-exwm-workspace-process-alist nil nil #'cl-equalp)
          (start-process workspace nil app)))
  (+exwm-persp--focus-workspace-app))

(defvar cae-yank-point nil)
(defvar cae-yank-point-overlays nil)
(add-hook! 'minibuffer-exit-hook
  (defun cae-yank-on-exit-h ()
    (dolist (ov cae-yank-point-overlays)
      (delete-overlay ov))))

;;;###autoload
(defun cae-yank-word-to-minibuffer (arg)
  (interactive "p")
  (insert
   (replace-regexp-in-string
    "\\s-+" " "
    (with-minibuffer-selected-window
      (unless (and cae-yank-point (eq last-command this-command))
        (setq cae-yank-point (point))
        (cae-yank-on-exit-h))
      (save-excursion
        (let  ((beg (progn (goto-char cae-yank-point)
                           cae-yank-point))
               (end (setq cae-yank-point
                          (progn (forward-word)
                                 (point)))))
          (let ((ov (make-overlay beg end)))
            (push ov cae-yank-point-overlays)
            ;; 1000 is higher than ediff's 100+,
            ;; but lower than isearch main overlay's 1001
            (overlay-put ov 'priority 1000)
            (overlay-put ov 'face 'lazy-highlight))
          (save-excursion (buffer-substring-no-properties beg end))))))))

;;;###autoload
(defun cae-edit-indirect-dwim ()
  "DWIM version of edit-indirect-region.
When region is selected, behave like `edit-indirect-region'
but when no region is selected and the cursor is in a 'string' syntax
mark the string and call `edit-indirect-region' with it."
  (interactive)
  (cond ((region-active-p)
         (call-interactively #'edit-indirect-region))
        ((and (derived-mode-p 'org-mode)
              (ignore-error 'user-error (call-interactively #'org-edit-special))))
        ((nth 3 (sp--syntax-ppss)) (call-interactively #'string-edit-at-point))
        (t (save-mark-and-excursion
             (let ((pos (point))
                   (beg (progn (mark-defun) (region-beginning))))
               (call-interactively #'edit-indirect-region)
               (goto-char (- pos beg)))))))

;;;###autoload
(defun cae-kill-current-buffer ()
  (interactive)
  (when-let* ((proc (get-buffer-process (current-buffer))))
    ;; Stop AI from freezing Emacs while Emacs is waiting for the AI to respond.
    (set-process-sentinel proc nil))
  (call-interactively #'kill-current-buffer))


(defvar cae--sibling-file-history (make-hash-table :test 'equal)
  "A hash-table that keeps track of sibling file history.")

(defun cae-find-sibling-file (file)
  "Find a sibling file of FILE.

This function is a wrapper around `find-sibling-file' that also allows for
jumping backwards."
  (interactive (progn
                 (unless buffer-file-name
                   (user-error "Not visiting a file"))
                 (list buffer-file-name)))
  (let ((old-file (buffer-file-name)))
    (condition-case err (call-interactively #'find-sibling-file)
      (user-error
       (when (string-match-p "\\`Couldn['’]t find any sibling files\\'" (error-message-string err))
         (let ((previous-files (gethash old-file cae--sibling-file-history)))
           (cond ((null previous-files)
                  ;; no previous file recorded
                  nil)
                 ((stringp previous-files)
                  ;; one previous file, use it directly
                  (find-file previous-files))
                 ((listp previous-files)
                  ;; multiple previous files, let the user choose
                  (find-file
                   (completing-read
                    "Choose file: " previous-files nil t nil))))))))
    (unless (string= old-file (buffer-file-name))
      ;; append new sibling or create list if multiple siblings
      (let ((current-history (gethash (buffer-file-name) cae--sibling-file-history)))
        (puthash (buffer-file-name)
                 (cond ((null current-history)
                        ;; no entry exists, store the old-file
                        old-file)
                       ((stringp current-history)
                        (if (string= old-file current-history)
                            current-history
                          ;; one entry exists, make it a list
                          (list old-file current-history)))
                       ((listp current-history)
                        ;; a list already, append if not already there
                        (cons old-file (remove old-file current-history))))
                 cae--sibling-file-history)))))

;;;###autoload
(defun cae-jump-to-random-line ()
  "Jump to the end of a random line in the current buffer."
  (interactive)
  (push-mark)
  (goto-char (point-min))               ; Start at the beginning of the buffer
  (let ((line-count (count-lines (point-min) (point-max))))
    (unless (zerop line-count)
      (forward-line (random line-count))
      (if (derived-mode-p 'dired-mode)
          (dired-move-to-filename)
        (end-of-line)))))

;;;###autoload
(defun cae-embark-act ()
  (interactive)
  (require 'embark)
  (let ((embark-cycle-key (key-description (this-command-keys))))
    (call-interactively 'embark-act)))

;;;###autoload
(defun cae-workspace-switch-to-9 ()
  (interactive)
  (+workspace/switch-to 9))

;;;###autoload
(defun cae-workspace-switch-to-10 ()
  (interactive)
  (+workspace/switch-to 10))

;;;###autoload
(defun cae-complete-in-minibuffer ()
  (interactive)
  (let  ((completion-in-region-function #'consult-completion-in-region))
    (call-interactively #'complete-symbol)))

(defun cae-org-get-image-or-latex-filename-at-point ()
  "Get filename of org-mode image link, overlay or latex fragment.

Coppied org-mode section from ox-clip.el."
  (require 'ov)
  (let ((scale nil) (el (org-element-context)))
    (cond
     ;; condition on a latex fragment
     ((eq 'latex-fragment (org-element-type el))
      (when (ov-at) (org-toggle-latex-fragment))
      ;; should be no image, so we rebuild one
      (let ((current-scale (plist-get org-format-latex-options :scale))
	    ov display file relfile)
	(plist-put org-format-latex-options :scale
		   (or scale ox-clip-default-latex-scale))
	(org-toggle-latex-fragment)
	(plist-put org-format-latex-options :scale current-scale)
	(setq ov (ov-at)
	      display (overlay-get ov 'display)
	      file (plist-get (cdr display) :file))
	(file-relative-name file)))
     ;; condition t a link of an image
     ((and (eq 'link (org-element-type el))
	   (string= "file" (org-element-property :type el))
	   (string-match (cdr (assoc "file" org-html-inline-image-rules))
			 (org-element-property :path el)))
      (file-relative-name (org-element-property :path el)))
     ;; at an overlay with a display that is an image
     ((and (ov-at)
	   (overlay-get (ov-at) 'display)
	   (plist-get (cdr (overlay-get (ov-at) 'display)) :file)
	   (string-match (cdr (assoc "file" org-html-inline-image-rules))
			 (plist-get (cdr (overlay-get (ov-at) 'display))
				    :file)))
      (file-relative-name (plist-get (cdr (overlay-get (ov-at) 'display))
				     :file)))
     ;; not sure what else we can do here.
     (t
      nil))))

;;;###autoload
(defun cae-copy-image-to-clipboard (&optional image-file)
  "Copy image at point as clipboard image.

This function recognizes org-mode links, org-mode latex, dired-mode files and
image-mode buffers."
  (interactive)
  (let ((image-file
         (or image-file
             (cond
              ((derived-mode-p 'dired-mode) (dired-copy-filename-as-kill))
              ((derived-mode-p 'org-mode)
               (cae-org-get-image-or-latex-filename-at-point))
              ((derived-mode-p 'image-mode) (buffer-file-name))
              (t (let ((display (get-text-property (point) 'display)))
                   (when (eq 'image (car display))
                     (file-relative-name (plist-get (cdr display) :file)))))))))
    (when image-file
      (cond
       ((eq system-type 'windows-nt)
        (message "Not supported yet."))
       ((eq system-type 'darwin)
        (do-applescript
         (format "set the clipboard to POSIX file \"%s\"" (expand-file-name image-file))))
       ((eq system-type 'gnu/linux)
        (call-process-shell-command
         (format "xclip -selection clipboard -t image/%s -i %s"
                 (file-name-extension image-file)
                 image-file)))))
    (message "Copied %s" image-file)))

;;;###autoload
(defun cae-insert-bracket-pair ()
  (interactive)
  (let* ((start (point))
         (end (progn (insert "<>") (point)))
         (overlay (make-overlay start end))
         (keymap (make-sparse-keymap)))
    (forward-char -1)
    (move-overlay overlay start end)
    (overlay-put overlay 'keymap keymap)
    (define-key keymap (kbd "DEL")
      `(lambda ()
         (interactive)
         (let ((current (point)))
           (if (and (eq current (1+ ,start))
                    (eq (1+ current) ,end))
               (delete-region ,start ,end)
             (delete-char -1)))))))

;;;###autoload
(defun cae-call-leader-map ()
  (interactive)
  (setq unread-command-events (listify-key-sequence [menu])))

;;;###autoload
(defun cae-open-eshell-in-new-workspace ()
  "Open a new eshell in a new workspace."
  (interactive)
  (if (+workspace-exists-p "*eshell*")
      (+workspace-switch "*eshell*")
    (+workspace/new "*eshell*"))
  (eshell)
  (delete-other-windows))

;;;###autoload
(defun cae-open-vterm-in-new-workspace ()
  "Open a new vterm in a new workspace."
  (interactive)
  (if (+workspace-exists-p "*vterm*")
      (+workspace-switch "*vterm*")
    (+workspace/new "*vterm*"))
  (vterm)
  (delete-other-windows))
