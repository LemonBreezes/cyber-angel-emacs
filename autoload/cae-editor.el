;;; autoload/cae-editor.el -*- lexical-binding: t; -*-

;;; Helper functions

(defun cae--with-minibuffer-setup (setup-fn &rest args)
  "Run function with SETUP-FN in minibuffer setup hook, passing ARGS."
  (minibuffer-with-setup-hook setup-fn
    (apply args)))

(defun cae--save-position-and-execute (fn &rest args)
  "Execute FN with ARGS while preserving point and mark."
  (save-mark-and-excursion
    (apply fn args)))

(defun cae--get-file-at-point ()
  "Get filename at point based on current major mode."
  (cond
   ((derived-mode-p 'dired-mode) 
    (dired-get-filename nil t))
   ((derived-mode-p 'org-mode)
    (cae-org-get-image-or-latex-filename-at-point))
   ((derived-mode-p 'image-mode) 
    (buffer-file-name))
   (t (when-let ((display (get-text-property (point) 'display)))
        (when (eq 'image (car display))
          (file-relative-name (plist-get (cdr display) :file)))))))

(defun cae--get-vertico-posframe-size (posframe)
  "Return a function that gets the size of POSFRAME."
  (lambda (_)
    `(:height ,(frame-height posframe)
      :width ,(frame-width posframe)
      :min-height nil
      :min-width nil)))

;;; Text manipulation functions

(defun cae-strip-top-level-indentation (str)
  "Strip the top-level leading indentation for every line in STR.
The least indented line will have 0 leading whitespace. Convert tabs to spaces
using the tab-width variable."
  (let* ((lines (split-string (replace-regexp-in-string "\t" (make-string tab-width ?\s) str) "\n"))
         (non-empty-lines (cl-remove-if #'string-empty-p lines))
         (indentations (mapcar (lambda (line)
                                 (string-match "^[[:space:]]*" line)
                                 (match-end 0))
                               non-empty-lines))
         (min-indentation (if indentations (apply #'min indentations) 0)))
    (mapconcat
     (lambda (line)
       (if (and (not (string-empty-p line))
                (string-match (format "^[[:space:]]\\{%d\\}" min-indentation) line))
           (substring line min-indentation)
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
(defun cae-dos2unix ()
  "Convert DOS line endings (CRLF) to Unix line endings (LF)."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match "" nil t))))

;;;###autoload
(defun cae-kill-region ()
  "Kill region and clean up blank lines."
  (interactive)
  (call-interactively #'kill-region)
  (delete-blank-lines))

;;;###autoload
(defun cae-yank-indent-a (&rest _)
  "Advice to indent after yanking."
  (let ((this-command 'yank)
        (real-this-command 'yank))
    (yank-indent--post-command-hook)))

;;;###autoload
(defun cae-mark-comment ()
  "Mark the entire comment around point including leading whitespace.
Like `er/mark-comment' but also marks comment with leading whitespace."
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
  "Create a pair for embrace with function prefix."
  (let ((fname (read-string "Function: ")))
    (cons (format "(%s " (or fname "")) ")")))

;;;###autoload
(defun cae-insert-bracket-pair ()
  "Insert angle bracket pair and position cursor between them."
  (interactive)
  (let* ((start (point))
         (end (progn (insert "<>") (point)))
         (overlay (make-overlay start end))
         (keymap (make-sparse-keymap)))
    (forward-char -1)
    (move-overlay overlay start end)
    (overlay-put overlay 'keymap keymap)
    (define-key keymap (kbd "DEL")
      (lambda ()
        (interactive)
        (let ((current (point)))
          (if (and (eq current (1+ start))
                   (eq (1+ current) end))
              (delete-region start end)
            (delete-char -1)))))))

;;; Navigation and mark functions

;;;###autoload
(defun cae-pop-mark ()
  "Pop mark ring with prefix argument."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (setq this-command 'set-mark-command
          real-this-command 'set-mark-command)
    (call-interactively #'set-mark-command)))

;;;###autoload
(defun cae-exchange-point-and-mark ()
  "Enhanced version of `exchange-point-and-mark'.
Toggles the prefix argument based on region state."
  (interactive)
  (let ((current-prefix-arg
         (if (region-active-p)
             current-prefix-arg
           (pcase current-prefix-arg
             (`(4) nil)
             (_ '(4))))))
    (call-interactively #'exchange-point-and-mark)))

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
(defun cae-narrow-to-page ()
  "Narrow to the current page using logos."
  (interactive)
  (cae--save-position-and-execute
   (lambda ()
     (end-of-line)
     (deactivate-mark)
     (logos-narrow-dwim))))

;;; Buffer management functions

;;;###autoload
(defun cae-make-new-buffer ()
  "Create a new buffer named *new* with default major mode."
  (interactive)
  (let ((buffer (generate-new-buffer "*new*")))
    (set-window-buffer nil buffer)
    (with-current-buffer buffer
      (funcall (default-value 'major-mode))
      (setq doom-real-buffer-p t))))

;;;###autoload
(defun cae-kill-current-buffer ()
  "Kill current buffer, safely handling process buffers."
  (interactive)
  (let ((buf (current-buffer)))
    (when-let ((proc (get-buffer-process buf)))
      (set-process-sentinel proc nil)))
  (kill-current-buffer))

;;;###autoload
(defun cae-edit-indirect-dwim ()
  "DWIM version of edit-indirect-region.
When region is selected, behave like `edit-indirect-region'
but when no region is selected and the cursor is in a 'string' syntax
mark the string and call `edit-indirect-region' with it."
  (interactive)
  (cond ((region-active-p)
         (edit-indirect-region))
        ((and (derived-mode-p 'org-mode)
              (ignore-error 'user-error (org-edit-special))))
        ((nth 3 (sp--syntax-ppss))
         (string-edit-at-point))
        (t (let ((pos (point)))
             (mark-defun)
             (edit-indirect-region)
             (goto-char (- pos (region-beginning)))))))

;;; Rotation functions

(defvar cae-rotation-pairs
  '(("true" "false")
    ("True" "False")
    ("t" "nil")
    ("forward" "backward")
    ("yes" "no")
    ("Yes" "No")
    ("on" "off")
    ("On" "Off")
    ("up" "down")
    ("Up" "Down")
    ("left" "right")
    ("Left" "Right")
    ("width" "height")
    ("Width" "Height")
    ("horizontal" "vertical")
    ("Horizontal" "Vertical")
    ("&&" "||")
    ("and" "or")
    ("And" "Or")
    ("min" "max")
    ("Min" "Max")
    ("public" "private")
    ("Public" "Private")
    ("before" "after")
    ("Before" "After")
    ("+" "-")
    ("==" "!=")
    ("<" ">")
    ("<=" ">=")
    ("1" "0")
    ("enable" "disable")
    ("Enable" "Disable")
    ("enabled" "disabled")
    ("Enabled" "Disabled")
    ("first" "last")
    ("First" "Last")
    ("allow" "deny")
    ("Allow" "Deny")
    ("in" "out")
    ("In" "Out")
    ("dark" "light")
    ("Dark" "Light"))
  "Pairs of words that can be rotated between each other.")

(defun cae--get-rotation-function (direction)
  "Get the appropriate rotation function based on availability.
DIRECTION should be 'forward or 'backward."
  (cond
   ((and (featurep 'parrot) cae-init-editor-enabled-p)
    (if (eq direction 'forward)
        #'parrot-rotate-next-word-at-point
      #'parrot-rotate-prev-word-at-point))
   (t
    (if (eq direction 'forward)
        #'cae-rotate-word-forward-internal
      #'cae-rotate-word-backward-internal))))

;;;###autoload
(defun cae-rotate-word-at-point (direction)
  "Rotate word at point in DIRECTION (1 for forward, -1 for backward).
This is a fallback for when parrot is not available."
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (word (and bounds (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (case-fold-search nil))
    (when (and bounds word)
      (let* ((group (cl-find-if (lambda (group) (member word group)) cae-rotation-pairs))
             (pos (when group (cl-position word group :test 'string=)))
             (len (when group (length group)))
             (next-pos (when (and pos len)
                         (mod (+ pos direction) len)))
             (next-word (when (and group next-pos)
                          (nth next-pos group))))
        (when next-word
          (delete-region (car bounds) (cdr bounds))
          (insert next-word)
          t)))))

;; Internal implementation functions
(defun cae-rotate-word-forward-internal ()
  "Internal implementation to rotate word forward."
  (interactive)
  (unless (cae-rotate-word-at-point 1)
    (message "No rotation found for word at point")))

(defun cae-rotate-word-backward-internal ()
  "Internal implementation to rotate word backward."
  (interactive)
  (unless (cae-rotate-word-at-point -1)
    (message "No rotation found for word at point")))

;;;###autoload
(defun cae-rotate-word-forward ()
  "Rotate word at point forward through a predefined list."
  (interactive)
  (call-interactively (cae--get-rotation-function 'forward)))

;;;###autoload
(defun cae-rotate-word-backward ()
  "Rotate word at point backward through a predefined list."
  (interactive)
  (call-interactively (cae--get-rotation-function 'backward)))

;;; Avy rotation functions

(defun cae--get-rotation-candidates ()
  "Get candidates for rotation from visible windows."
  (if (and cae-init-editor-enabled-p
           (require 'parrot nil t))
      ;; Use parrot dictionary when available
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
        res)
    ;; Fallback to our own rotation pairs
    (let* ((all-words (apply #'append cae-rotation-pairs))
           (regexp (regexp-opt all-words 'symbols))
           (candidates))
      (dolist (window (window-list) candidates)
        (with-selected-window window
          (save-excursion
            (goto-char (window-start))
            (while (re-search-forward regexp (window-end nil t) t)
              (push (cons (bounds-of-thing-at-point 'symbol)
                          (selected-window))
                    candidates)))))
      candidates)))

(defun cae-avy-rotate-action (rotate-fn pt)
  "Apply ROTATE-FN at point PT using avy."
  (cae--save-position-and-execute
   (lambda ()
     (goto-char pt)
     (call-interactively rotate-fn))))

;;;###autoload
(defun cae-avy-rotate-forward-action (pt)
  "Rotate word forward at point PT using parrot or fallback."
  (cae-avy-rotate-action 
   (cae--get-rotation-function 'forward)
   pt))

;;;###autoload
(defun cae-avy-rotate-backward-action (pt)
  "Rotate word backward at point PT using parrot or fallback."
  (cae-avy-rotate-action 
   (cae--get-rotation-function 'backward)
   pt))

;;;###autoload
(defun cae-avy-rotate ()
  "Use avy to select and rotate words from rotation dictionary."
  (interactive)
  (setq avy-action #'cae-avy-rotate-forward-action)
  (when-let ((candidates (cae--get-rotation-candidates)))
    (avy-process candidates)))

;;; Rotation UI functions

(defun cae--rotate-word-at-point (rotate-function)
  "Apply ROTATE-FUNCTION to word at point.
Tries to find a suitable word to rotate, even if point is not directly on it."
  (save-excursion
    (when-let* ((beg (car-safe (bounds-of-thing-at-point 'symbol))))
      (goto-char beg))
    (skip-syntax-forward "^w" (line-end-position))
    (condition-case _err
        (call-interactively rotate-function)
      (error
       (skip-syntax-backward "^w" (line-beginning-position))
       (call-interactively rotate-function)))))

;;;###autoload
(defun cae-rotate-forward-word-at-point ()
  "Rotate word forward at point."
  (interactive)
  (cae--rotate-word-at-point (cae--get-rotation-function 'forward)))

;;;###autoload
(defun cae-rotate-backward-word-at-point ()
  "Rotate word backward at point."
  (interactive)
  (cae--rotate-word-at-point (cae--get-rotation-function 'backward)))

;;; Workspace and EXWM functions

(defvar cae-exwm-workspace-process-alist nil
  "Association list mapping workspaces to their processes.")

;;;###autoload
(defun cae-exwm-start-app (app workspace &optional arg)
  "Start APP in WORKSPACE, killing existing process with ARG."
  (when (modulep! :ui workspaces)
    (+workspace-switch workspace t))
  (let ((proc (alist-get workspace cae-exwm-workspace-process-alist nil nil #'cl-equalp)))
    (when arg
      (kill-process proc))
    (unless (process-live-p proc)
      (setf (alist-get workspace cae-exwm-workspace-process-alist nil nil #'cl-equalp)
            (start-process workspace nil app))))
  (cae-exwm-persp--focus-workspace-app))

(defun cae--open-terminal-in-new-workspace (name terminal-func)
  "Open terminal using TERMINAL-FUNC in a new workspace with NAME."
  (if (+workspace-exists-p name)
      (+workspace-switch name)
    (+workspace/new name))
  (funcall terminal-func)
  (delete-other-windows)
  (persp-add-buffer (current-buffer)))

;;;###autoload
(defun cae-open-eshell-in-new-workspace ()
  "Open eshell in a new workspace."
  (interactive)
  (cae--open-terminal-in-new-workspace "*eshell*" #'eshell))

;;;###autoload
(defun cae-open-vterm-in-new-workspace ()
  "Open vterm in a new workspace."
  (interactive)
  (cae--open-terminal-in-new-workspace "*vterm*" #'vterm))

;;;###autoload
(defun cae-workspace-switch-to-9 ()
  "Switch to workspace 9."
  (interactive)
  (+workspace/switch-to 9))

;;;###autoload
(defun cae-workspace-switch-to-10 ()
  "Switch to workspace 10."
  (interactive)
  (+workspace/switch-to 10))

;;; Minibuffer and completion functions

(defvar cae-yank-point nil
  "Point from which to yank text.")
(defvar cae-yank-point-overlays nil
  "Overlays used to highlight yanked text.")

(defun cae-yank-cleanup-overlays ()
  "Clean up yank overlays."
  (mapc #'delete-overlay cae-yank-point-overlays)
  (setq cae-yank-point-overlays nil))

(add-hook! 'minibuffer-exit-hook #'cae-yank-cleanup-overlays)

;;;###autoload
(defun cae-yank-word-to-minibuffer (arg)
  "Yank word at point from the minibuffer's original buffer.
With prefix ARG, yank multiple words."
  (interactive "p")
  (insert
   (replace-regexp-in-string
    "\\s-+" " "
    (with-minibuffer-selected-window
      (unless (and cae-yank-point (eq last-command this-command))
        ;; Move to beginning of symbol at point when starting
        (if (thing-at-point 'symbol)
            (setq cae-yank-point (car (bounds-of-thing-at-point 'symbol)))
          (setq cae-yank-point (point)))
        (cae-yank-cleanup-overlays))
      (save-excursion
        (goto-char cae-yank-point)
        (let* ((beg cae-yank-point)
               (end (progn (forward-word) (point)))
               (text (buffer-substring-no-properties beg end))
               (ov (make-overlay beg end)))
          (setq cae-yank-point end)
          (push ov cae-yank-point-overlays)
          ;; 1000 is higher than ediff's 100+,
          ;; but lower than isearch main overlay's 1001
          (overlay-put ov 'priority 1000)
          (overlay-put ov 'face 'lazy-highlight)
          text))))))

;;;###autoload
(defun cae-bind-C-z-to-abort-a (oldfun &rest args)
  "Advice to bind C-z to abort in minibuffer for OLDFUN with ARGS."
  (cae--with-minibuffer-setup
   (lambda ()
     (local-set-key (kbd "C-z") #'abort-recursive-edit))
   oldfun args))

;;;###autoload
(defun cae-complete-in-minibuffer ()
  "Complete symbol in minibuffer using consult."
  (interactive)
  (let ((completion-in-region-function #'consult-completion-in-region))
    (call-interactively #'complete-symbol)))

;;;###autoload
(defun cae-call-leader-map ()
  "Call the leader key map."
  (interactive)
  (setq unread-command-events (listify-key-sequence [menu])))

;;; Embark functions

;;;###autoload
(defun cae-embark-act-with-completing-read (&optional arg)
  "Run embark-act with completing-read prompter and ARG."
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
            (cae--get-vertico-posframe-size posframe)))
    (cae--with-minibuffer-setup
     (lambda ()
       (local-set-key (kbd "C-z")
                      (lambda () (interactive)
                        (run-at-time 0.0 nil
                                     (lambda ()
                                       (vertico--exhibit)))
                        (abort-recursive-edit))))
     #'embark-act arg)))

;;;###autoload
(defun cae-embark-act ()
  "Call embark-act with current key as cycle key."
  (interactive)
  (require 'embark)
  (let ((embark-cycle-key (key-description (this-command-keys))))
    (call-interactively 'embark-act)))

;;;###autoload
(defun cae-avy-embark-act-on-region ()
  "Use avy to select a region and then run embark-act on it."
  (interactive)
  (require 'avy)
  (save-window-excursion
    (let* ((initial-window (selected-window))
           (avy-indent-line-overlay t)  ; my preference
           (avy-action #'identity)
           (beg (avy--line)))
      (if (not beg)
          nil
        (let ((end (avy--line)))
          (if (not end)
              nil
            (when (> beg end)
              (cl-rotatef beg end))
            (setq beg (save-excursion (goto-char beg)
                                      (line-beginning-position)))
            (setq end (save-excursion (goto-char end)
                                      (1+ (line-end-position))))
            (cae--save-position-and-execute
             (lambda ()
               (goto-char beg)
               (set-mark end)
               (activate-mark)
               (embark-act)))))))))

;;; Bookmark functions

;;;###autoload
(defun cae-browse-url-generic-bookmark-handler (bookmark)
  "Bookmark handler for opening URLs with `browse-url-generic'."
  (require 'ffap)
  (let ((url (bookmark-prop-get bookmark 'filename)))
    (if (ffap-url-p url)
        (browse-url-generic url)
      (message "Bookmark does not have a valid FILENAME property."))))

;;;###autoload
(defun cae-delete-duplicate-bookmarks ()
  "Delete bookmarks with numeric suffixes like <1>, <2>, etc."
  (interactive)
  (let ((bookmarks (cl-remove-if-not (lambda (x)
                                       (string-match-p "<[0-9]+>\\'" x))
                                     (bookmark-all-names))))
    (dolist (bookmark bookmarks)
      (bookmark-delete bookmark))))

(defvar cae-bookmark-downloads-directory (expand-file-name "~/Downloads/")
  "Directory path for downloads.")

;;;###autoload
(defun cae-bookmark-jump-to-newest-download (_)
  "Jump to the newest downloaded file in dired.
For backwards compatibility with bookmarks file."
  (let* ((download-files (directory-files "~/Downloads/" t))
         (home-files (directory-files "~/" t))
         (all-files (cl-union download-files home-files))
         (filtered-files (cl-remove-if
                          (lambda (file)
                            (or (string-prefix-p "." (file-name-nondirectory file))
                                (file-directory-p file)))
                          all-files))
         (newest-file (-max-by #'file-newer-than-file-p filtered-files)))
    (dired (file-name-directory newest-file))
    (dired-goto-file newest-file)))

;;; Sibling file functions

(defvar cae--sibling-file-history (make-hash-table :test 'equal)
  "A hash-table that keeps track of sibling file history.")

(defun cae--update-sibling-history (old-file new-file)
  "Update sibling history from OLD-FILE to NEW-FILE."
  (let ((current-history (gethash new-file cae--sibling-file-history)))
    (puthash new-file
             (cond ((null current-history) old-file)
                   ((stringp current-history)
                    (if (string= old-file current-history)
                        current-history
                      (list old-file current-history)))
                   ((listp current-history)
                    (cons old-file (remove old-file current-history))))
             cae--sibling-file-history)))

;;;###autoload
(defun cae-find-sibling-file (file)
  "Find a sibling file of FILE.
This function is a wrapper around `find-sibling-file' that also allows for
jumping backwards through the history of visited sibling files."
  (interactive (list (or buffer-file-name
                         (user-error "Not visiting a file"))))
  (let ((old-file (buffer-file-name)))
    (condition-case err 
        (call-interactively #'find-sibling-file)
      (user-error
       (when (string-match-p "\\`Couldn['']t find any sibling files\\'" (error-message-string err))
         (let ((previous-files (gethash old-file cae--sibling-file-history)))
           (cond 
            ((null previous-files)
             (signal (car err) (cdr err))) ; Re-signal the original error
            ((stringp previous-files)
             (find-file previous-files))
            ((listp previous-files)
             (find-file
              (completing-read "Choose previous sibling: " previous-files nil t nil))))))))
    (unless (string= old-file (buffer-file-name))
      (cae--update-sibling-history old-file (buffer-file-name)))))

;;; Clipboard and image functions

;;;###autoload
(defun cae-org-get-image-or-latex-filename-at-point ()
  "Get filename of org-mode image link, overlay or latex fragment.
Copied from org-mode section in ox-clip.el."
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
image-mode buffers. Optional IMAGE-FILE can be provided directly."
  (interactive)
  (let ((image-file (or image-file (cae--get-file-at-point))))
    (if (not image-file)
        (user-error "No image found at point")
      (let ((full-path (expand-file-name image-file)))
        (pcase system-type
          ('windows-nt
           (message "Not supported on Windows yet."))
          ('darwin
           (do-applescript
            (format "set the clipboard to POSIX file \"%s\"" full-path)))
          ('gnu/linux
           (call-process-shell-command
            (format "xclip -selection clipboard -t image/%s -i %s"
                    (file-name-extension image-file)
                    (shell-quote-argument full-path))))))
      (message "Copied %s" image-file))))

;;; Ispell functions

(defun cae-ispell-simple-get-word ()
  "Get the word at point for ispell."
  (require 'ispell)
  (car-safe (save-excursion (ispell-get-word nil))))

;;;###autoload
(defun cae-ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (require 'ispell)
  (let (original corrected)
    (save-excursion
      (while (and (not (bobp))
                  (not (and original 
                            (setq corrected (cae-ispell-simple-get-word))
                            (not (equal original corrected)))))
        (setq original (cae-ispell-simple-get-word))
        (if original
            (unless (ispell-word nil 'quiet)
              (backward-word)
              (backward-char))
          (backward-word)
          (backward-char))))
    
    (if (and original corrected (not (equal original corrected)))
        (let ((original (downcase original))
              (corrected (downcase corrected)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            original corrected)
          (message "\"%s\" now expands to \"%s\" %sally"
                   original corrected (if p "loc" "glob")))
      (user-error "No typo at or before point"))))
