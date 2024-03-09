;;; autoload/cae-editor.el -*- lexical-binding: t; -*-

;; I used to advise `kill-buffer' with this but stopped doing so since it always
;; caused problems to pop up elsewhere somehow.
;;(defun cae-kill-buffer-query-diff-a (orig-func &optional buffer-or-name)
;;  "Like `kill-buffer', but prompts to diff or save the buffer if it's modified."
;;  (setq buffer-or-name (or buffer-or-name (current-buffer)))
;;  (when (and (buffer-local-value 'buffer-file-name (get-buffer buffer-or-name))
;;             (buffer-modified-p (get-buffer buffer-or-name)))
;;    (when (+popup-window-p)
;;      (backtrace))
;;    (catch 'quit
;;      (save-window-excursion
;;        (with-current-buffer buffer-or-name
;;          (let ((done nil) (buf (current-buffer)))
;;            (while (not done)
;;              (let ((response (read-char-choice
;;                               (format "Save file %s? (y, n, d, q) "
;;                                       (buffer-file-name buf))
;;                               '(?y ?n ?d ?q))))
;;                (setq done (cond
;;                            ((eq response ?q) (throw 'quit nil))
;;                            ((eq response ?y) (save-buffer) t)
;;                            ((eq response ?n) (set-buffer-modified-p nil) t)
;;                            ((eq response ?d) (diff-buffer-with-file) nil))))))))))
;;  (funcall orig-func buffer-or-name))
;;
;;;###autoload
(defun cae-delete-char ()
  "Like `delete-char', but works on the region if active, and
deletes the following char if the sexps in the buffer are
unbalanced. Works with Lispy and Smartparens."
  (interactive)
  (let ((delete-fn
         (cond ((condition-case _
                    (scan-sexps (point-min) (point-max))
                  (scan-error t))
                #'delete-char)
               ((bound-and-true-p lispy-mode)
                #'lispy-delete)
               ((bound-and-true-p smartparens-mode)
                (if (region-active-p)
                    #'sp-delete-region
                  #'sp-delete-char))
               (t #'delete-char))))
    (call-interactively delete-fn)))

;;;###autoload
(defun cae-toggle-sudo ()
  "Toggle sudo access for the current file."
  (interactive)
  (let* ((file (or buffer-file-name
                   (when (or (derived-mode-p 'dired-mode)
                             (derived-mode-p 'wdired-mode))
                     default-directory)))
         (file-localname (file-remote-p file 'localname))
         (tramp-prefix (and file-localname
                            (string-remove-suffix file-localname file)))
         (sudo-prefix (format "/sudo:root@%s:" (file-remote-p file 'host)))
         (p (point)))
    (if (string-suffix-p sudo-prefix tramp-prefix)
        (find-file (concat (string-remove-suffix sudo-prefix tramp-prefix)
                           (tramp-file-local-name file)))
      (doom/sudo-this-file))
    (unless (buffer-modified-p)
      (goto-char p))))

;;;###autoload
(defun cae-insert-closing-paren ()
  "Inserts a closing paren if the sexps in the buffer are
unbalanced, otherwise acts like `self-insert-command'. Works with
Lispy."
  (interactive)
  (cond ((condition-case error
             (scan-sexps (point-min) (point-max))
           (scan-error t))
         (insert-char ?\)))
        ((bound-and-true-p lispy-mode)
         (call-interactively #'lispy-right-nostring))
        (t (call-interactively #'self-insert-command))))

;;;###autoload
(defun cae-eval-last-sexp (arg)
  ;; Call `pp-eval-last-sexp' when called with a negative
  ;; prefix argument
  (interactive "P")
  (cond ((or (eq arg '-)
             (and (numberp arg)
                  (< arg 0)))
         (funcall #'pp-eval-last-sexp (if (numberp arg) nil)))
        ((bound-and-true-p eros-mode)
         (funcall #'eros-eval-last-sexp arg))
        (t (funcall #'eval-last-sexp arg))))

;;;###autoload
(defun cae-eval-expression (arg)
  ;; Call `pp-eval-expression' when called with a negative
  ;; prefix argument
  (interactive "P")
  (cond ((or (eq arg '-)
             (and (numberp arg)
                  (< arg 0)))
         (setq current-prefix-arg (and (numberp arg)
                                       (- arg)))
         (call-interactively #'pp-eval-expression))
        (t (call-interactively #'eval-expression))))


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
          `((cae-embark-act-with-completing-read grid ,@(delq 'vertico-flat-mode (car vertico-multiform--stack))))))
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
  (save-window-excursion
    (let* ((initial-window (selected-window))
           (avy-indent-line-overlay t)  ;my preference
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
(defun cae-embark-act ()
  (interactive)
  (require 'embark)
  (let ((embark-cycle-key (key-description (this-command-keys))))
    (call-interactively 'embark-act)))

;;;###autoload
(defun cae-avy-do (action pt)
  (save-mark-and-excursion
    (goto-char pt)
    (cond ((or (eq avy-command 'avy-goto-line)
               (memq this-command '(avy-goto-line-above
                                    avy-goto-line-below)))
           (progn (goto-char (line-beginning-position))
                  (set-mark (point))
                  (goto-char (line-end-position))))
          ((eq this-command 'cae-avy-symbol-at-point)
           (er/mark-symbol))
          (t (if (fboundp 'eri/expand-region)
                 (eri/expand-region 1)
               (er/expand-region 1))))
    (call-interactively action))
  (run-at-time 0.0 nil
               (lambda ()
                 (cae-pop-mark)
                 (when (bound-and-true-p hl-line-mode)
                   (hl-line-highlight))
                 (when (bound-and-true-p beacon-mode)
                   (beacon-blink)))))

;;;###autoload
(defalias 'cae-avy-action-embark-act
  (apply-partially #'cae-avy-do #'embark-act))

;;;###autoload
(defalias 'cae-avy-action-kill
  (apply-partially #'cae-avy-do #'cae-kill-region))

;;;###autoload
(defun cae-kill-region ()
  (interactive)
  (call-interactively #'kill-region)
  (delete-blank-lines))

;;;###autoload
(defalias 'cae-avy-parrot-rotate-forward-dispatch
  (apply-partially #'cae-avy-do #'cae-modeline-rotate-forward-word-at-point))

;;;###autoload
(defalias 'cae-avy-parrot-rotate-backward-dispatch
  (apply-partially #'cae-avy-do #'cae-modeline-rotate-backward-word-at-point))

;;;###autoload
(defalias 'cae-avy-action-comment-dwim
  (apply-partially #'cae-avy-do
                   (lambda ()
                     (cond ((or (eq avy-command 'avy-goto-line)
                                (memq this-command '(avy-goto-line-above
                                                     avy-goto-line-below)))
                            (call-interactively #'comment-or-uncomment-region))
                           ((bound-and-true-p lispy-mode)
                            (deactivate-mark)
                            (lispy-comment))
                           (t (call-interactively #'comment-or-uncomment-region)))
                     (avy-pop-mark))))

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
    (when-let ((beg (car-safe (bounds-of-thing-at-point 'symbol))))
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
  (when-let (proc (get-buffer-process (current-buffer)))
    ;; Stop AI from freezing Emacs while Emacs is waiting for the AI to respond.
    (set-process-sentinel proc nil))
  (call-interactively #'kill-current-buffer))


(defvar cae--sibling-file-history (make-hash-table :test 'equal)
  "A hash-table that keeps track of sibling file history.")

(defun cae-find-sibling-file (file)
  "Find a sibling file of FILE.

This function is a wrapper around `find-file' that also allows for
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
