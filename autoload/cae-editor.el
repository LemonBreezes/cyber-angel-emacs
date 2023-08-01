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
         (cond ((condition-case error
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
         (sudo-prefix (format "/sudo:root@%s:" (file-remote-p file 'host))))
    (if (string-suffix-p sudo-prefix tramp-prefix)
        (find-file (concat (string-remove-suffix sudo-prefix tramp-prefix)
                           (tramp-file-local-name file)))
      (doom/sudo-this-file))))

;;;###autoload
(defun cae-raise-sexp ()
  "Like `sp-raise-sexp', but works on the region if active."
  (interactive)
  (if (region-active-p)
      (save-excursion
        (let ((beg (region-beginning))
              (end (region-end)))
          (goto-char end)
          (delete-region end (progn (sp-up-sexp) (point)))
          (goto-char beg)
          (delete-region beg (progn (sp-backward-up-sexp) (point)))))
    (call-interactively #'sp-raise-sexp)))

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
(defun cae-avy-symbol-at-point ()
  "Jump to another occurance of symbol with avy."
  (interactive)
  (avy-with symbol-overlay-jump-avy
    (avy-process
     (avy--regex-candidates (regexp-quote (thing-at-point 'symbol t))))))

;;;###autoload
(defun cae-eval-last-sexp (arg)
  ;; Call `pp-eval-last-sexp' when called with a negative
  ;; prefix argument
  (interactive "P")
  (cond ((or (eq arg '-)
             (and (numberp arg)
                  (< arg 0)))
         (require 'pp+)
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
         (require 'pp+)
         (setq current-prefix-arg (and (numberp arg)
                                       (- arg)))
         (call-interactively #'pp-eval-expression))
        (t (call-interactively #'eval-expression))))


;;;###autoload
(defun cae-tab-close-and-select-right ()
  (interactive)
  (let ((tab-bar-close-tab-select 'right))
    (call-interactively #'tab-close)))

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
(defun cae-forward-sentence-function (&optional arg)
  (if (fboundp #'sentex-forward-sentence)
      (progn (or arg (setq arg 1))
             (if (< arg 0)
                 (sentex-backward-sentence arg)
               (sentex-forward-sentence arg)))
    (forward-sentence-default-function arg)))

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
        (t
         (call-interactively #'string-edit-at-point))))

;;;###autoload
(defun cae-browse-url-generic-bookmark-handler (bookmark)
  "Bookmark handler for opening URLs with `browse-url-generic'."
  (let ((url (bookmark-prop-get bookmark 'filename)))
    (if (ffap-url-p url)
        (browse-url-generic url)
      (message "Bookmark does not have a valid FILENAME property."))))

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
(defun cae-lookup-definition-dwim ()
  (interactive)
  (require 'ffap)
  (if-let ((file (ffap-file-at-point)))
      (if (and (file-exists-p file)
               (not (and buffer-file-name
                         (string= (file-truename file)
                                  (file-truename buffer-file-name)))))
          (progn (better-jumper-set-jump (marker-position (point-marker)))
                 (find-file file))
        (call-interactively #'ffap))
    (call-interactively #'+lookup/definition)))

;;;###autoload
(defun cae-lookup-definition-dwim (identifier &optional arg)
  (interactive (list (doom-thing-at-point-or-region)
                     current-prefix-arg))
  (require 'ffap)
  (cond ((null identifier) (user-error "Nothing under point"))
        ((and (string-match-p "/" identifier)
              (file-exists-p identifier)
              (not (and buffer-file-name
                        (file-equal-p identifier buffer-file-name))))
         (progn (better-jumper-set-jump (marker-position (point-marker)))
                (find-file identifier)))
        ((+lookup--jump-to :definition identifier nil arg))
        ((user-error "Couldn't find the definition of %S" (substring-no-properties identifier)))))

;;;###autoload
(defun cae-dos2unix ()
  "Automate M-% C-q C-m RET C-q C-j RET"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match "" nil t))))

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
          (t (eri/expand-region 1)))
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
(defalias 'cae-avy-parrot-rotate
  (apply-partially #'cae-avy-do #'cae-modeline-rotate-forward-word-at-point))

;;;###autoload
(defalias 'cae-avy-parrot-rotate-backward
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
(defun cae-pop-mark ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (setq this-command 'set-mark-command
          real-this-command 'set-mark-command)
    (call-interactively #'set-mark-command)))

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
         (vertico-posframe-size-function))
    (setf vertico-posframe-size-function
          `(lambda (_)
             '(:height ,(frame-height posframe)
               :width ,(frame-width posframe)
               :min-height nil
               :min-width nil)))
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
                                       (string-match-p "<[0-9]+>$" x))
                                     (bookmark-all-names))))
    (dolist (bookmark bookmarks)
      (bookmark-delete bookmark))))

;;;###autoload
(defun cae-jinx--add-to-abbrev (overlay word)
  "Add abbreviation to `global-abbrev-table'.
The misspelled word is taken from OVERLAY.  WORD is the corrected word."
  (let ((abbrev (buffer-substring-no-properties
                 (overlay-start overlay)
                 (overlay-end overlay))))
    (message "Abbrev: %s -> %s" abbrev word)
    (define-abbrev global-abbrev-table abbrev word)))

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

(defun cae-hippie-expand-completions (&optional hippie-expand-function)
  "Return the full list of possible completions generated by `hippie-expand'.
    The optional argument can be generated with `make-hippie-expand-function'."
  (let ((this-command 'cae-hippie-expand-completions)
        (last-command last-command)
        (buffer-modified (buffer-modified-p))
        (hippie-expand-function (or hippie-expand-function 'hippie-expand)))
    (cl-letf (((symbol-function #'ding) (symbol-function #'ignore))) ; avoid the (ding) when hippie-expand exhausts its options.
      (while (progn
               (funcall hippie-expand-function nil)
               (setq last-command 'cae-hippie-expand-completions)
               (not (equal he-num -1)))))
    ;; Evaluating the completions modifies the buffer, however we will finish
    ;; up in the same state that we began.
    (set-buffer-modified-p buffer-modified)
    ;; Provide the options in the order in which they are normally generated.
    (delete he-search-string (reverse he-tried-table))))

(defun cae-hippie-expand-with (hippie-expand-function)
  "Offer ido-based completion using the specified hippie-expand function."
  (let* ((options (cae-hippie-expand-completions hippie-expand-function))
         (selection (and options
                         (helm-completing-read "Completions: " options))))
    (if selection
        (progn (he-substitute-string selection t)
               (when (condition-case error
                         (scan-sexps (point-min) (point-max))
                       (scan-error t))
                 (delete-region (point) (point-at-eol))))
      (message "No expansion found"))))

;;;###autoload
(defun cae-hippie-expand ()
  "Offer ido-based completion for the word at point."
  (interactive)
  (cae-hippie-expand-with 'hippie-expand))
