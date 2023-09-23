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
                                       (string-match-p "<[0-9]+>$" x))
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
(defun cae-auto-sudoedit-maybe-h ()
  (unless (let ((path (or (buffer-file-name) list-buffers-directory)))
            (string= (file-attribute-user-id
                      (file-attributes path 'string))
                     (if (and (featurep 'tramp)
                              (tramp-tramp-file-p path))
                         (tramp-get-remote-uid (tramp-dissect-file-name path)
                                               'string)
                       (user-login-name))))
    (require 'auto-sudoedit)
    (auto-sudoedit)))

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
  (unless (alist-get workspace cae-exwm-workspace-process-alist nil nil #'cl-equalp)
    (setf (alist-get workspace cae-exwm-workspace-process-alist nil nil #'cl-equalp)
          (start-process workspace nil app)))
  (+exwm-persp--focus-workspace-app))

;;;###autoload
(defun +org-insert-file-link ()
  "Insert a file link.  At the prompt, enter the filename."
  (interactive)
  (insert (format "[[%s]]" (org-link-complete-file))))
