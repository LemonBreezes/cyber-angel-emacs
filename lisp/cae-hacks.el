;;; lisp/cae-hacks.el -*- lexical-binding: t; -*-


;;; Other hacks

;; Prevent the minibuffer from "glitching" the workspace switch.
(defadvice! cae-hacks-workspace-ignore-minibuffer-a (&rest _)
  :before-until #'+workspace/switch-to
  (when (minibuffer-window-active-p (selected-window))
    ;; Do not trigger the repeat map.
    (setq this-command 'ignore
          real-this-command 'ignore)))

;; Make `advice-remove' ignore the keyword argument
(defadvice! cae-hacks-advice-remove-ignore-keyword-args-a (args)
  :filter-args #'advice-remove
  (if (keywordp (nth 1 args))
      (list (nth 0 args) (nth 2 args))
    args))

;; If `try' is used before the package list is loaded, fetch it.
(defadvice! cae-hacks-try-package-refresh-contents-maybe (&rest _)
  :before #'try
  (unless package-archive-contents
    (package--archives-initialize)))

;; Make `eshell-previous-prompt' properly handle the case when there is no
;; previous prompt. Normally it goes to the beginning of the buffer. I prefer
;; for it to just stay on the first prompt.
(defadvice! cae-hacks-jump-back-if-bolp (oldfun &rest args)
  :around #'eshell-previous-prompt
  (let ((p (point)))
    (apply oldfun args)
    (when (bolp)
      (goto-char p))))

;; For backwards compatibility.
(defun toggle-read-only (arg)
  (read-only-mode
   (cond ((not arg) (not buffer-read-only))
         ((and (integerp arg) (<= arg 0)) nil)
         (t t))))

;; This is for backwards compatibility with some of my old bookmarks.
(defalias #'cae-exwm-firefox-bookmark-handler #'cae-browse-url-generic-bookmark-handler)
(defalias #'bookmark/jump-to-newest-download #'cae-bookmark-jump-to-newest-download)

;; For some reason I got a void variable error in `helm-system-packages' for
;; this.
(defvar helm-marked-buffer-name "*helm marked*")

;; This is for finding and fixing commands that leave the current buffer and the
;; window buffer out of sync.
;;(add-hook! 'post-command-hook
;;  (defun cae-catch-buffers-out-of-sync-h ()
;;    (unless (eq (current-buffer) (window-buffer))
;;      (message "Buffer out of sync: %s" (buffer-name)))))

;; Getting a void variable error with modus theme.
(defvar date-scheduled-subtle nil)

;; (void-variable chatgpt-shell-anthropic--make-url)
(defvar chatgpt-shell-anthropic--make-url nil)

;;Debugger entered--Lisp error: (no-catch emacs-version-changed nil)
(advice-add #'w3m-fix-melpa-installation :override #'ignore)

(defvar-keymap doom-leader-GitHub-map) ; Silence byte-compiler.

(defadvice! cae-handle-missing-xclip-program ()
  :before-until #'doom-init-clipboard-in-tty-emacs-h
  (and (memq system-type '(gnu gnu/linux gnu/kfreebsd))
       (not (executable-find "xclip"))))

;;let*: Symbol’s function definition is void: consult--async-split-style
(setf (symbol-function 'consult--async-split-style) (symbol-function 'ignore))

;; Ignore some annoying errors when the buffer gets inadvertently deleted.
;;(defadvice! cae-ignore-selecting-deleted-buffer-error-a (oldfun &rest args)
;;  "Ignore 'Selecting deleted buffer' errors in `lsp--read-char`."
;;  :around #'lsp--read-char
;;  (condition-case err
;;      (apply oldfun args)
;;    (error (unless (string-prefix-p "Selecting deleted buffer" (error-message-string err))
;;             (signal (car err) (cdr err))))))

;; BUG When using `aider', which copies the font-lock-keywords, we get an error
;; with `whitespace-mode' since it's not copying the respective overlay.
(defadvice! cae-inhibit-whitespace-flush-in-aider-a (_)
  :before-while #'whitespace-point--flush-used
  (overlayp whitespace-point--used))

;; BUG Something bad is happening while editing C++ code sometimes.
(defadvice! cae-debug-set-window-buffer-a (win buf &optional _)
  :before #'set-window-buffer
  (when (eq (get-buffer buf) eldoc--doc-buffer)
    (backtrace)))

;; BUG Fix void variable langelem in `c-langelem-pos'
(defvar langelem nil)

;; BUG Fix void function error
(setf (symbol-function (intern "")) 'llama)

(defun cae-projectile-project-root (&optional dir)
  "Retrieves the root directory of a project if available.
If DIR is not supplied its set to the current directory by default."
  (let ((dir (or dir default-directory)))
    ;; Back out of any archives, the project will live on the outside and
    ;; searching them is slow.
    (when (and (fboundp 'tramp-archive-file-name-archive)
               (tramp-archive-file-name-p dir))
      (setq dir (file-name-directory (tramp-archive-file-name-archive dir))))
    ;; the cached value will be 'none in the case of no project root (this is to
    ;; ensure it is not reevaluated each time when not inside a project) so use
    ;; cl-subst to replace this 'none value with nil so a nil value is used
    ;; instead
    (cl-subst nil 'none
              (or
               ;; if we've already failed to find a project dir for this
               ;; dir, and cached that failure, don't recompute
               (let* ((cache-key (format "projectilerootless-%s" dir))
                      (cache-value (gethash cache-key projectile-project-root-cache)))
                 cache-value)
               ;; if the file isn't local, and we're not connected, don't try to
               ;; find a root now now, but don't cache failure, as we might
               ;; re-connect.  The `is-local' and `is-connected' variables are
               ;; used to fix the behavior where Emacs hangs because of
               ;; Projectile when you open a file over TRAMP. It basically
               ;; prevents Projectile from trying to find information about
               ;; files for which it's not possible to get that information
               ;; right now.
               (let ((is-local (not (file-remote-p dir))) ;; `true' if the file is local
                     (is-connected (file-remote-p dir nil t))) ;; `true' if the file is remote AND we are connected to the remote
                 (unless (or is-local is-connected)
                   'none))
               ;; if the file is local or we're connected to it via TRAMP, run
               ;; through the project root functions until we find a project dir
               (cl-some
                (lambda (func)
                  (let* ((cache-key (format "%s-%s" func dir))
                         (cache-value (gethash cache-key projectile-project-root-cache)))
                    (if (and cache-value (file-exists-p cache-value))
                        cache-value
                      (let ((value (funcall func (file-truename dir))))
                        (message "HELLO! %s %s %s %s %s" cache-key cache-value (buffer-file-name) default-directory dir)
                        (puthash cache-key value projectile-project-root-cache)
                        value))))
                projectile-project-root-functions)
               ;; if we get here, we have failed to find a root by all
               ;; conventional means, and we assume the failure isn't transient
               ;; / network related, so cache the failure
               (let ((cache-key (format "projectilerootless-%s" dir)))
                 (puthash cache-key 'none projectile-project-root-cache)
                 'none)))))

(advice-remove #'projectile-project-root :override #'cae-projectile-project-root)
