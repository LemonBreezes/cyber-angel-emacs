;;; lisp/cae-hacks.el -*- lexical-binding: t; -*-


;;; Other hacks

(defun cae-shut-up-a (fn &rest args)
  "Generic advisor for silencing noisy functions."
  (quiet!! (apply fn args)))

;; Prevent the minibuffer from "glitching" the workspace switch.
(cae-defadvice! cae-hacks-workspace-ignore-minibuffer-a (&rest _)
  :before-until #'+workspace/switch-to
  (when (minibuffer-window-active-p (selected-window))
    ;; Do not trigger the repeat map.
    (setq this-command 'ignore
          real-this-command 'ignore)))

;; Make `advice-remove' ignore the keyword argument
(cae-defadvice! cae-hacks-advice-remove-ignore-keyword-args-a (args)
  :filter-args #'advice-remove
  (if (keywordp (nth 1 args))
      (list (nth 0 args) (nth 2 args))
    args))

;; If `try' is used before the package list is loaded, fetch it.
(cae-defadvice! cae-hacks-try-package-refresh-contents-maybe (&rest _)
  :before #'try
  (unless package-archive-contents
    (package--archives-initialize)))

;; Make `eshell-previous-prompt' properly handle the case when there is no
;; previous prompt. Normally it goes to the beginning of the buffer. I prefer
;; for it to just stay on the first prompt.
(cae-defadvice! cae-hacks-jump-back-if-bolp (oldfun &rest args)
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
(cae-advice-add #'w3m-fix-melpa-installation :override #'ignore)

(defvar-keymap doom-leader-GitHub-map) ; Silence byte-compiler.

(cae-defadvice! cae-handle-missing-xclip-program ()
  :before-until #'doom-init-clipboard-in-tty-emacs-h
  (and (memq system-type '(gnu gnu/linux gnu/kfreebsd))
       (not (executable-find "xclip"))))

;; BUG Fix void variable langelem in `c-langelem-pos'
(defvar langelem nil)

;; BUG Fix void function error
(setf (symbol-function (intern "")) 'llama)

;; Always get a fresh command list so new commands show up immediately.
(cae-defadvice! cae-force-refresh-external-commands-a (&optional _)
  :before #'helm-external-commands-list-1
  (setq helm-external-commands-list nil))

;; Allow killing buffers that are visible in multiple persps
(advice-add #'persp-kill-buffer-query-function :override
            #'cae-persp-kill-buffer-query-function)

;; Work around a garbage change in Emacs31 that made viewing diffs not work in
;; `save-some-buffers'.
(after! diff-mode
  (put 'diff-mode 'mode-class nil))

(setq debug-on-message "[iI]nvalid face reference")

(advice-add #'doom-modeline--font-height :override
            (defun cae-const-24 () 24))
