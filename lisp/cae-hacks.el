;;; lisp/cae-hacks.el -*- lexical-binding: t; -*-


;;; GC hacks

(unless (fboundp 'igc--collect)
  (defconst cae-hacks-gc-cons-threshold (min (* 64 1024 1024 1024)
                                             (if (memory-info)
                                                 (/ (car (memory-info)) 3)
                                               ;; Fallback
                                               0)))
  (defconst cae-hacks-gc-idle-delay 20)
  (defvar cae-hacks--gc-messages nil)
  (defvar cae-hacks--gc-disabled nil)   ;Make these functions idempotent.
  (defvar cae-hacks--gcmh-mode nil)
  (defvar cae-hacks--gc-idle-timer nil)
  (defvar cae-hacks--gc-cons-threshold-old nil)

  ;; The purpose of these functions is to disable GC during long-running tasks
  ;; while showing GC messages if Emacs GCs anyways while running such a task.

  ;; Currently I only use this to prevent GC while running `kill-emacs-hook'.

  (defun cae-hacks-disable-gc (&rest _)
    "Raise the GC threshold to a large value and enable GC messages."
    (unless cae-hacks--gc-disabled
      (setq cae-hacks--gcmh-mode        (bound-and-true-p gcmh-mode))
      (and (fboundp #'gcmh-mode) (gcmh-mode -1))
      (setq cae-hacks--gc-messages      garbage-collection-messages
            garbage-collection-messages t
            cae-hacks--gc-cons-threshold-old gc-cons-threshold
            gc-cons-threshold           cae-hacks-gc-cons-threshold)
      (setq cae-hacks--gc-idle-timer
            (run-with-idle-timer cae-hacks-gc-idle-delay
                                 nil #'cae-hacks-garbage-collect))
      (when (timerp (bound-and-true-p gcmh-idle-timer))
        (cancel-timer gcmh-idle-timer))
      (add-hook 'post-gc-hook #'cae-hacks-enable-gc)
      (setq cae-hacks--gc-disabled t)))

  (defun cae-hacks-garbage-collect ()
    (garbage-collect)
    (cae-hacks-enable-gc))

  (defun cae-hacks-enable-gc ()
    "This is the inverse of `cae-hacks-disable-gc'.
It is meant to be used as a `post-gc-hook'."
    (when cae-hacks--gc-disabled
      (when (timerp cae-hacks--gc-idle-timer)
        (cancel-timer cae-hacks--gc-idle-timer))
      (setq garbage-collection-messages cae-hacks--gc-messages
            cae-hacks--gc-messages      nil
            cae-hacks--gcmh-mode        nil
            cae-hacks--gc-idle-timer    nil
            gc-cons-threshold           cae-hacks--gc-cons-threshold-old)
      (and (fboundp #'gcmh-mode) (gcmh-mode cae-hacks--gcmh-mode))
      (remove-hook 'post-gc-hook #'cae-hacks-enable-gc)
      (setq cae-hacks--gc-disabled nil)))

  (when (memory-info)
    (if (boundp 'after-focus-change-function)
        (add-function :after after-focus-change-function
                      (lambda ()
                        (unless (frame-focus-state)
                          (cae-hacks-garbage-collect))))
      (add-hook 'after-focus-change-function #'cae-hacks-garbage-collect))

    ;; Be wary of enabling this, especially on Android devices:
    ;; https://lists.gnu.org/archive/html/emacs-devel/2023-03/msg00431.html
    (add-hook 'kill-emacs-hook #'cae-hacks-disable-gc -10)
    (advice-add #'save-buffers-kill-emacs :before #'cae-hacks-disable-gc)
    (advice-add #'kill-emacs :before #'cae-hacks-disable-gc)))


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

;; Compile Vterm without asking.
(setq vterm-always-compile-module t)

;; Use the system's `libvterm' if available.
(setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes")

;; Make `eshell-previous-prompt' properly handle the case when there is no
;; previous prompt. Normally it goes to the beginning of the buffer. I prefer
;; for it to just stay on the first prompt.
(defadvice! cae-hacks-jump-back-if-bolp (oldfun &rest args)
  :around #'eshell-previous-prompt
  (let ((p (point)))
    (apply oldfun args)
    (when (bolp)
      (goto-char p))))

;; I made these to work around void function errors that I've seen once and
;; haven't seen since.
(autoload 'tramp-set-connection-local-variables-for-buffer "tramp")
(autoload 'tramp-command-completion-p "tramp")
(autoload 'org-eldoc-get-src-lang "org-eldoc")

;; For backwards compatibility.
(defun toggle-read-only (arg)
  (read-only-mode
   (cond ((not arg) (not buffer-read-only))
         ((and (integerp arg) (<= arg 0)) nil)
         (t t))))

;; For some reason, I had to do this after updating Emacs30 to get
;; `cape-yasnippet' to work.
(defalias 'prefix #'string-prefix-p)

;; Finally figured out where the `oddp' function went! Now those errors in Corfu
;; are gone.
(defalias 'oddp #'cl-oddp)

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

;; BUG If Eldoc tries to show help while Which Key is active, there is an error.
;; Inhibit `eldoc' when `which-key' is active to prevent errors.
(defadvice! cae-disable-eldoc-on-which-key-a ()
  :before-until #'eldoc-documentation-default
  (and (featurep 'which-key) (which-key--popup-showing-p)))

;; BUG When using `aider', which copies the font-lock-keywords, we get an error
;; with `whitespace-mode' since it's not copying the respective overlay.
(defadvice! cae-inhibit-whitespace-flush-in-aider-a (_)
  :before-while #'whitespace-point--flush-used
  (overlayp whitespace-point--used))

;; BUG I'm testing to see if this fixes an error with Corfu autocompletion.
(defadvice! cae-corfu-disable-if-no-candidates-a (_)
  :before-while #'corfu--candidates-popup
  (when (eq corfu--total 0)
    (message "HELLOOO"))
  (eq corfu--total 0))
