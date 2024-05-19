;;; lisp/cae-hacks.el -*- lexical-binding: t; -*-


;;; GC hacks

(defconst cae-hacks-gc-cons-threshold (* 64 1024 1024 1024))
(defconst cae-hacks-gc-idle-delay 20)
(defvar cae-hacks--gc-messages nil)
(defvar cae-hacks--gc-disabled nil)     ;Make these functions idempotent.
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
(advice-add #'kill-emacs :before #'cae-hacks-disable-gc)


;;; Other hacks

;; Prevent the minibuffer from "glitching" the workspace switch.
(defadvice! cae-hacks-workspace-ignore-minibuffer-a (&rest _)
  :before-until #'+workspace/switch-to
  (minibuffer-window-active-p (selected-window)))

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

;; Fix `save-some-buffers' so that I can continue the command after quitting a
;; diff with "q".
(defadvice! cae-hacks-quit-view-mode-a (oldfun)
  :around #'+popup/quit-window
  (if view-mode
      (View-quit)
    (funcall oldfun)))
(advice-add #'meow-quit :around #'cae-hacks-quit-view-mode-a)

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
(defalias #'+exwm-firefox-bookmark-handler #'cae-browse-url-generic-bookmark-handler)
(defalias #'bookmark/jump-to-newest-download #'cae-bookmark-jump-to-newest-download)

;; For some reason I got a void variable error in `helm-system-packages' for
;; this.
(defvar helm-marked-buffer-name "*helm marked*")

;; Prevent a void variable error
(unless (boundp 'sp-lisp-modes)
  (defvar sp-lisp-modes
    '(cider-repl-mode clojure-mode clojurec-mode clojurescript-mode clojurex-mode
      common-lisp-mode emacs-lisp-mode eshell-mode fennel-mode
      fennel-repl-mode geiser-repl-mode gerbil-mode inf-clojure-mode
      inferior-emacs-lisp-mode inferior-lisp-mode
      inferior-scheme-mode lisp-interaction-mode lisp-mode
      monroe-mode racket-mode racket-repl-mode
      scheme-interaction-mode scheme-mode slime-repl-mode
      sly-mrepl-mode stumpwm-mode)))

;; This is for finding and fixing commands that leave the current buffer and the
;; window buffer out of sync.
;;(add-hook! 'post-command-hook
;;  (defun cae-catch-buffers-out-of-sync-h ()
;;    (unless (eq (current-buffer) (window-buffer))
;;      (message "Buffer out of sync: %s" (buffer-name)))))


