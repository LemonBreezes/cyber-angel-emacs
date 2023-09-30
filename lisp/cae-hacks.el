;;; lisp/cae-hacks.el -*- lexical-binding: t; -*-


;;; GC hacks

(defconst cae-hacks-gc-cons-threshold (* 3 1024 1024 1024))
(defconst cae-hacks-gc-cons-percentage 10)
(defconst cae-hacks-gc-idle-delay 20)
(defvar cae-hacks--gc-percentage nil)
(defvar cae-hacks--gc-messages nil)
(defvar cae-hacks--gc-disabled nil)     ;Make these functions idempotent.
(defvar cae-hacks--gcmh-mode nil)
(defvar cae-hacks--gc-idle-timer nil)

;; The purpose of these functions is to disable GC during long-running tasks
;; while showing GC messages if Emacs GCs anyways while running such a task.

(defun cae-hacks-disable-gc ()
  "Raise the GC threshold to a large value and enable GC messages."
  (unless cae-hacks--gc-disabled
    (setq cae-hacks--gcmh-mode        (bound-and-true-p gcmh-mode))
    (and (fboundp #'gcmh-mode) (gcmh-mode -1))
    (setq cae-hacks--gc-messages      garbage-collection-messages
          cae-hacks--gc-percentage    gc-cons-percentage
          garbage-collection-messages t
          gc-cons-threshold           cae-hacks-gc-cons-threshold
          gc-cons-percentage          cae-hacks-gc-cons-percentage)
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
    (and (fboundp #'gcmh-mode) (gcmh-mode cae-hacks--gcmh-mode))
    (when (timerp cae-hacks--gc-idle-timer)
      (cancel-timer cae-hacks--gc-idle-timer))
    (setq garbage-collection-messages cae-hacks--gc-messages
          gc-cons-percentage          cae-hacks--gc-percentage
          cae-hacks--gc-messages      nil
          cae-hacks--gc-percentage    nil
          cae-hacks--gcmh-mode        nil
          cae-hacks--gc-idle-timer    nil)
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

;; This autoload fixes a void function error on `find-file-hook' that occurs
;; sporadically for me.
(autoload 'tramp-set-connection-local-variables-for-buffer "tramp")

;; Another error but during completion. Strange.
(autoload 'tramp-command-completion-p "tramp")

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

;; These are for backwards compatibility.
(dolist (sym '(cae-keyboard-strings
               cae-keyboard-remap
               cae-keyboard-remap-reverse
               cae-keyboard-remap-to-strings
               cae-keyboard-kbd
               cae-keyboard-kbd-reverse
               cae-keyboard-remap-hydra-hint))
  (defalias sym #'identity))

;; For some reason I got a void variable error in `helm-system-packages' for
;; this.
(defvar helm-marked-buffer-name "*helm marked*")

(unless (or (autoloadp 'turn-on-show-smartparens-mode)
            (fboundp 'turn-on-show-smartparens-mode))
  (defvar sp-lisp-modes
    '(cider-repl-mode clojure-mode clojurec-mode clojurescript-mode clojurex-mode
      common-lisp-mode emacs-lisp-mode eshell-mode fennel-mode
      fennel-repl-mode geiser-repl-mode gerbil-mode inf-clojure-mode
      inferior-emacs-lisp-mode inferior-lisp-mode
      inferior-scheme-mode lisp-interaction-mode lisp-mode
      monroe-mode racket-mode racket-repl-mode
      scheme-interaction-mode scheme-mode slime-repl-mode
      sly-mrepl-mode stumpwm-mode)))
