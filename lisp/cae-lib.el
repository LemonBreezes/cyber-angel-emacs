;;; lisp/cae-lib.el -*- lexical-binding: t; -*-

(defmacro cae-defun (name arglist &optional docstring &rest body)
  "`defun' but guaranteed return the created function."
  (declare (doc-string 3) (indent 2))
  `(progn (defun ,name ,arglist ,docstring ,@body)
          #',name))

(defun cae-ignore-errors-a (fun &rest args)
  "Ignore errors in FUN with ARGS."
  (ignore-errors (apply fun args)))

(defun cae-display-graphic-p ()
  (and (display-graphic-p)
       (not (daemonp))))

(defun cae-which-key-inhibit-hook ()
  (setq which-key-inhibit nil)
  (remove-hook 'pre-command-hook
               #'cae-which-key-inhibit-hook))

(defun cae-which-key-show-map (keymap)
  (setq which-key-inhibit t)
  (add-hook 'pre-command-hook #'cae-which-key-inhibit-hook)
  (run-with-idle-timer
   which-key-idle-delay nil
   `(lambda ()
      (when which-key-inhibit
        (which-key-show-keymap
         ',keymap)))))

;; For backwards compatibility. `syslog-mode' uses this.
(defun toggle-read-only (arg)
  (read-only-mode
   (cond ((not arg) (not buffer-read-only))
         ((and (integerp arg) (<= arg 0)) nil)
         (t t))))

;; For some reason, I had to do this after updating Emacs30 to get
;; `cape-yasnippet' to work.
(defalias 'prefix #'string-prefix-p)

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
