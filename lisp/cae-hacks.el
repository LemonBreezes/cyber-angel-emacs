;;; lisp/cae-hacks.el -*- lexical-binding: t; -*-


;;; GC hacks

(defconst cae-hacks-gc-cons-threshold (* 64 1024 1024 1024))
(defconst cae-hacks-gc-cons-percentage 10)
(defconst cae-hacks-gc-idle-delay 20)
(defvar cae-hacks--gc-percentage nil)
(defvar cae-hacks--gc-messages nil)
(defvar cae-hacks--gc-disabled nil)     ;Make these functions idempotent.
(defvar cae-hacks--gcmh-mode nil)
(defvar cae-hacks--gc-idle-timer nil)

;; The purpose of these functions is to disable GC during long-running tasks
;; while showing GC messages if Emacs GCs anyways while running such a task.

;; Currently I only use this to prevent GC while running `kill-emacs-hook'.

(defun cae-hacks-disable-gc (&rest _)
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
(advice-add #'save-buffers-kill-emacs :before-until #'cae-hacks-disable-gc)
(advice-add #'kill-emacs :before-until #'cae-hacks-disable-gc)


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

;; These are for backwards compatibility.
(require 'cl-macs)
(cl-dolist (sym '(cae-keyboard-kbd
                  cae-keyboard-kbd-reverse
                  cae-keyboard-strings
                  cae-keyboard-remap
                  cae-keyboard-remap-reverse
                  cae-keyboard-remap-to-strings
                  cae-keyboard-remap-hydra-hint))
  (if (or (fboundp sym) (autoloadp sym)
          (bound-and-true-p cae-keyboard-remaps-enabled-p))
      (cl-return nil)
    (defalias sym #'identity)))

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

;;(add-hook! 'post-command-hook
;;  (defun cae-catch-buffers-out-of-sync-h ()
;;    (unless (eq (current-buffer) (window-buffer))
;;      (message "Buffer out of sync: %s" (buffer-name)))))

;; This is temporary until Org fixes the backgrounds issue.
(defun cae-org-set-font-lock-defaults ()
  "Set font lock defaults for the current buffer."
  (let ((org-font-lock-extra-keywords
         (list
          ;; Call the hook
          '(org-font-lock-hook)
          ;; Headlines
          `(,(if org-fontify-whole-heading-line
                 "^\\(\\**\\)\\(\\* \\)\\(.*\n?\\)"
               "^\\(\\**\\)\\(\\* \\)\\(.*\\)")
            (1 (org-get-level-face 1))
            (2 (org-get-level-face 2))
            (3 (org-get-level-face 3)))
          ;; Table lines
          '("^[ \t]*\\(\\(|\\|\\+-[-+]\\).*\\S-\\)\n?"
            (0 'org-table-row t)
            (1 'org-table t))
          ;; Table internals
          '("^[ \t]*|\\(?:.*?|\\)? *\\(:?=[^|\n]*\\)" (1 'org-formula t))
          '("^[ \t]*| *\\([#*]\\) *|" (1 'org-formula t))
          '("^[ \t]*|\\( *\\([$!_^/]\\) *|.*\\)|" (1 'org-formula t))
          '("| *\\(<[lrc]?[0-9]*>\\)" (1 'org-formula t))
          ;; Properties
          (list org-property-re
                '(1 'org-special-keyword t)
                '(3 'org-property-value t))
          ;; Drawers
          '(org-fontify-drawers)
          ;; Link related fontification.
          '(org-activate-links)
          (when (memq 'tag org-highlight-links) '(org-activate-tags (1 'org-tag prepend)))
          (when (memq 'radio org-highlight-links) '(org-activate-target-links (1 'org-link prepend)))
          (when (memq 'date org-highlight-links) '(org-activate-dates (0 'org-date prepend)))
          (when (memq 'footnote org-highlight-links) '(org-activate-footnote-links))
          ;; Targets.
          (list org-radio-target-regexp '(0 'org-target t))
          (list org-target-regexp '(0 'org-target t))
          ;; Diary sexps.
          '("^&?%%(.*\\|<%%([^>\n]*?>" (0 'org-sexp-date t))
          ;; Macro
          '(org-fontify-macros)
          ;; TODO keyword
          (list (format org-heading-keyword-regexp-format
                        org-todo-regexp)
                '(2 (org-get-todo-face 2) prepend))
          ;; TODO
          (when org-fontify-todo-headline
            (list (format org-heading-keyword-regexp-format
                          (concat
                           "\\(?:"
                           (mapconcat 'regexp-quote org-not-done-keywords "\\|")
                           "\\)"))
                  '(2 'org-headline-todo prepend)))
          ;; DONE
          (when org-fontify-done-headline
            (list (format org-heading-keyword-regexp-format
                          (concat
                           "\\(?:"
                           (mapconcat 'regexp-quote org-done-keywords "\\|")
                           "\\)"))
                  '(2 'org-headline-done prepend)))
          ;; Priorities
          '(org-font-lock-add-priority-faces)
          ;; Tags
          '(org-font-lock-add-tag-faces)
          ;; Tags groups
          (when (and org-group-tags org-tag-groups-alist)
            (list (concat org-outline-regexp-bol ".+\\(:"
                          (regexp-opt (mapcar 'car org-tag-groups-alist))
                          ":\\).*$")
                  '(1 'org-tag-group prepend)))
          ;; Special keywords
          (list (concat "\\<" org-deadline-string) '(0 'org-special-keyword t))
          (list (concat "\\<" org-scheduled-string) '(0 'org-special-keyword t))
          (list (concat "\\<" org-closed-string) '(0 'org-special-keyword t))
          (list (concat "\\<" org-clock-string) '(0 'org-special-keyword t))
          ;; Emphasis
          (when org-fontify-emphasized-text '(org-do-emphasis-faces))
          ;; Checkboxes
          `(,org-list-full-item-re 3 'org-checkbox prepend lax)
          (when (cdr (assq 'checkbox org-list-automatic-rules))
            '("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
              (0 (org-get-checkbox-statistics-face) prepend)))
          ;; Description list items
          '("\\(?:^[ \t]*[-+]\\|^[ \t]+[*]\\)[ \t]+\\(.*?[ \t]+::\\)\\([ \t]+\\|$\\)"
            1 'org-list-dt prepend)
          ;; Inline export snippets
          '("\\(@@\\)\\([a-z-]+:\\).*?\\(@@\\)"
            (1 'font-lock-comment-face t)
            (2 'org-tag t)
            (3 'font-lock-comment-face t))
          ;; ARCHIVEd headings
          (list (concat
                 org-outline-regexp-bol
                 "\\(.*:" org-archive-tag ":.*\\)")
                '(1 'org-archived prepend))
          ;; Specials
          '(org-do-latex-and-related)
          '(org-fontify-entities)
          '(org-raise-scripts)
          ;; Code
          '(org-activate-code (1 'org-code t))
          ;; COMMENT
          (list (format
                 "^\\*+\\(?: +%s\\)?\\(?: +\\[#[A-Z0-9]\\]\\)? +\\(?9:%s\\)\\(?: \\|$\\)"
                 org-todo-regexp
                 org-comment-string)
                '(9 'org-special-keyword prepend))
          ;; Blocks and meta lines
          '(org-fontify-meta-lines-and-blocks)
          '(org-fontify-inline-src-blocks)
          ;; Citations.  When an activate processor is specified, if
          ;; specified, try loading it beforehand.
          (progn
            (unless (null org-cite-activate-processor)
              (org-cite-try-load-processor org-cite-activate-processor))
            '(org-cite-activate))
          '(org-activate-folds))))
    (setq org-font-lock-extra-keywords (delq nil org-font-lock-extra-keywords))
    (run-hooks 'org-font-lock-set-keywords-hook)
    ;; Now set the full font-lock-keywords
    (setq-local org-font-lock-keywords org-font-lock-extra-keywords)
    (setq-local font-lock-defaults
                '(org-font-lock-keywords t nil nil backward-paragraph))
    (setq-local font-lock-extend-after-change-region-function
                #'org-fontify-extend-region)
    (kill-local-variable 'font-lock-keywords)
    nil))
(advice-add #'org-set-font-lock-defaults :override #'cae-org-set-font-lock-defaults)
