;;; ~/.doom.d/lisp/cae-corfu.el -*- lexical-binding: t; -*-

(setq +corfu-want-C-x-bindings nil)
(setq +corfu-want-minibuffer-completion 'aggressive)
(setq +corfu-want-ret-to-confirm 'minibuffer)
(setq +corfu-want-tab-prefer-expand-snippets t)
(setq +corfu-want-tab-prefer-navigating-snippets t)
(setq +corfu-want-tab-prefer-navigating-org-tables t)

(after! cape
  (setq cape-dabbrev-check-other-buffers t))
(after! corfu
  (setq corfu-auto-delay 0.05)
  (after! corfu-quick
    (setq corfu-quick1 (cae-keyboard-kbd "asdfgh")
          corfu-quick2 (cae-keyboard-kbd "jkluionm")))

  ;; Glue between Copilot and Corfu.
  (defun cae-corfu-quit ()
    (interactive)
    (let ((copilot-state (and (bound-and-true-p copilot-mode)
                              (copilot--overlay-visible))))
      (corfu-quit)
      (when copilot-state
        ;; Restore Copilot's overlay.
        (copilot-complete))))
  (add-hook! 'doom-escape-hook
    (defun cae-corfu-quit-h ()
      (when (cae-corfu-visible-p) (cae-corfu-quit) t)))
  (map! :map corfu-map
        [remap corfu-quit] #'cae-corfu-quit))

;; Wildcard separator
(defvar +orderless-wildcard-character ?,
  "A character used as a wildcard in Corfu for fuzzy autocompletion. If you
want to match the wildcard literally in completion, you can
escape it with forward slash. Do NOT set this to SPC.

This variable needs to be set at the top-level before any `after!' blocks.")

(when (and (modulep! :completion corfu +orderless)
           +orderless-wildcard-character)
  (defmacro +orderless-escapable-split-fn (char)
    (let ((char-string (string (if (symbolp char) (symbol-value char) char))))
      `(defun +orderless-escapable-split-on-space-or-char (s)
         (mapcar
          (lambda (piece)
            (replace-regexp-in-string
             (string 1) ,char-string
             (replace-regexp-in-string
              (concat (string 0) "\\|" (string 1))
              (lambda (x)
                (pcase x
                  ("\0" " ")
                  ("\1" ,char-string)
                  (_ x)))
              piece
              ;; These are arguments to `replace-regexp-in-string'.
              'fixedcase 'literal)
             'fixedcase 'literal))
          (split-string (replace-regexp-in-string
                         (concat "\\\\\\\\\\|\\\\ \\|\\\\"
                                 ,char-string)
                         (lambda (x)
                           (pcase x
                             ("\\ " "\0")
                             (,(concat "\\" char-string)
                              "\1")
                             (_ x)))
                         s 'fixedcase 'literal)
                        ,(concat "[ " char-string "]+")
                        t)))))
  ;; Orderless splits the string into components and then determines the
  ;; matching style for each component. This is all regexp stuff.
  (after! orderless
    (setq orderless-component-separator
          (+orderless-escapable-split-fn +orderless-wildcard-character)))
  (after! corfu
    (setq corfu-separator +orderless-wildcard-character)
    (keymap-set corfu-map (char-to-string +orderless-wildcard-character)
                #'+corfu-insert-wildcard-separator)
    ;; Quit completion after typing the wildcard followed by a space.
    (keymap-set corfu-map "SPC"
                `(menu-item "corfu-maybe-quit" nil
                  :filter
                  ,(lambda (_)
                     (when (and (> (point) (point-min))
                                (eq (char-before)
                                    +orderless-wildcard-character))
                       (corfu-quit)
                       nil))))
    ;; I do not need the module's command for inserting the wildcard.
    (map! :map corfu-map [remap completion-at-point] nil)))

;; Autocomplete words from the dictionary. Useful for typing country names!
(add-hook! (prog-mode conf-mode)
  (defun +corfu-add-cape-dict-h ()
    (add-hook 'completion-at-point-functions
              (cape-capf-inside-faces
               ;; Only call inside comments and docstrings.
               #'cape-dict 'tree-sitter-hl-face:doc 'font-lock-doc-face
               'font-lock-comment-face 'tree-sitter-hl-face:comment)
              40 t)))
(add-hook! text-mode
  (defun +corfu-add-cape-dict-text-h ()
    (add-hook 'completion-at-point-functions #'cape-dict 40 t)))

;; Autocomplete emoji.
(add-hook! (prog-mode conf-mode)
  (defun +corfu-add-cape-emoji-h ()
    (add-hook 'completion-at-point-functions
              (cape-capf-inside-faces
               (cape-capf-prefix-length #'cape-emoji 1)
               ;; Only call inside comments and docstrings.
               'tree-sitter-hl-face:doc 'font-lock-doc-face
               'font-lock-comment-face 'tree-sitter-hl-face:comment)
              10 t)))
(add-hook! text-mode
  (defun +corfu-add-cape-emoji-text-h ()
    (add-hook 'completion-at-point-functions
              (cape-capf-prefix-length #'cape-emoji 1) 10 t)))

;; Enable Fish autocompletion in `read-shell-command'.
(autoload 'turn-on-fish-completion-mode "fish-completion" nil t)
(advice-add #'shell-completion-vars :after #'turn-on-fish-completion-mode)
