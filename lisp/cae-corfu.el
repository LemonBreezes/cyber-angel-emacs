;;; ~/.doom.d/lisp/cae-corfu.el -*- lexical-binding: t; -*-

(setq +corfu-want-C-x-bindings nil)

(after! cape
  (setq cape-dabbrev-check-other-buffers t))
(after! corfu
  (setq corfu-auto-delay 0.05
        tab-first-completion 'eol)
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
        (copilot-complete))))
  ;;(add-hook! 'doom-escape-hook
  ;;  (defun cae-corfu-quit-h ()
  ;;    (when (cae-corfu-visible-p) (cae-corfu-quit) t)))
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

;; Use my logic for enabling Corfu in the minibuffer:
;;(after! corfu
;;  (remove-hook 'minibuffer-setup-hook #'+corfu-enable-in-minibuffer))
;;(add-hook 'minibuffer-setup-hook #'cae-corfu-enable-in-minibuffer-h)
(setq +corfu-want-minibuffer-completion 'aggressive)
