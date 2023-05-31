;;; lisp/cae-multiple-cursors.el -*- lexical-binding: t; -*-


(when (modulep! :editor multiple-cursors)
  (map! "C->"   #'mc/mark-next-like-this
        "C-<"   #'mc/mark-previous-like-this
        "C-M->" #'mc/skip-to-next-like-this
        "C-M-<" #'mc/skip-to-previous-like-this
        "M-<mouse-1>" #'mc/add-cursor-on-click)
  (map! :leader
        :prefix "m"
        :desc "Pop mark"                        "SPC"   #'mc/mark-pop
        :desc "Mark all above"                  "<"     #'mc/mark-all-above
        :desc "Mark all below"                  ">"     #'mc/mark-all-below
        :desc "Mark words like this"            "W"     #'mc/mark-all-words-like-this
        :desc "Mark symbols like this"          "S"     #'mc/mark-all-symbols-like-this
        :desc "Mark words like this in defun"   "C-w"   #'mc/mark-all-words-like-this-in-defun
        :desc "Mark symbols like this in defun" "C-s"   #'mc/mark-all-symbols-like-this-in-defun
        :desc "Mark next sexps"                 "C-M-f" #'mc/mark-next-sexps
        :desc "Mark previous sexps"             "C-M-b" #'mc/mark-previous-sexps
        :desc "Mark regexp"                     "%"     #'mc/mark-all-in-region-regexp)
  (after! multiple-cursors-core
    (dolist (cmd '(doom/delete-backward-word
                   doom/forward-to-last-non-comment-or-eol
                   mark-sexp
                   eros-eval-last-sexp
                   eval-last-sexp
                   cae-eval-last-sexp))
      (add-to-list 'mc/cmds-to-run-for-all cmd))
    (dolist (cmd '(+workspace/new +workspace/load +workspace/save
                   +workspace/cycle +workspace/other +workspace/delete
                   +workspace/rename +workspace/display +workspace/new-named
                   +workspace/swap-left +workspace/switch-to
                   +workspace/swap-right +workspace/switch-left
                   +workspace/switch-to-0 +workspace/switch-to-1
                   +workspace/switch-to-2 +workspace/switch-to-3
                   +workspace/switch-to-4 +workspace/switch-to-5
                   +workspace/switch-to-6 +workspace/switch-to-7
                   +workspace/switch-to-8 +workspace/kill-session
                   +workspace/switch-right +workspace/switch-to-final
                   +workspace/restore-last-session +workspace/kill-session-and-quit
                   +workspace/close-window-or-workspace read-only-mode))
      (add-to-list 'mc/cmds-to-run-once cmd))
    (add-to-list 'mc/unsupported-minor-modes #'cae-completion-mode)
    (define-key mc/keymap (kbd "C-. .")     #'mc/move-to-column)
    (define-key mc/keymap (kbd "C-. =")     #'mc/compare-chars)
    (define-key mc/keymap (kbd "C-. C-.")   #'mc/freeze-fake-cursors-dwim)
    (define-key mc/keymap (kbd "C-. C-d")   #'mc/remove-current-cursor)
    (define-key mc/keymap (kbd "C-. C-k")   #'mc/remove-cursors-at-eol)
    (define-key mc/keymap (kbd "C-. C-o")   #'mc/remove-cursors-on-blank-lines)
    (define-key mc/keymap (kbd "C-. d")     #'mc/remove-duplicated-cursors)
    (define-key mc/keymap (kbd "C-. l")     #'mc/insert-letters)
    (define-key mc/keymap (kbd "C-. n")     #'mc/insert-numbers)
    (define-key mc/keymap (kbd "C-. s")     #'mc/sort-regions)
    (define-key mc/keymap (kbd "C-. r")     #'mc/reverse-regions)
    (define-key mc/keymap (kbd "C-. [")     #'mc/vertical-align-with-space)
    (define-key mc/keymap (kbd "C-. {")     #'mc/vertical-align)))
