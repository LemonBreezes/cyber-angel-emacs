;;; lisp/cae-multiple-cursors.el -*- lexical-binding: t; -*-


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
                 doom/forward-to-last-non-comment-or-eol mark-sexp
                 eros-eval-last-sexp eval-last-sexp cae-eval-last-sexp
                 forward-sentence backward-sentence kill-sentence
                 sentex-forward-sentence sentex-backward-sentence
                 sentex-kill-sentence parrot-rotate-next-word-at-point
                 cae-delete-char cae-modeline-rotate-forward-word-at-point
                 cae-modeline-rotate-backward-word-at-point
                 forward-sexp backward-sexp backward-list
                 forward-list down-list backward-up-list up-list))
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
                 +workspace/close-woutdow-or-workspace read-only-mode
                 save-buffers-kill-terminal))
    (add-to-list 'mc/cmds-to-run-once cmd))
  (dolist (mode '(cae-completion-mode symbol-overlay-mode goggles-mode
                  lispy-mode corfu-mode hungry-delete-mode
                  worf-mode isearch-mb-mode))
    (add-to-list 'mc/unsupported-minor-modes mode))
  (define-prefix-command 'cae-mc-map)
  (define-key cae-mc-map (kbd ".")   #'mc/move-to-column)
  (define-key cae-mc-map (kbd "=")   #'mc/compare-chars)
  (define-key cae-mc-map (kbd "C-.") #'mc/freeze-fake-cursors-dwim)
  (define-key cae-mc-map (kbd "C-d")   #'mc/remove-current-cursor)
  (define-key cae-mc-map (kbd "C-k")   #'mc/remove-cursors-at-eol)
  (define-key cae-mc-map (kbd "C-o")   #'mc/remove-cursors-on-blank-lines)
  (define-key cae-mc-map (kbd "d")     #'mc/remove-duplicated-cursors)
  (define-key cae-mc-map (kbd "l")     #'mc/insert-letters)
  (define-key cae-mc-map (kbd "n")     #'mc/insert-numbers)
  (define-key cae-mc-map (kbd "s")     #'mc/sort-regions)
  (define-key cae-mc-map (kbd "r")     #'mc/reverse-regions)
  (define-key cae-mc-map (kbd "[")     #'mc/vertical-align-with-space)
  (define-key cae-mc-map (kbd "{")     #'mc/vertical-align)
  (define-key mc/keymap (kbd "C-.") #'cae-mc-map)
  (define-key mc/keymap (kbd "<f5>") #'cae-mc-map))

;;Local Variables:
;;eval: (unless (and (modulep! :editor multiple-cursors) (not (modulep! :editor evil))) (remove-hook 'write-file-functions #'eval-buffer t))
;;End:
