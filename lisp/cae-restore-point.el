;;; private/holy/restore-point.el -*- lexical-binding: t; -*-

(use-package! restore-point
  :defer t :init
  (add-hook 'doom-first-input-hook #'restore-point-mode)
  :config
  (setq rp/restore-point-commands
        (nconc
         '(beginning-of-buffer
           end-of-buffer
           mark-defun
           mark-page
           mark-paragraph
           mark-sexp
           mark-whole-buffer
           mark-work
           mwheel-scroll
           scroll-bar-drag
           scroll-bar-scroll-down
           scroll-bar-scroll-up
           scroll-bar-toolkit-scroll
           scroll-down-command
           scroll-other-window
           scroll-other-window-down
           scroll-up scroll-down
           scroll-up-command
           rp/point-ring-nav-previous
           dired-maybe-insert-subdir
           dired-insert-subdir
           dired-kill-subdir
           dired-next-subdir
           dired-prev-subdir
           lispy-forward
           lispy-backward
           keyboard-quit
           describe-key describe-key-briefly)
         (and (featurep 'evil)
              '(evil-snipe-s evil-snipe-S evil-snipe-f evil-snipe-F evil-snipe-t evil-snipe-T
                evil-snipe-x evil-snipe-X evil-snipe-repeat
                evil-snipe-repeat-reverse evil-ex-search-next
                evil-ex-search-previous evil-goto-line evil-jump-item
                evil-goto-first-line keyboard-escape-quit evil-force-normal-state
                evil-exit-visual-state evil-normal-state evil-inner-xml-attr
                +evil:inner-url-txtobj +evil:inner-any-quote
                evil-indent-plus-i-indent-up evil-indent-plus-i-indent-up-down
                evil-indent-plus-i-indent evilnc-inner-comment evil-inner-arg
                evil-inner-symbol evil-inner-tag evil-inner-back-quote
                evil-inner-double-quote evil-inner-single-quote evil-inner-angle
                evil-inner-curly evil-textobj-anyblock-inner-block
                evil-inner-bracket evil-inner-paren evil-inner-paragraph
                evil-inner-sentence evil-inner-WORD evil-inner-word
                evil-outer-xml-attr +evil:outer-url-txtobj +evil:outer-any-quote
                evil-indent-plus-a-indent-up evil-indent-plus-a-indent-up-down
                evil-indent-plus-a-indent +evil:whole-buffer-txtobj
                +evil:defun-txtobj evilnc-outer-commenter evil-outer-arg
                evil-a-symbol evil-a-tag evil-a-back-quote evil-a-double-quote
                evil-a-single-quote evil-an-angle evil-a-curly
                evil-textobj-anyblock-a-block evil-a-bracket evil-a-paren
                evil-a-paragraph evil-a-sentence evil-a-WORD evil-a-word
                evil-scroll-up evil-scroll-down evil-scroll-left evil-scroll-right
                evil-scroll-line-up evil-scroll-line-down evil-scroll-page-down
                evil-scroll-page-up lispyville-backward-atom-begin
                lispyville-forward-atom-begin lispyville-backward-atom-end
                lispyville-forward-atom-end evil-cp-a-WORD evil-cp-a-form
                evil-cp-a-defun evil-cp-a-comment evil-forward-arg
                evil-backward-arg evil-forward-char evil-jump-forward
                evil-backward-char evil-jump-backward evil-search-forward
                evil-cp-drag-forward evil-cp-forward-sexp evil-search-backward
                evil-cp-backward-sexp evil-cp-drag-backward
                evil-ex-search-forward
                evil-forward-paragraph evil-backward-paragraph
                evil-ex-search-backward evil-find-char-backward
                evil-jump-backward-swap
                evil-cp-backward-up-sexp
                evil-forward-section-end evil-search-word-forward
                evil-backward-section-end evil-cp--backward-up-list
                evil-delete-backward-char evil-delete-backward-word
                evil-org-forward-sentence evil-search-word-backward
                evil-find-char-to-backward
                evil-forward-section-begin evil-org-backward-sentence
                evil-backward-section-begin
                evil-ex-search-word-forward evil-forward-sentence-begin
                evil-backward-sentence-begin evil-cp-delete-backward-word
                evil-ex-delete-backward-char
                evil-ex-search-word-backward
                evil-org-delete-backward-char evilem-motion-forward-WORD-end
                evilem-motion-forward-word-end evilem-motion-backward-WORD-end
                evilem-motion-backward-word-end evil-collection-pdf-jump-forward
                evilem-motion-find-char-backward evilem-motion-forward-WORD-begin
                evilem-motion-forward-word-begin evil-collection-pdf-jump-backward
                evilem-motion-backward-WORD-begin
                evilem-motion-backward-word-begin
                evilem-motion-forward-section-end
                evilem-motion-search-word-forward
                evil-command-window-search-forward
                evil-delete-backward-char-and-join
                evil-search-unbounded-word-forward
                evilem-motion-backward-section-end
                evilem-motion-search-word-backward
                evil-search-unbounded-word-backward
                evilem-motion-find-char-to-backward
                evilem-motion-forward-section-begin
                evil-visualstar/begin-search-forward
                evilem-motion-backward-section-begin
                evilem-motion-forward-sentence-begin
                evil-ex-search-unbounded-word-forward
                evil-visualstar/begin-search-backward
                evilem-motion-backward-sentence-begin
                evil-ex-search-unbounded-word-backward
                evil-cp-delete-char-or-splice-backwards
                evilem--motion-function-evil-forward-arg
                evilem--motion-function-evil-backward-arg
                evil-collection-vterm-delete-backward-char
                evil-textobj-anyblock-forward-any-block-end
                evil-textobj-anyblock-backward-any-block-end
                evil-textobj-anyblock-forward-open-block-end
                evil-textobj-anyblock-backward-open-block-end
                evil-textobj-anyblock-forward-any-block-start
                evil-textobj-anyblock-forward-close-block-end
                evil-textobj-anyblock-backward-any-block-start
                evil-textobj-anyblock-backward-close-block-end
                evil-textobj-anyblock-forward-open-block-start
                evil-textobj-anyblock-backward-open-block-start
                evil-textobj-anyblock-forward-close-block-start
                evil-textobj-anyblock-backward-close-block-start
                cae-cheatsheets-evil-cleverparens/evil-cp-drag-forward
                cae-cheatsheets-evil-cleverparens/evil-cp-drag-backward
                cae-evil-append-buffer-or-code evil-cp-append
                evil-normal-state evil-force-normal-state
                evil-insert-line evil-append-line
                evil-window-top evil-window-middle evil-window-bottom
                evil-cp-insert-at-end-of-form
                evil-cp-insert-at-beginning-of-form
                evil-end-of-line evil-beginning-of-line evil-first-non-blank
                evil-end-of-line-or-visual-line evil-beginning-of-visual-line
                evil-first-non-blank-of-visual-line
                evil-goto-mark-line evil-goto-mark evil-owl-goto-mark
                evil-owl-goto-mark-line +evil/reselect-paste
                ;; Moving by words/symbols
                ;;evil-forward-word-end evil-backward-WORD-end
                ;;evil-backward-word-end evil-backward-word-begin
                ;;evil-forward-WORD-begin evil-forward-word-begin
                ;;evil-forward-WORD-end evil-backward-WORD-begin
                ;;evil-cp-forward-symbol-end evil-cp-backward-symbol-end
                ;;evil-cp-forward-symbol-begin evil-cp-backward-symbol-begin
                ))))
  (advice-add #'rp/restore-point-position :after #'deactivate-mark)
  ;; Restore point in the minibuffer.
  (defun cae-restore-point-h ()
    (when restore-point-mode
      (rp/cond-restore-point)))
  (defun cae-restore-point-enable-in-minibuffer-h ()
    (if restore-point-mode
        (progn (advice-add #'minibuffer-keyboard-quit :before #'rp/cond-restore-point)
               (advice-remove #'keyboard-quit #'rp/cond-restore-point)
               ;; Use `doom-escape-hook' instead of a `keyboard-quit' advice because that
               ;; way we are certain this function is only called interactively.
               (add-hook 'doom-escape-hook #'cae-restore-point-h -1)

               ;; When we're using Evil, we also want to restore point when we
               ;; exit visual state.
               (add-hook 'evil-visual-state-exit-hook #'rp/cond-restore-point)
               (add-hook 'evil-insert-state-exit-hook #'rp/cond-restore-point))
      (advice-remove #'minibuffer-keyboard-quit #'rp/cond-restore-point)
      (advice-remove #'evil-exit-visual-state #'rp/cond-restore-point)
      (remove-hook 'doom-escape-hook #'cae-restore-point-h)))
  (add-hook 'restore-point-mode-hook #'cae-restore-point-enable-in-minibuffer-h))
