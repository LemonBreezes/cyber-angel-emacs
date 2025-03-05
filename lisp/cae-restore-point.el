;;; cae/holy/restore-point.el -*- lexical-binding: t; -*-

(defconst cae-restore-point-default-commands
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
    scroll-up
    scroll-down
    scroll-up-command
    pop-mark
    cae-pop-mark
    cae-jump-to-random-line
    c-mark-function
    c-mark
    query-replace
    query-replace-regexp
    anzu-query-replace
    anzu-query-replace-regexp
    rp/point-ring-nav-previous
    dired-maybe-insert-subdir
    dired-insert-subdir
    dired-kill-subdir
    dired-next-subdir
    dired-prev-subdir
    lispy-forward
    lispy-backward
    keyboard-quit
    describe-key
    describe-key-briefly
    sp-backward-down-sexp
    sp-down-sexp
    sp-up-sexp
    sp-forward-sexp
    sp-next-sexp
    sp-previous-sexp
    sp-backward-up-sexp
    eri/expand-region
    er/expand-region
    er/c-mark-statement
    er/c-mark-function-call-1
    er/c-mark-function-call-2
    er/c-mark-vector-access-1
    er/c-mark-vector-access-2
    er/c-mark-statement-block-1
    er/c-mark-statement-block-2
    er/c-mark-fully-qualified-name
    er/mark-url
    er/mark-word
    er/mark-defun
    er/mark-email
    eri/mark-line
    er/mark-symbol
    eri/mark-block
    er/mark-comment
    er/expand-region
    er/mark-sentence
    er/mark-paragraph
    eri/expand-region
    er/contract-region
    er/mark-org-parent
    er/mark-method-call
    er/mark-org-element
    eri/contract-region
    eri/maximize-region
    er/mark-inside-pairs
    er/mark-inside-quotes
    er/mark-next-accessor
    er/mark-outside-pairs
    er/mark-text-sentence
    er/mark-org-code-block
    er/mark-outside-quotes
    er/mark-text-paragraph
    eri/mark-outside-quotes
    er/mark-org-element-parent
    er/mark-symbol-with-prefix
    eri/web-mode-element-parent
    eri/mark-inside-org-table-cell
    eri/mark-outside-org-table-cell
    eri/web-mode-element-parent-content
    cae-jump-to-random-line-end
    better-jumper-jump-backward
    better-jumper-jump-forward
    beginend-bs-mode-goto-end
    beginend-rg-mode-goto-end
    beginend-org-mode-goto-end
    beginend-deft-mode-goto-end
    beginend-prog-mode-goto-end
    beginend-LaTeX-mode-goto-end
    beginend-dired-mode-goto-end
    beginend-latex-mode-goto-end
    beginend-nroam-mode-goto-end
    beginend-occur-mode-goto-end
    beginend-vc-dir-mode-goto-end
    beginend-ibuffer-mode-goto-end
    beginend-message-mode-goto-end
    beginend-outline-mode-goto-end
    beginend-prodigy-mode-goto-end
    beginend-bs-mode-goto-beginning
    beginend-rg-mode-goto-beginning
    beginend-org-mode-goto-beginning
    beginend-deft-mode-goto-beginning
    beginend-org-agenda-mode-goto-end
    beginend-prog-mode-goto-beginning
    beginend-LaTeX-mode-goto-beginning
    beginend-compilation-mode-goto-end
    beginend-dired-mode-goto-beginning
    beginend-elfeed-show-mode-goto-end
    beginend-latex-mode-goto-beginning
    beginend-nroam-mode-goto-beginning
    beginend-occur-mode-goto-beginning
    beginend-epa-key-list-mode-goto-end
    beginend-magit-status-mode-goto-end
    beginend-vc-dir-mode-goto-beginning
    beginend-elfeed-search-mode-goto-end
    beginend-ibuffer-mode-goto-beginning
    beginend-message-mode-goto-beginning
    beginend-outline-mode-goto-beginning
    beginend-prodigy-mode-goto-beginning
    beginend-magit-revision-mode-goto-end
    beginend-notmuch-search-mode-goto-end
    beginend-recentf-dialog-mode-goto-end
    beginend-org-agenda-mode-goto-beginning
    beginend-compilation-mode-goto-beginning
    beginend-elfeed-show-mode-goto-beginning
    beginend-epa-key-list-mode-goto-beginning
    beginend-magit-status-mode-goto-beginning
    beginend-elfeed-search-mode-goto-beginning
    beginend-magit-revision-mode-goto-beginning
    beginend-notmuch-search-mode-goto-beginning
    beginend-recentf-dialog-mode-goto-beginning)
  "List of default commands for restore-point mode.")
(defconst cae-restore-point-evil-commands
  '(evil-snipe-s evil-snipe-S evil-snipe-f evil-snipe-F
    evil-snipe-t evil-snipe-T
    evil-snipe-x evil-snipe-X evil-snipe-repeat
    evil-snipe-repeat-reverse evil-ex-search-next
    evil-backward-WORD-begin evil-cp-forward-symbol-end
    evil-forward-WORD-end
    evil-undo evil-redo
    evil-search-previous evil-search-next
    evil-ex-search-previous evil-goto-line evil-jump-item
    evil-goto-first-line keyboard-escape-quit
    evil-force-normal-state
    evil-exit-visual-state evil-normal-state evil-inner-xml-attr
    +evil:inner-url-txtobj +evil:inner-any-quote
    evil-indent-plus-i-indent-up
    evil-indent-plus-i-indent-up-down
    evil-indent-plus-i-indent evilnc-inner-comment evil-inner-arg
    evil-inner-symbol evil-inner-tag evil-inner-back-quote
    evil-inner-double-quote evil-inner-single-quote evil-inner-angle
    evil-inner-curly evil-textobj-anyblock-inner-block
    evil-inner-bracket evil-inner-paren evil-inner-paragraph
    evil-inner-sentence evil-inner-WORD evil-inner-word
    evil-outer-xml-attr +evil:outer-url-txtobj
    +evil:outer-any-quote
    evil-indent-plus-a-indent-up
    evil-indent-plus-a-indent-up-down
    evil-indent-plus-a-indent +evil:whole-buffer-txtobj
    +evil:defun-txtobj evilnc-outer-commenter evil-outer-arg
    evil-a-symbol evil-a-tag evil-a-back-quote
    evil-a-double-quote
    evil-a-single-quote evil-an-angle evil-a-curly
    evil-textobj-anyblock-a-block evil-a-bracket evil-a-paren
    evil-a-paragraph evil-a-sentence evil-a-WORD evil-a-word
    evil-scroll-up evil-scroll-down evil-scroll-left
    evil-scroll-right
    evil-scroll-line-up evil-scroll-line-down
    evil-scroll-page-down
    evil-scroll-page-up lispyville-backward-atom-begin
    lispyville-forward-atom-begin lispyville-backward-atom-end
    lispyville-forward-atom-end evil-cp-a-WORD evil-cp-a-form
    evil-cp-a-defun evil-cp-a-comment evil-forward-arg
    evil-cp-beginning-of-defun
    evil-backward-arg evil-forward-char evil-jump-forward
    evil-backward-char evil-jump-backward evil-search-forward
    evil-cp-drag-forward evil-cp-forward-sexp
    evil-search-backward
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
    evilem-motion-forward-word-end
    evilem-motion-backward-WORD-end
    evilem-motion-backward-word-end
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
    evil-beginend-bs-mode-goto-end
    evil-beginend-rg-mode-goto-end
    evil-beginend-org-mode-goto-end
    evil-beginend-deft-mode-goto-end
    evil-beginend-prog-mode-goto-end
    evil-beginend-LaTeX-mode-goto-end
    evil-beginend-dired-mode-goto-end
    evil-beginend-latex-mode-goto-end
    evil-beginend-nroam-mode-goto-end
    evil-beginend-occur-mode-goto-end
    evil-beginend-vc-dir-mode-goto-end
    evil-beginend-ibuffer-mode-goto-end
    evil-beginend-message-mode-goto-end
    evil-beginend-outline-mode-goto-end
    evil-beginend-prodigy-mode-goto-end
    evil-beginend-bs-mode-goto-beginning
    evil-beginend-rg-mode-goto-beginning
    evil-beginend-org-mode-goto-beginning
    evil-beginend-deft-mode-goto-beginning
    evil-beginend-org-agenda-mode-goto-end
    evil-beginend-prog-mode-goto-beginning
    evil-beginend-LaTeX-mode-goto-beginning
    evil-beginend-compilation-mode-goto-end
    evil-beginend-dired-mode-goto-beginning
    evil-beginend-elfeed-show-mode-goto-end
    evil-beginend-latex-mode-goto-beginning
    evil-beginend-nroam-mode-goto-beginning
    evil-beginend-occur-mode-goto-beginning
    evil-beginend-epa-key-list-mode-goto-end
    evil-beginend-magit-status-mode-goto-end
    evil-beginend-vc-dir-mode-goto-beginning
    evil-beginend-elfeed-search-mode-goto-end
    evil-beginend-ibuffer-mode-goto-beginning
    evil-beginend-message-mode-goto-beginning
    evil-beginend-outline-mode-goto-beginning
    evil-beginend-prodigy-mode-goto-beginning
    evil-beginend-magit-revision-mode-goto-end
    evil-beginend-notmuch-search-mode-goto-end
    evil-beginend-recentf-dialog-mode-goto-end
    evil-beginend-org-agenda-mode-goto-beginning
    evil-beginend-compilation-mode-goto-beginning
    evil-beginend-elfeed-show-mode-goto-beginning
    evil-beginend-epa-key-list-mode-goto-beginning
    evil-beginend-magit-status-mode-goto-beginning
    evil-beginend-elfeed-search-mode-goto-beginning
    evil-beginend-magit-revision-mode-goto-beginning
    evil-beginend-notmuch-search-mode-goto-beginning
    evil-beginend-recentf-dialog-mode-goto-beginning
    cae-evil-append-buffer-or-code evil-cp-append
    evil-normal-state evil-force-normal-state
    evil-insert-line evil-append-line
    evil-window-top evil-window-middle evil-window-bottom
    evil-cp-insert-at-end-of-form
    evil-cp-insert-at-beginning-of-form
    evil-cp-insert evil-insert evil-append
    evil-end-of-line evil-beginning-of-line evil-first-non-blank
    evil-end-of-line-or-visual-line evil-beginning-of-visual-line
    evil-first-non-blank-of-visual-line
    evil-goto-mark-line evil-goto-mark evil-owl-goto-mark
    evil-owl-goto-mark-line +evil/reselect-paste))
(use-package! restore-point
  :defer t :init
  (add-hook 'doom-first-input-hook #'restore-point-mode)
  (cae-advice-add #'rp/restore-point-position :after #'deactivate-mark)
  :config
  (setq rp/restore-point-commands
        (append '(beginning-of-buffer
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
                  pop-mark
                  cae-pop-mark
                  cae-jump-to-random-line
                  c-mark-function
                  c-mark
                  query-replace
                  query-replace-regexp
                  anzu-query-replace
                  anzu-query-replace-regexp
                  rp/point-ring-nav-previous
                  dired-maybe-insert-subdir
                  dired-insert-subdir
                  dired-kill-subdir
                  dired-next-subdir
                  dired-prev-subdir
                  lispy-forward
                  lispy-backward
                  keyboard-quit
                  describe-key describe-key-briefly
                  sp-backward-down-sexp sp-down-sexp
                  sp-up-sexp sp-forward-sexp sp-next-sexp sp-previous-sexp
                  sp-backward-up-sexp eri/expand-region er/expand-region
                  er/c-mark-statement
                  er/c-mark-function-call-1
                  er/c-mark-function-call-2
                  er/c-mark-vector-access-1
                  er/c-mark-vector-access-2
                  er/c-mark-statement-block-1
                  er/c-mark-statement-block-2
                  er/c-mark-fully-qualified-name
                  er/mark-url
                  er/mark-word
                  er/mark-defun
                  er/mark-email
                  eri/mark-line
                  er/mark-symbol
                  eri/mark-block
                  er/mark-comment
                  er/expand-region
                  er/mark-sentence
                  er/mark-paragraph
                  eri/expand-region
                  er/contract-region
                  er/mark-org-parent
                  er/mark-method-call
                  er/mark-org-element
                  eri/contract-region
                  eri/maximize-region
                  er/mark-inside-pairs
                  er/mark-inside-quotes
                  er/mark-next-accessor
                  er/mark-outside-pairs
                  er/mark-text-sentence
                  er/mark-org-code-block
                  er/mark-outside-quotes
                  er/mark-text-paragraph
                  eri/mark-outside-quotes
                  er/mark-org-element-parent
                  er/mark-symbol-with-prefix
                  eri/web-mode-element-parent
                  eri/mark-inside-org-table-cell
                  eri/mark-outside-org-table-cell
                  eri/web-mode-element-parent-content
                  cae-jump-to-random-line-end
                  better-jumper-jump-backward
                  better-jumper-jump-forward
                  beginend-bs-mode-goto-end
                  beginend-rg-mode-goto-end
                  beginend-org-mode-goto-end
                  beginend-deft-mode-goto-end
                  beginend-prog-mode-goto-end
                  beginend-LaTeX-mode-goto-end
                  beginend-dired-mode-goto-end
                  beginend-latex-mode-goto-end
                  beginend-nroam-mode-goto-end
                  beginend-occur-mode-goto-end
                  beginend-vc-dir-mode-goto-end
                  beginend-ibuffer-mode-goto-end
                  beginend-message-mode-goto-end
                  beginend-outline-mode-goto-end
                  beginend-prodigy-mode-goto-end
                  beginend-bs-mode-goto-beginning
                  beginend-rg-mode-goto-beginning
                  beginend-org-mode-goto-beginning
                  beginend-deft-mode-goto-beginning
                  beginend-org-agenda-mode-goto-end
                  beginend-prog-mode-goto-beginning
                  beginend-LaTeX-mode-goto-beginning
                  beginend-compilation-mode-goto-end
                  beginend-dired-mode-goto-beginning
                  beginend-elfeed-show-mode-goto-end
                  beginend-latex-mode-goto-beginning
                  beginend-nroam-mode-goto-beginning
                  beginend-occur-mode-goto-beginning
                  beginend-epa-key-list-mode-goto-end
                  beginend-magit-status-mode-goto-end
                  beginend-vc-dir-mode-goto-beginning
                  beginend-elfeed-search-mode-goto-end
                  beginend-ibuffer-mode-goto-beginning
                  beginend-message-mode-goto-beginning
                  beginend-outline-mode-goto-beginning
                  beginend-prodigy-mode-goto-beginning
                  beginend-magit-revision-mode-goto-end
                  beginend-notmuch-search-mode-goto-end
                  beginend-recentf-dialog-mode-goto-end
                  beginend-org-agenda-mode-goto-beginning
                  beginend-compilation-mode-goto-beginning
                  beginend-elfeed-show-mode-goto-beginning
                  beginend-epa-key-list-mode-goto-beginning
                  beginend-magit-status-mode-goto-beginning
                  beginend-elfeed-search-mode-goto-beginning
                  beginend-magit-revision-mode-goto-beginning
                  beginend-notmuch-search-mode-goto-beginning
                  beginend-recentf-dialog-mode-goto-beginning)
                (and
                 (featurep 'evil)
                 '(evil-snipe-s evil-snipe-S evil-snipe-f evil-snipe-F
                   evil-snipe-t evil-snipe-T
                   evil-snipe-x evil-snipe-X evil-snipe-repeat
                   evil-snipe-repeat-reverse evil-ex-search-next
                   evil-backward-WORD-begin evil-cp-forward-symbol-end
                   evil-forward-WORD-end
                   evil-undo evil-redo
                   evil-search-previous evil-search-next
                   evil-ex-search-previous evil-goto-line evil-jump-item
                   evil-goto-first-line keyboard-escape-quit
                   evil-force-normal-state
                   evil-exit-visual-state evil-normal-state evil-inner-xml-attr
                   +evil:inner-url-txtobj +evil:inner-any-quote
                   evil-indent-plus-i-indent-up
                   evil-indent-plus-i-indent-up-down
                   evil-indent-plus-i-indent evilnc-inner-comment evil-inner-arg
                   evil-inner-symbol evil-inner-tag evil-inner-back-quote
                   evil-inner-double-quote evil-inner-single-quote
                   evil-inner-angle
                   evil-inner-curly evil-textobj-anyblock-inner-block
                   evil-inner-bracket evil-inner-paren evil-inner-paragraph
                   evil-inner-sentence evil-inner-WORD evil-inner-word
                   evil-outer-xml-attr +evil:outer-url-txtobj
                   +evil:outer-any-quote
                   evil-indent-plus-a-indent-up
                   evil-indent-plus-a-indent-up-down
                   evil-indent-plus-a-indent +evil:whole-buffer-txtobj
                   +evil:defun-txtobj evilnc-outer-commenter evil-outer-arg
                   evil-a-symbol evil-a-tag evil-a-back-quote
                   evil-a-double-quote
                   evil-a-single-quote evil-an-angle evil-a-curly
                   evil-textobj-anyblock-a-block evil-a-bracket evil-a-paren
                   evil-a-paragraph evil-a-sentence evil-a-WORD evil-a-word
                   evil-scroll-up evil-scroll-down evil-scroll-left
                   evil-scroll-right
                   evil-scroll-line-up evil-scroll-line-down
                   evil-scroll-page-down
                   evil-scroll-page-up lispyville-backward-atom-begin
                   lispyville-forward-atom-begin lispyville-backward-atom-end
                   lispyville-forward-atom-end evil-cp-a-WORD evil-cp-a-form
                   evil-cp-a-defun evil-cp-a-comment evil-forward-arg
                   evil-cp-beginning-of-defun
                   evil-backward-arg evil-forward-char evil-jump-forward
                   evil-backward-char evil-jump-backward evil-search-forward
                   evil-cp-drag-forward evil-cp-forward-sexp
                   evil-search-backward
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
                   evilem-motion-forward-word-end
                   evilem-motion-backward-WORD-end
                   evilem-motion-backward-word-end
                   evil-collection-pdf-jump-forward
                   evilem-motion-find-char-backward
                   evilem-motion-next-line evilem-motion-previous-line
                   evilem-motion-forward-WORD-begin
                   evilem-motion-forward-word-begin
                   evil-collection-pdf-jump-backward
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
                   evil-beginend-bs-mode-goto-end
                   evil-beginend-rg-mode-goto-end
                   evil-beginend-org-mode-goto-end
                   evil-beginend-deft-mode-goto-end
                   evil-beginend-prog-mode-goto-end
                   evil-beginend-LaTeX-mode-goto-end
                   evil-beginend-dired-mode-goto-end
                   evil-beginend-latex-mode-goto-end
                   evil-beginend-nroam-mode-goto-end
                   evil-beginend-occur-mode-goto-end
                   evil-beginend-vc-dir-mode-goto-end
                   evil-beginend-ibuffer-mode-goto-end
                   evil-beginend-message-mode-goto-end
                   evil-beginend-outline-mode-goto-end
                   evil-beginend-prodigy-mode-goto-end
                   evil-beginend-bs-mode-goto-beginning
                   evil-beginend-rg-mode-goto-beginning
                   evil-beginend-org-mode-goto-beginning
                   evil-beginend-deft-mode-goto-beginning
                   evil-beginend-org-agenda-mode-goto-end
                   evil-beginend-prog-mode-goto-beginning
                   evil-beginend-LaTeX-mode-goto-beginning
                   evil-beginend-compilation-mode-goto-end
                   evil-beginend-dired-mode-goto-beginning
                   evil-beginend-elfeed-show-mode-goto-end
                   evil-beginend-latex-mode-goto-beginning
                   evil-beginend-nroam-mode-goto-beginning
                   evil-beginend-occur-mode-goto-beginning
                   evil-beginend-epa-key-list-mode-goto-end
                   evil-beginend-magit-status-mode-goto-end
                   evil-beginend-vc-dir-mode-goto-beginning
                   evil-beginend-elfeed-search-mode-goto-end
                   evil-beginend-ibuffer-mode-goto-beginning
                   evil-beginend-message-mode-goto-beginning
                   evil-beginend-outline-mode-goto-beginning
                   evil-beginend-prodigy-mode-goto-beginning
                   evil-beginend-magit-revision-mode-goto-end
                   evil-beginend-notmuch-search-mode-goto-end
                   evil-beginend-recentf-dialog-mode-goto-end
                   evil-beginend-org-agenda-mode-goto-beginning
                   evil-beginend-compilation-mode-goto-beginning
                   evil-beginend-elfeed-show-mode-goto-beginning
                   evil-beginend-epa-key-list-mode-goto-beginning
                   evil-beginend-magit-status-mode-goto-beginning
                   evil-beginend-elfeed-search-mode-goto-beginning
                   evil-beginend-magit-revision-mode-goto-beginning
                   evil-beginend-notmuch-search-mode-goto-beginning
                   evil-beginend-recentf-dialog-mode-goto-beginning
                   cae-evil-append-buffer-or-code evil-cp-append
                   evil-normal-state evil-force-normal-state
                   evil-insert-line evil-append-line
                   evil-window-top evil-window-middle evil-window-bottom
                   evil-cp-insert-at-end-of-form
                   evil-cp-insert-at-beginning-of-form
                   evil-cp-insert evil-insert evil-append
                   evil-end-of-line evil-beginning-of-line evil-first-non-blank
                   evil-end-of-line-or-visual-line evil-beginning-of-visual-line
                   evil-first-non-blank-of-visual-line
                   evil-goto-mark-line evil-goto-mark evil-owl-goto-mark
                   evil-owl-goto-mark-line +evil/reselect-paste))))

  ;; Restore point in the minibuffer.
  (defun cae-restore-point-h ()
    "If restore-point-mode is active, restore point in the minibuffer and return non-nil if the point changed."
    (when-let ((start (point))
               (restored (and restore-point-mode
                              (rp/cond-restore-point) (point))))
      (not (eq start restored))))
  (defun cae--enable-minibuffer-restore-point ()
    "Enable advice and hooks to restore point in the minibuffer."
    (dolist (pair (list (list #'minibuffer-keyboard-quit :before #'rp/cond-restore-point)))
      (apply #'advice-add pair))
    (advice-remove #'keyboard-quit #'rp/cond-restore-point)
    (add-hook 'doom-escape-hook #'cae-restore-point-h -2)
    (add-hook 'evil-visual-state-exit-hook #'rp/cond-restore-point)
    (add-hook 'evil-insert-state-exit-hook #'rp/cond-restore-point))

  (defun cae--disable-minibuffer-restore-point ()
    "Remove advice and hooks for minibuffer restore-point."
    (advice-remove #'minibuffer-keyboard-quit #'rp/cond-restore-point)
    (advice-remove #'evil-exit-visual-state #'rp/cond-restore-point)
    (remove-hook 'doom-escape-hook #'cae-restore-point-h))

  (defun cae-restore-point-enable-in-minibuffer-h ()
    (if restore-point-mode
        (cae--enable-minibuffer-restore-point)
      (cae--disable-minibuffer-restore-point)))
  (add-hook 'restore-point-mode-hook #'cae-restore-point-enable-in-minibuffer-h))
