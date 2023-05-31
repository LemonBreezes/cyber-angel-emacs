;;; completion/corfu/config.el -*- lexical-binding: t; -*-

(defvar +corfu-auto-delay 0.1
  "How long after point stands still will completion be called automatically,
in seconds.

Setting `corfu-auto-delay' directly may not work, as it needs to be set *before*
enabling `corfu-mode'.")
(defvar +corfu-auto-prefix 2
  "How many characters should be typed before auto-complete starts to kick in.

Setting `corfu-auto-prefix' directly may not work, as it needs to be set
*before* enabling `corfu-mode'.")
(defvar +corfu-want-multi-component t
  "Enables multiple component search, with pieces separated by spaces.

This allows search of non-contiguous unordered bits, for instance by typing
\"tear rip\" to match \"rip-and-tear\". Notice the space, it does not break
completion in this case.")

(defvar +corfu-ispell-completion-modes '(org-mode markdown-mode text-mode)
  "Modes to enable ispell completion in.

For completion in comments, see `+corfu-ispell-in-comments-and-strings'.")
(defvar +corfu-ispell-in-comments-and-strings t
  "Enable completion with ispell inside comments when in a `prog-mode'
derivative.")

;;; Packages
(use-package! corfu
  :init
  (add-hook 'doom-first-input-hook #'global-corfu-mode)
  :init
  ;; Auto-completion settings, must be set before calling `global-corfu-mode'.
  (setq corfu-auto t
        corfu-auto-delay +corfu-auto-delay
        corfu-auto-prefix +corfu-auto-prefix
        corfu-excluded-modes '(erc-mode
                               circe-mode
                               help-mode
                               gud-mode
                               vterm-mode))

  :config
  (when (and (modulep! :tools lsp) (not (modulep! :tools lsp +eglot)))
    (add-hook 'lsp-mode-hook (defun doom--add-lsp-capf ()
                               (add-to-list 'completion-at-point-functions (cape-capf-buster #'lsp-completion-at-point)))
              ;; Below is so that context specific completions in cape come first.
              :depth 1))
  (add-to-list 'completion-styles 'partial-completion t)
  (add-to-list 'completion-styles 'initials t)
  (setq corfu-cycle t
        corfu-separator (when +corfu-want-multi-component ?\s)
        corfu-preselect t
        corfu-count 16
        corfu-max-width 120
        corfu-preview-current 'insert
        corfu-quit-at-boundary (if +corfu-want-multi-component 'separator t)
        corfu-quit-no-match (if +corfu-want-multi-component 'separator t)
        ;; In the case of +tng, TAB should be smart regarding completion;
        ;; However, it should otherwise behave like normal, whatever normal was.
        tab-always-indent (if (modulep! +tng) 'complete tab-always-indent))
  ;; Only done with :tools vertico active due to orderless. Alternatively, we
  ;; could set it up here if it's not there.
  (when (and +corfu-want-multi-component (modulep! :completion vertico))
    (cond ((modulep! :tools lsp +eglot) (add-to-list 'completion-category-overrides '(eglot (styles orderless))))
          ((modulep! :tools lsp) (add-hook 'lsp-completion-mode-hook
                                           (defun doom--use-orderless-lsp-capf ()
                                             (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
                                                   '(orderless)))))))

  (map! (:unless (modulep! +tng)
         :desc "complete" "C-SPC" #'completion-at-point)
        (:map 'corfu-map
         (:when +corfu-want-multi-component
          :desc "insert separator" "C-SPC" #'corfu-insert-separator)
         (:when (modulep! :completion vertico)
          :desc "move to minibuffer" "s-<down>" #'corfu-move-to-minibuffer
          (:when (modulep! :editor evil)
           :desc "move to minibuffer" "s-j" #'corfu-move-to-minibuffer))
         (:when (modulep! +tng)
          :desc "next" [tab] #'corfu-next
          :desc "previous" [backtab] #'corfu-previous
          :desc "next" "TAB" #'corfu-next
          :desc "previous" "S-TAB" #'corfu-previous)))

  ;; Taken from corfu's README.
  ;; TODO: extend this to other completion front-ends, mainly helm and ido, since
  ;; ivy is being considered for removal.
  (when (modulep! :completion vertico)
    (defun corfu-move-to-minibuffer ()
      (interactive)
      (let ((completion-extra-properties corfu--extra)
            completion-cycle-threshold completion-cycling)
        (apply #'consult-completion-in-region completion-in-region--data)))))

(use-package! cape
  :after corfu
  :commands (cape-dabbrev
             cape-file
             cape-history
             cape-keyword
             cape-tex
             cape-sgml
             cape-rfc1345
             cape-abbrev
             cape-ispell
             cape-dict
             cape-symbol
             cape-line)
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (when +corfu-ispell-in-comments-and-strings
    (defalias 'corfu--ispell-in-comments-and-strings
      (cape-super-capf (cape-capf-inside-comment #'cape-ispell)
                       (cape-capf-inside-string #'cape-ispell)))
    (add-hook 'prog-mode-hook
              (lambda ()
                (add-to-list 'completion-at-point-functions #'corfu--ispell-in-comments-and-strings))))
  (dolist (sym +corfu-ispell-completion-modes)
    (add-hook (intern (concat (symbol-name sym) "-hook"))
              (lambda ()
                (add-to-list 'completion-at-point-functions #'cape-ispell))))
  (add-hook! '(TeX-mode-hook LaTeX-mode-hook org-mode-hook)
    (lambda ()
      (add-to-list 'completion-at-point-functions #'cape-tex t))
    :depth 2)
  (add-hook! '(html-mode-hook +web-react-mode-hook typescript-tsx-mode-hook org-mode-hook markdown-mode-hook)
    (lambda ()
      (add-to-list 'completion-at-point-functions #'cape-sgml t))
    :depth 2)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package! kind-icon
  :when (modulep! +icons)
  :commands kind-icon-margin-formatter
  :init
  (add-hook 'corfu-margin-formatters #'kind-icon-margin-formatter)
  :config
  (setq kind-icon-default-face 'corfu-default
        kind-icon-blend-background t
        kind-icon-blend-frac 0.2))

(use-package! corfu-terminal
  :when (and (modulep! :os tty) (not (cae-display-graphic-p)))
  :hook (corfu-mode . corfu-terminal-mode))

(use-package! dabbrev
  :config
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;;
;;; Extensions
(use-package! corfu-history
  :after savehist
  :hook (corfu-mode . corfu-history-mode)
  :config
  (add-to-list 'savehist-additional-variables 'corfu-history))
(use-package! corfu-popupinfo
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay '(0.5 . 1.0))
  (map! (:map 'corfu-map
         :desc "scroll info up" "C-<up>" #'corfu-popupinfo-scroll-down
         :desc "scroll info down" "C-<down>" #'corfu-popupinfo-scroll-up
         :desc "scroll info up" "C-S-p" #'corfu-popupinfo-scroll-down
         :desc "scroll info down" "C-S-n" #'corfu-popupinfo-scroll-up
         :desc "toggle info" "C-h" #'corfu-popupinfo-toggle)
        (:map 'corfu-popupinfo-map
         :when (modulep! :editor evil)
         ;; Reversed because popupinfo assumes opposite of what feels intuitive
         ;; with evil.
         :desc "scroll info up" "C-S-k" #'corfu-popupinfo-scroll-down
         :desc "scroll info down" "C-S-j" #'corfu-popupinfo-scroll-up)))
