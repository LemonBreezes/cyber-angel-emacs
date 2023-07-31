;;; completion/corfu/config.el -*- lexical-binding: t; -*-

(defvar +corfu-icon-height 0.9
  "The height applied to the icons (it is passed to both svg-lib and kind-icon).

It may need tweaking for the completions to not become cropped at the end.
Note that changes are applied only after a cache reset, via
`kind-icon-reset-cache'.")

;;
;;; Packages
(use-package! corfu
  :hook (doom-first-buffer . global-corfu-mode)
  :init
  ;; Auto-completion settings, must be set before calling `global-corfu-mode'.
  ;; Due to lazy-loading, setting them in config.el works too.
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 2
        corfu-excluded-modes '(erc-mode
                               circe-mode
                               help-mode
                               gud-mode
                               vterm-mode))
  :config
  (add-to-list 'completion-styles 'partial-completion t)
  (add-to-list 'completion-styles 'initials t)
  (setq corfu-cycle t
        corfu-separator (when (modulep! +orderless) ?\s)
        corfu-preselect t
        corfu-count 16
        corfu-max-width 120
        corfu-preview-current 'insert
        corfu-on-exact-match nil
        corfu-quit-at-boundary (if (modulep! +orderless) 'separator t)
        corfu-quit-no-match (if (modulep! +orderless) 'separator t)
        ;; In the case of +tng, TAB should be smart regarding completion;
        ;; However, it should otherwise behave like normal, whatever normal was.
        tab-always-indent (if (modulep! +tng) 'complete tab-always-indent))
  (when (modulep! +orderless)
    (cond ((modulep! :tools lsp +eglot) (add-to-list 'completion-category-overrides '(eglot (styles orderless))))
          ((modulep! :tools lsp) (add-hook 'lsp-completion-mode-hook
                                           (defun doom--use-orderless-lsp-capf ()
                                             (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
                                                   '(orderless)))))))
  (map! (:unless (modulep! +tng)
          :desc "complete" "C-SPC" #'completion-at-point)
        (:map 'corfu-map
              (:when (modulep! +orderless)
                :desc "insert separator" "C-SPC" #'corfu-insert-separator)
              (:when (modulep! +tng)
                :desc "next" [tab] #'corfu-next
                :desc "previous" [backtab] #'corfu-previous
                :desc "next" "TAB" #'corfu-next
                :desc "previous" "S-TAB" #'corfu-previous)))

  (when (modulep! :editor evil)
    (evil-collection-define-key 'insert 'corfu-map
      (kbd "RET") #'corfu-insert
      [return] #'corfu-insert))

  (after! vertico
    ;; Taken from corfu's README.
    ;; TODO: extend this to other completion front-ends.
    (defun corfu-move-to-minibuffer ()
      (interactive)
      (let ((completion-extra-properties corfu--extra)
            (completion-cycle-threshold completion-cycling))
        (apply #'consult-completion-in-region completion-in-region--data)))
    (map! :map 'corfu-map
          :desc "move to minibuffer" "s-<down>" #'corfu-move-to-minibuffer
          (:when (modulep! :editor evil)
            :desc "move to minibuffer" "s-j" #'corfu-move-to-minibuffer))))

(use-package! cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  :config
  (when (modulep! :editor snippets)
    (load! "+yas-capf.el")
    (add-hook 'yas-minor-mode-hook
              (lambda ()
                (add-to-list 'completion-at-point-functions #'yas-capf)))))

(use-package! kind-icon
  :commands kind-icon-margin-formatter
  :init
  (add-hook 'corfu-margin-formatters #'kind-icon-margin-formatter)
  :config
  (setq kind-icon-default-face 'corfu-default
        kind-icon-blend-background t
        kind-icon-blend-frac 0.2)
  (plist-put kind-icon-default-style :height +corfu-icon-height)
  (plist-put svg-lib-style-default :height +corfu-icon-height))

(use-package! corfu-terminal
  :when (not (display-graphic-p))
  :hook (corfu-mode . corfu-terminal-mode))

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
