;;; completion/corfu/config.el -*- lexical-binding: t; -*-

;;
;;; Packages
(use-package! corfu
  :hook (doom-first-buffer . global-corfu-mode)
  :hook (org-mode . corfu-mode)
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
          "C-SPC" #'completion-at-point)
        (:map 'corfu-map
              (:when (modulep! +orderless)
                "C-SPC" #'corfu-insert-separator)
              (:when (modulep! +tng)
                [tab] #'corfu-next
                [backtab] #'corfu-previous
                "TAB" #'corfu-next
                "S-TAB" #'corfu-previous)))

  (after! evil-collection-corfu
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
    (map! :map 'corfu-map "s-<down>" #'corfu-move-to-minibuffer
          (:when (modulep! :editor evil) "s-j" #'corfu-move-to-minibuffer))))

(defmacro +add-capf! (capf)
  "Create sexp to add CAPF to the list of CAPFs."
  `(add-to-list 'completion-at-point-functions ,capf))
(use-package! cape
  :after corfu
  :config
  (add-hook! prog-mode (+add-capf! #'cape-file))
  (add-hook! (org-mode markdown-mode) (+add-capf! #'cape-elisp-block))
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible))

;;(use-package! yasnippet-capf
;;  :after corfu
;;  :config
;;  (add-hook 'yas-minor-mode-hook
;;            (lambda () (add-to-list 'completion-at-point-functions #'yasnippet-capf))))

(use-package! svg-lib
  :after kind-icon)
(use-package! kind-icon
  :commands (kind-icon-margin-formatter
             kind-icon-reset-cache
             kind-icon-formatted)
  :init
  (add-hook 'corfu-margin-formatters #'kind-icon-margin-formatter)
  :config
  (defface corfu-kind-icon '((t :inherit corfu-default))
    "Face for the icons in the corfu popup.
For changing color, you should probably use `kind-icon-mapping', which see. The
purpose here is overriding size and helping with scaling issues."
    :group 'corfu)
  (setq kind-icon-default-face 'corfu-kind-icon
        kind-icon-blend-background t
        kind-icon-blend-frac 0.2)
  (let ((def-style (svg-lib-style-compute-default 'corfu-kind-icon))
        res)
    (cl-loop for (key value) on def-style by 'cddr
             do (unless (member key '(:foreground
                                      :background
                                      :font-size
                                      :font-width
                                      :font-weight
                                      :font-family
                                      :width))
                  (setq res (plist-put res key value))))
    (setq kind-icon-default-style (plist-put res :stroke 0.25)))
  (defadvice! doom--kind-icon-remove-padding (orig kind)
    "Rescale icon images to 1, and set surrounding spaces to width 0.
This fixes the cropping due to scaling issues."
    :around #'kind-icon-formatted
    (let* ((text (funcall orig kind))
           (image (get-text-property 1 'display text)))
      (when (eq (car-safe image) 'image)
        (setf (image-property image :scale) 1)
        (put-text-property 0 1 'display '(space :width (0)) text)
        (put-text-property 2 3 'display '(space :width (0)) text))
      text)))

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
              "C-<up>" #'corfu-popupinfo-scroll-down
              "C-<down>" #'corfu-popupinfo-scroll-up
              "C-S-p" #'corfu-popupinfo-scroll-down
              "C-S-n" #'corfu-popupinfo-scroll-up
              "C-h" #'corfu-popupinfo-toggle)
        (:map 'corfu-popupinfo-map
         :when (modulep! :editor evil)
         ;; Reversed because popupinfo assumes opposite of what feels intuitive
         ;; with evil.
         "C-S-k" #'corfu-popupinfo-scroll-down
         "C-S-j" #'corfu-popupinfo-scroll-up)))
