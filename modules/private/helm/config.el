;;; completion/helm/config.el -*- lexical-binding: t; -*-

;; Posframe (requires +childframe)
(defvar +helm-posframe-handler #'posframe-poshandler-frame-center
  "The function that determines the location of the childframe.
It should return a cons cell representing the X and Y coordinates. See
`posframe-poshandler-frame-center' as a reference.")

(defvar +helm-posframe-text-scale 1
  "The text-scale to use in the helm childframe. Set to nil for no scaling.
Can be negative.")

(defvar +helm-posframe-parameters
  '((internal-border-width . 8)
    (width . 0.65)
    (height . 0.35)
    (min-width . 80)
    (min-height . 16))
  "Default parameters for the helm childframe.")

;;
;;; Packages

(use-package! helm-mode
  :defer t
  :config
  ;; helm is too heavy for `find-file-at-point'
  (add-to-list 'helm-completing-read-handlers-alist (cons #'find-file-at-point nil)))


(use-package! helm
  :after helm-mode
  :preface
  (setq helm-candidate-number-limit 150
        ;; Remove extraineous helm UI elements
        helm-display-header-line nil
        helm-ff-auto-update-initial-value nil
        helm-find-files-doc-header nil
        ;; Default helm window sizes
        helm-display-buffer-default-width nil
        helm-display-buffer-default-height 0.25
        ;; When calling `helm-semantic-or-imenu', don't immediately jump to
        ;; symbol at point.
        helm-imenu-execute-action-at-once-if-one nil
        ;; Disable special behavior for left/right, M-left/right keys.
        helm-ff-lynx-style-map nil)

  (when (modulep! :editor evil +everywhere)
    (setq helm-default-prompt-display-function #'+helm--set-prompt-display))

  :init
  (let ((fuzzy (modulep! +fuzzy)))
    (setq helm-apropos-fuzzy-match fuzzy
          helm-bookmark-show-location fuzzy
          helm-buffers-fuzzy-matching fuzzy
          helm-ff-fuzzy-matching fuzzy
          helm-file-cache-fuzzy-match fuzzy
          helm-flx-for-helm-locate fuzzy
          helm-imenu-fuzzy-match fuzzy
          helm-lisp-fuzzy-completion fuzzy
          helm-locate-fuzzy-match fuzzy
          helm-projectile-fuzzy-match fuzzy
          helm-recentf-fuzzy-match fuzzy
          helm-semantic-fuzzy-match fuzzy)

    ;; Make sure that we have helm-multi-matching or fuzzy matching, (as
    ;; prescribed by the fuzzy flag) also in the following cases:
    ;;
    ;; - helmized commands that use `completion-at-point' and similar functions
    ;; - native commands that fall back to `completion-styles' like `helm-M-x'
    ;;
    ;; However, do not add helm completion styles to the front of
    ;; `completion-styles', since that would be overly intrusive. E.g., it
    ;; results in `company-capf' returning far to many completion candidates.
    ;; Instead, append those styles so that they act as a fallback.  Variable
    ;; completion-styles is ignored unless helm-completion-style is customized
    ;; to 'emacs.
    (setq helm-completion-style 'emacs)
    (add-to-list 'completion-styles (if fuzzy 'flex 'helm) t))

  :config
  (set-popup-rule! "^\\*helm" :vslot -100 :size 0.22 :ttl nil)

  ;; Hide minibuffer if `helm-echo-input-in-header-line'
  (add-hook 'helm-minibuffer-set-up-hook #'helm-hide-minibuffer-maybe)

  ;; Use helpful instead of describe-* to display documentation
  (dolist (fn '(helm-describe-variable helm-describe-function))
    (advice-add fn :around #'doom-use-helpful-a)))


(use-package! helm-posframe
  :when (modulep! +childframe)
  :hook (helm-mode . helm-posframe-enable)
  :config
  (setq helm-posframe-poshandler #'posframe-poshandler-frame-center
        helm-posframe-width 0.65
        helm-posframe-height 0.35
        helm-posframe-min-width 80
        helm-posframe-min-height 16
        helm-posframe-border-width 8))


(use-package! helm-flx
  :when (modulep! +fuzzy)
  :hook (helm-mode . helm-flx-mode))


(after! helm-rg
  (setq helm-rg-display-buffer-normal-method #'pop-to-buffer)
  (set-popup-rule! "^helm-rg-" :ttl nil :select t :size 0.45)
  (map! :map helm-rg-map
        "C-c C-e" #'helm-rg--bounce)
  (map! :map helm-rg--bounce-mode-map
        "q" #'kill-current-buffer
        "C-c C-c" (cmd! (helm-rg--bounce-dump) (kill-current-buffer))
        "C-x C-c" #'helm-rg--bounce-dump-current-file
        "C-c C-k" #'kill-current-buffer))


;;;###package helm-bookmark
(setq helm-bookmark-show-location t)


(after! helm-files
  (setq helm-boring-file-regexp-list
        (append (list "\\.projects$" "\\.DS_Store$")
                helm-boring-file-regexp-list)))


(defvar helm-generic-files-map (make-sparse-keymap))
(after! helm-locate
  (when (and IS-MAC
             (null helm-locate-command)
             (executable-find "mdfind"))
    (setq helm-locate-command "mdfind -name %s"))
  (set-keymap-parent helm-generic-files-map helm-map))

(use-package! helm-descbinds
  :hook (helm-mode . helm-descbinds-mode))

(use-package! helm-icons
  :when (modulep! +icons)
  :hook (helm-mode . helm-icons-enable)
  :init
  (setq helm-icons-provider 'all-the-icons)
  :config
  (when (eq helm-icons-provider 'all-the-icons)
    (setq helm-icons-mode->icon nil)))
