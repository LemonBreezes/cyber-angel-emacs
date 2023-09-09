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
  '((internal-border-width . 1)
    (width . 0.65)
    (height . 0.35)
    (min-width . 80)
    (min-height . 16))
  "Default parameters for the helm childframe.")

;;
;;; Packages

(unless (featurep 'helm)
  (map! :map ctl-x-map
        "c" (cae-oneshot-keymap helm-command-map helm-global-bindings)))

(after! helm-core
  (map! (:map helm-map
         [remap next-line] #'helm-next-line
         [remap previous-line] #'helm-previous-line
         [left] #'left-char
         [right] #'right-char
         [prior] #'helm-previous-page
         [next] #'helm-next-page
         "C-S-f" #'helm-previous-page
         "C-S-n" #'helm-next-source
         "C-S-p" #'helm-previous-source
         (:when (modulep! :editor evil +everywhere)
          "C-j" #'helm-next-line
          "C-k" #'helm-previous-line
          "C-S-j" #'helm-next-source
          "C-S-k" #'helm-previous-source
          ;; This removes a keybinding that copies text from the point to the
          ;; search query. It seems useful so maybe I'll rebind that in the future.
          "C-w" #'doom/delete-backward-word)
         "C-u" #'helm-delete-minibuffer-contents
         "C-s" #'helm-minibuffer-history
         ;; Swap TAB and C-z
         "TAB" #'helm-execute-persistent-action
         [tab] #'helm-execute-persistent-action
         "C-z" #'helm-select-action)
        (:after helm-ag :map helm-ag-map
         "C--" #'+helm-do-ag-decrease-context
         "C-=" #'+helm-do-ag-increase-context
         [left] nil
         [right] nil)
        (:after helm-files :map (helm-find-files-map helm-read-file-map)
         [C-return] #'helm-ff-run-switch-other-window
         "C-w" #'helm-find-files-up-one-level
         (:when (modulep! :editor evil +everywhere)
          "C-h" #'helm-find-files-up-one-level
          "C-l" #'helm-execute-persistent-action))
        (:after helm-locate :map helm-generic-files-map
         [C-return] #'helm-ff-run-switch-other-window)
        (:after helm-buffers :map helm-buffer-map
         [C-return] #'helm-buffer-switch-other-window)
        (:after helm-occur :map helm-occur-map
         [C-return] #'helm-occur-run-goto-line-ow)
        (:after helm-grep :map helm-grep-map
         [C-return] #'helm-grep-run-other-window-action)))

(use-package! helm
  :defer t
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

  ;; My preferences
  (setq helm-display-header-line t
        helm-echo-input-in-header-line t)

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
    (setq helm-completion-style 'emacs))
  :config
  (after! which-key
    (which-key-add-key-based-replacements "C-x c" "helm"))

  (set-popup-rule! "^\\*helm" :vslot -100 :size 0.3 :ttl nil)

  ;; Hide minibuffer if `helm-echo-input-in-header-line'
  (add-hook 'helm-minibuffer-set-up-hook #'helm-hide-minibuffer-maybe)

  ;; Use helpful instead of describe-* to display documentation
  (dolist (fn '(helm-describe-variable helm-describe-function))
    (advice-add fn :around #'doom-use-helpful-a)))

(use-package! helm-posframe
  :when (and (modulep! +childframe)
             (cae-display-graphic-p))
  :after helm :config
  (setq helm-posframe-poshandler #'posframe-poshandler-frame-center
        helm-posframe-width 0.65
        helm-posframe-height 0.35
        helm-posframe-min-width 160
        helm-posframe-min-height 20
        helm-posframe-border-width 1)
  (setq helm-posframe-parameters +helm-posframe-parameters)
  (advice-add #'helm-posframe-enable :around #'doom-shut-up-a)
  (advice-add #'helm-posframe-disable :around #'doom-shut-up-a)
  (helm-posframe-enable))

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

(autoload 'helm-info-emacs "helm-info" nil t)
(autoload 'helm-info-gnus "helm-info" nil t)
(after! helm-net
  (setq helm-net-prefer-curl t
        helm-google-suggest-search-url helm-surfraw-duckduckgo-url))

(use-package! helm-tramp
  :defer t :config
  (after! helm-global-bindings
    (map! :map helm-command-map
          "T" #'helm-tramp)))
