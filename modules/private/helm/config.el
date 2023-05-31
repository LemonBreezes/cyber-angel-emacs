;;; private/helm/config.el -*- lexical-binding: t; -*-

(after! helm
  (setq helm-echo-input-in-header-line t)

  (when (modulep! :editor evil +everywhere)
    (setq helm-default-prompt-display-function #'+helm--set-prompt-display))

  (set-popup-rule! "^\\*helm" :vslot -100 :size 0.40 :ttl nil)

  ;; Hide the modeline in helm windows as it serves little purpose.
  (defun +helm--hide-mode-line (&rest _)
    (with-current-buffer (helm-buffer-get)
      (unless helm-mode-line-string
        (hide-mode-line-mode +1))))
  (add-hook 'helm-after-initialize-hook #'+helm--hide-mode-line)
  (advice-add #'helm-display-mode-line :override #'+helm--hide-mode-line)
  (advice-add #'helm-ag-show-status-default-mode-line :override #'ignore)

  ;; Hide minibuffer if `helm-echo-input-in-header-line'
  (add-hook 'helm-minibuffer-set-up-hook #'helm-hide-minibuffer-maybe)

  ;; Use helpful instead of describe-* to display documentation
  (dolist (fn '(helm-describe-variable helm-describe-function))
    (advice-add fn :around #'doom-use-helpful-a))

  (after! which-key
    (which-key-add-key-based-replacements "C-x c" "helm"))

  (map! :map helm-map
        :ig "C-j" #'helm-next-line
        :ig "C-k" #'helm-previous-line
        :ig "M-j" #'helm-next-source
        :ig "M-k" #'helm-previous-source
        :ig "C-S-<backspace>" #'helm-delete-minibuffer-contents
        :ig "C-z" #'helm-execute-persistent-action))

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
  :defer t
  :init
  (add-hook 'helm-mode-hook #'helm-descbinds-mode))

(use-package! helm-icons
  :after helm
  :when (modulep! +icons)
  :init
  (setq helm-icons-provider 'all-the-icons)
  :config
  (helm-icons-enable))


(autoload 'helm-info-emacs "helm-info" nil t)
(autoload 'helm-info-gnus "helm-info" nil t)
(unless (featurep 'helm)
  (map! :map ctl-x-map
        "c" #'+helm-lazy-load))

(when (modulep! :app emms)
  (use-package! helm-emms
    :defer t
    :config
    (setq helm-emms-dired-directories (list (expand-file-name "/hdd/unindexed-music/"))
          helm-emms-use-track-description-function t
          helm-emms-directory-files-recursive-fn #'helm-emms-walk-directory-with-find
          helm-emms-default-sources '(helm-source-emms-files
                                      helm-source-emms-streams
                                      helm-source-emms-dired))))

(after! helm-net
  (setq helm-net-prefer-curl t
        helm-google-suggest-search-url helm-surfraw-duckduckgo-url))
