;;; trash/config-use-package-blocks.el -*- lexical-binding: t; -*-

(use-package! aggressive-indent
  :disabled t
  :defer t :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode) ;See my `lisp'
                                        ;module.
  (add-hook 'c-mode-common-hook #'aggressive-indent-mode)
  :config
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (bound-and-true-p lsp-mode)
         (or (and lsp-enable-on-type-formatting
                  (lsp--capability "documentOnTypeFormattingProvider"))
             (and lsp-enable-indentation
                  (lsp--capability "documentRangeFormattingProvider")))))
  (dolist (command '(lsp-format-buffer
                     lsp-format-region
                     lsp-organize-imports
                     lsp-organize-imports-remove-unused
                     prog-fill-reindent-defun
                     indent-pp-sexp
                     save-buffer
                     indent-for-tab-command))
    (add-to-list 'aggressive-indent-protected-commands command))
  (add-to-list 'aggressive-indent-dont-indent-if '(bound-and-true-p lispy-mode)))

(use-package! hungry-delete
  :defer t :init (add-hook 'aggressive-indent-mode-hook #'hungry-delete-mode)
  :config
  (when (modulep! :config default +smartparens)
    (map! :map hungry-delete-mode-map
          [remap backward-delete-char-untabify] #'sp-backward-delete-char
          [remap c-electric-backspace] #'sp-backward-delete-char
          [remap c-electric-delete-forward] #'cae-delete-char
          [remap delete-backward-char] #'sp-backward-delete-char
          [remap delete-char] #'cae-delete-char
          [remap delete-forward-char] #'cae-delete-char))
  (add-to-list 'hungry-delete-except-modes 'eshell-mode))

(use-package! tabgo
  :commands tabgo :defer t
  :config
  (setq tabgo-tab-line-keys (cae-keyboard-kbd tabgo-tab-line-keys)))

(use-package! jinx
  :defer t :init
  (dolist (hook '(text-mode-hook prog-mode-hook conf-mode-hook))
    (add-hook hook #'jinx-mode)))

(use-package! w3m
  :defer t :config
  (setq w3m-user-agent
        (string-join
         '("Mozilla/5.0"
           "(Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40)"
           "AppleWebKit/533.1""(KHTML, like Gecko)" "Version/4.0"
           "Mobile Safari/533.")
         " ")
        w3m-command-arguments '("-cookie" "-F"))
  (after! w3m-search
    (setq w3m-search-default-engine "duckduckgo"))
  (map! :map w3m-mode-map
        "o" #'ace-link-w3m))

(autoload 'cae-project-bookmark (concat doom-user-dir
                                        "lisp/cae-project-bookmark"))
(autoload 'cae-project-bookmark-set (concat doom-user-dir
                                            "lisp/cae-project-bookmark"))
(autoload 'cae-project--get-bookmark-file (concat doom-user-dir
                                                  "lisp/cae-project-bookmark"))
(map! :desc "project-bookmark" "C-x r p" #'cae-project-bookmark
      :desc "project-bookmark-set" "C-x r P" #'cae-project-bookmark-set)

(use-package! dwim-shell-command
  :defer t :init
  (autoload 'dwim-shell-command "dwim-shell-command" nil t)
  (map! [remap shell-command] #'dwim-shell-command
        (:after dired
         :map dired-mode-map
         [remap dired-do-async-shell-command] #'dwim-shell-command
         [remap dired-do-shell-command] #'dwim-shell-command
         [remap dired-smart-shell-command] #'dwim-shell-command)))

(use-package! topsy
  :defer t :init (add-hook 'prog-mode-hook #'topsy-mode)
  :config
  ;; Set custom function for rjsx-mode
  ;; Disable topsy-mode for gptel-mode
  (setf (alist-get 'rjsx-mode topsy-mode-functions) #'cae-ui-topsy-rjsx-fn)
  (add-hook 'gptel-mode-hook
            (cae-defun cae-disable-topsy-in-gptel-h ()
              "Disable topsy-mode in `gptel-mode'." ;`gptel' is Karthink's
                                        ;package.
              (topsy-mode -1))))
