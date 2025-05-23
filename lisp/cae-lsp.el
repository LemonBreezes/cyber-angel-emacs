;;; lisp/cae-lsp.el -*- lexical-binding: t; -*-

(when (modulep! :tools lsp -eglot)
  (cae-advice-add #'lsp-ui-doc--setup-mouse :override #'ignore)
  (cae-advice-add #'lsp-ui-doc--disable-mouse-on-prefix :override #'ignore)
  (cae-advice-add #'dap-tooltip-update-mouse-motions-if-enabled :override #'ignore)
  ;; Fixes an error I got from `lsp!'.
  (autoload 'lsp--suggest-project-root "lsp-mode")
  (after! lsp-mode
    (setq lsp-enable-snippet t
          lsp-enable-folding t
          lsp-inlay-hint-enable t
          lsp-enable-indentation nil
          lsp-semantic-tokens-enable t
          lsp-enable-file-watchers nil
          ;; BUG This is causing errors where it says Clangd does not suppor
          ;; inlay hints even though it does.
          lsp-update-inlay-hints-on-scroll nil
          ;; Doom disables these but I'll leave them on. The reason Doom
          ;; disables them is in the comments.
          lsp-enable-text-document-color t   ; performance
          lsp-enable-on-type-formatting t    ; unexpected modifications
          lsp-enable-folding t               ; performance
          lsp-headerline-breadcrumb-enable t ; redundant with modeline and imenu
          ;; For some reason LSP isn't working for me over Tramp as well as
          ;; Eglot does. At least not over `sudo'.
          lsp-auto-register-remote-clients nil)
    (after! lsp-ui
      (setq lsp-signature-auto-activate t
            lsp-ui-doc-include-signature t
            lsp-ui-doc-header nil))
    (after! lsp-ui-sideline
      (setq lsp-ui-sideline-show-hover t
            lsp-ui-sideline-show-code-actions t))
    (after! lsp-clangd
      (setq lsp-clients-clangd-args
            `(,(format "-j=%d" (max 1 (/ (doom-system-cpus) 2)))
              "--background-index"
              "--clang-tidy"
              "--completion-style=detailed"
              "--header-insertion=never"
              "--header-insertion-decorators=0")))
    (after! lsp-lua
      (setq lsp-lua-runtime-version "LuaJIT"
            lsp-lua-hint-enable t
            lsp-lua-hint-set-type t
            lsp-clients-lua-language-server-bin (executable-find "lua-language-server")
            lsp-clients-lua-lsp-server-install-dir lsp-clients-lua-language-server-bin
            lsp-clients-lua-language-server-main-location "/opt/lua-language-server/main.lua"))
    (after! lsp-json
      (plist-put lsp-json--schema-associations :/*.resume.json ["https://raw.githubusercontent.com/jsonresume/resume-schema/v1.0.0/schema.json"]))
    (add-to-list 'lsp-disabled-clients 'ccls)
    (add-to-list 'lsp-disabled-clients 'mspyls)

    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "fennel-ls")
                      :activation-fn (lsp-activate-on "fennel")
                      :server-id 'fennel-ls)))
  (when (modulep! :ui treemacs +lsp)
    (after! lsp-treemacs
      (lsp-treemacs-sync-mode +1)))
  (setq +treemacs-git-mode 'deferred
        lsp-treemacs-error-list-expand-depth 1)

  ;; These are from
  ;; https://www.reddit.com/r/emacs/comments/18ybxsa/emacs_lspmode_performance_booster/
  ;; and https://github.com/blahgeek/emacs-lsp-booster. They are meant to
  ;; improve the performance of LSP.
  (if (not (executable-find "emacs-lsp-booster"))
      (warn "Could not find emacs-lsp-booster executable.")
    (define-advice json-parse-buffer (:around (old-fn &rest args) lsp-booster-parse-bytecode)
      "Try to parse bytecode instead of json."
      (or
       (when (equal (following-char) ?#)
         (let ((bytecode (read (current-buffer))))
           (when (byte-code-function-p bytecode)
             (funcall bytecode))))
       (apply old-fn args)))

    (define-advice lsp-resolve-final-command (:around (old-fn cmd &optional test?) add-lsp-server-booster)
      "Prepend emacs-lsp-booster command to lsp CMD."
      (let ((orig-result (funcall old-fn cmd test?)))
        (if (and (not test?) ;; for check lsp-server-present?
                 (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                 lsp-use-plists
                 (not (functionp 'json-rpc-connection))) ;; native json-rpc
            (progn
              (message "Using emacs-lsp-booster for %s!" orig-result)
              (cons "emacs-lsp-booster" orig-result))
          orig-result)))))

(when (modulep! :tools lsp +eglot)
  (use-package! eglot-booster
    :after eglot :config (eglot-booster-mode +1))

  (after! eglot
    (setq eglot-sync-connect nil)
    (setq eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))
    (let ((clangd '("clangd" "--background-index" "--clang-tidy"
                    "--completion-style=detailed" "--header-insertion=never"
                    "--header-insertion-decorators=0")))
      (if (assoc '(c++-mode c-mode) eglot-server-programs)
          (setf (cdr (assoc '(c++-mode c-mode) eglot-server-programs)) clangd)
        (setq eglot-server-programs
              (cons (cons '(c++-mode c-mode) clangd)
                    eglot-server-programs))))
    (add-to-list 'eglot-server-programs '(fennel-mode . ("fennel-ls" )))
    (when (modulep! :lang lua +lsp)
      (add-hook 'fennel-mode-local-vars-hook #'lsp!)))

  (cae-advice-add #'eglot-ensure :before-until #'cae-eglot-ensure-no-home-or-root-directory)

  (use-package! breadcrumb
    :disabled t
    :defer t :init
    (add-hook 'prog-mode-hook #'breadcrumb-mode)
    (add-hook 'conf-mode-hook #'breadcrumb-mode)
    (add-hook 'text-mode-hook #'breadcrumb-mode)))

;; I don't really need LSP for the XML files I edit.
(remove-hook 'nxml-mode-local-vars-hook #'lsp!)

