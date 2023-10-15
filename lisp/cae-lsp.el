;;; lisp/cae-lsp.el -*- lexical-binding: t; -*-

(when (and (modulep! :tools lsp)
           (not (modulep! :tools lsp +eglot)))
  (after! lsp-mode
    (setq lsp-headerline-breadcrumb-enable (not (or (featurep 'breadcrumb)
                                                    (featurep 'topsy)))
          lsp-enable-snippet t
          lsp-enable-folding t
          lsp-inlay-hint-enable t
          lsp-enable-indentation nil
          lsp-semantic-tokens-enable t
          ;; Doom disables these but I'll leave them on.
          lsp-enable-text-document-color t
          lsp-enable-on-type-formatting t
          lsp-enable-folding t)
    (after! lsp-ui
      (setq lsp-signature-auto-activate t
            lsp-ui-doc-include-signature t
            lsp-ui-doc-header nil))
    (after! lsp-ui-sideline
      (setq lsp-ui-sideline-show-hover nil
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

    (setf (alist-get 'fennel-mode lsp-language-id-configuration) "fennel")
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "fennel-ls")
                      :activation-fn (lsp-activate-on "fennel")
                      :server-id 'fennel-ls))))

(when (modulep! :tools lsp +eglot)
  (after! eglot
    (setq eglot-sync-connect nil)
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
      (add-hook 'fennel-mode-local-vars-hook #'lsp!))))

;; I don't really need LSP for the XML files I edit.
(remove-hook 'nxml-mode-local-vars-hook #'lsp!)

