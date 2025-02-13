;;; Languages configuration

(when cae-init-languages-enabled-p
  ;; Logs
  (after! syslog-mode
    (setq syslog-font-lock-keywords
          (cl-remove-if
           (lambda (keyword)
             (cl-destructuring-bind (regexp . face) keyword
               (string= "'[^']*'" regexp)))
           syslog-font-lock-keywords)
          syslog-note-thing #'ignore))
  ;; ... (Languages configuration continues, e.g. Python, Lua, etc.) ...
  (when (modulep! :lang python +pyright)
    (after! lsp-pyright
      (setq lsp-pyright-langserver-command "basedpyright")))
  ;; Other language setups…
  )
