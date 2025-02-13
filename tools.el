;;; Tools configuration

(when cae-init-tools-enabled-p
  (when (modulep! :tools lsp)
    (load! "lisp/cae-lsp")
    (load! "lisp/cae-semantic"))
  (use-package! w3m
    :defer t :init (setq browse-url-secondary-browser-function #'w3m-browse-url)
    :config
    ;; ... (w3m and browse-url configuration) ...
    (map! :map w3m-mode-map "o" #'ace-link-w3m))
  (after! browse-url
    ;; ... (default browser configuration) ...
    )
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (add-to-list 'doom-large-file-excluded-modes 'nov-mode)
  (add-to-list 'auto-mode-alist '("/sway/.*config.*/" . i3wm-config-mode))
  (add-to-list 'auto-mode-alist '("/sway/config\\'" . i3wm-config-mode))
  ;; Set up printers.
  (after! lpr (setq printer-name "Brother_HL-2350DW"))
  (after! ps-print (setq ps-printer-name "Brother_HL-2350DW"))
  (after! pdf-misc
    (setq pdf-misc-print-program-executable (executable-find "lpr")))
  ;; Additional system and file management settings…
  (after! helm
    (setq helm-split-window-default-side 'right)))
