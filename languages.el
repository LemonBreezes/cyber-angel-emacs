;;; languages.el -*- lexical-binding: t; -*-


;;; Logs

;; Do not highlight quoted strings in syslog-mode because sometimes they
;; aren't balanced, which breaks font-lock.
(after! syslog-mode
  (setq syslog-font-lock-keywords
        (cl-remove-if
         (lambda (keyword)
           (cl-destructuring-bind (regexp . face) keyword
             (string= "'[^']*'" regexp)))
         syslog-font-lock-keywords)
        ;; I don't use syslog notes.
        syslog-note-thing #'ignore))
(add-hook 'syslog-mode-hook #'cae-apply-ansi-color-to-buffer-h)
(advice-add #'syslog-load-notes :override #'ignore)
(add-to-list 'auto-mode-alist '("/var/log.*\\'" . syslog-mode))
(add-to-list 'auto-mode-alist '("\\.log\\'" . syslog-mode))

;;; C/C++

(add-hook 'c-mode-common-hook #'subword-mode)

;;; Fennel

(add-hook 'fennel-mode-hook #'outline-minor-mode)

;;; Python

(after! nose
  (when (executable-find "nose2")
    (setq nose-global-name "nose2")))
(when (modulep! :lang python +pyright)
  (after! lsp-pyright
    (setq lsp-pyright-langserver-command "basedpyright")))

;;; Idris

(after! idris-settings
  (when (executable-find "idris2")
    (setq idris-interpreter-path "idris2")))

;;; Lua

(add-hook 'lua-mode-hook #'subword-mode)
(add-hook 'lua-mode-hook #'outline-minor-mode)
(setq-hook! 'lua-mode-hook
  outline-regexp "[ 	]*---\\(-*\\**\\) [^ 	\n]")

;; Prevent heading backgrounds from being overwritten.
(when (bound-and-true-p cae-theme-extend-heading-faces)
  (after! lsp
    (add-hook! 'lua-mode-hook
      (defun cae-lsp-do-not-highlight-comments-h ()
        (setq-local lsp-semantic-token-faces
                    (assoc-delete-all "comment" lsp-semantic-token-faces))))))

;;; Lean 4

(setq-hook! 'lean4-mode-hook
  tab-width 2)

;;; Haskell

(use-package! consult-hoogle
  :when (modulep! :lang haskell)
  :defer t :init
  (after! haskell-mode
    (map! :map haskell-mode-map
          :localleader
          "g" #'consult-hoogle
          "G" #'consult-hoogle-project)))
