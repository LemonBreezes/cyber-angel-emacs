;;; lisp/cae-compile.el -*- lexical-binding: t; -*-

(defun cae-setup-compile-angel-exclusions ()
  "Set up exclusions for native compilation."
  ;; Basic exclusions
  (add-to-list 'compile-angel-excluded-files "/early-init.el")
  (add-to-list 'compile-angel-excluded-files "/subdirs.el")

  ;; Files that fail to compile
  (add-to-list 'compile-angel-excluded-files "/aidermacs-backends.el")

  ;; Exclude various configuration files
  (with-eval-after-load "savehist"
    (add-to-list 'compile-angel-excluded-files
                 (concat "/" (file-name-nondirectory savehist-file))))

  (with-eval-after-load "recentf"
    (add-to-list 'compile-angel-excluded-files
                 (concat "/" (file-name-nondirectory recentf-save-file))))

  (with-eval-after-load "cus-edit"
    (when (stringp custom-file)
      (add-to-list 'compile-angel-excluded-files
                   (concat "/" (file-name-nondirectory custom-file)))))

  ;; /home/st/.config/emacs/.local/straight/build-31.0.50/aio/aio.el: Error Type aio-select missing from typeof-types!
  (add-to-list 'compile-angel-excluded-files "/aio/aio.el")

  (add-to-list 'compile-angel-excluded-files-regexps "/doom-snippets/.*")

  ;; Doom MODULE config files MUST load as source.  A bare `(modulep! +flag)'
  ;; has two macroexpansions (see `modulep!'): a constant when
  ;; `doom-module-context' is bound at expansion time -- which
  ;; `doom--startup-modules' binds per-module when loading SOURCE -- and a
  ;; fragile by-path runtime fallback otherwise.  compile-angel byte-compiles
  ;; these files STANDALONE (no module context), baking the fallback into the
  ;; `.elc'; it then fails at load with `void-variable +flag' (observed: `+eglot'
  ;; in `:tools lsp').  Since `load' prefers `.elc', one such file silently
  ;; breaks every later module load -- the pdump child's `doom-startup',
  ;; `doom/reload', and any non-dumped boot.  Never compile under a module root.
  (dolist (dir (bound-and-true-p doom-module-load-path))
    (when (stringp dir)
      (add-to-list 'compile-angel-excluded-files-regexps
                   (regexp-quote (file-name-as-directory (expand-file-name dir)))))))

(use-package! compile-angel
  :defer 10.0 :config
  (setq compile-angel-use-file-index t)
  (setq compile-angel-enable-native-compile t)
  (setq compile-angel-exclude-core-emacs-directory t)
  (setq compile-angel-native-compile-load t)

  ;;(setq compile-angel-debug t)
  ;;(setq compile-angel-verbose t)
  ;;(setq compile-angel-track-file-index-stats t)

  ;; Set up exclusions
  (cae-setup-compile-angel-exclusions)
  (compile-angel-on-load-mode +1))

;; Ensure local elisp packages are up-to-date.
;; Do not do this when we check out Emacs from Git.
(unless emacs-repository-version
  (add-hook! 'emacs-lisp-mode-hook
    (add-hook 'after-save-hook #'cae-compile-rebuild-package nil t)))
