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

  (add-to-list 'compile-angel-excluded-files-regexps "/doom-snippets/.*"))

(use-package! compile-angel
  :defer 10.0 :config
  (setq compile-angel-use-file-index t)
  (setq compile-angel-guess-el-file-use-load-history t)
  ;; We already native compile packages asynchronously on load.
  (setq compile-angel-enable-native-compile nil)

  ;;(setq compile-angel-debug t)
  ;;(setq compile-angel-verbose t)
  ;;(setq compile-angel-track-file-index-stats t)

  (cae-ensure-emacs-dir-writable)

  ;; Set up exclusions
  (cae-setup-compile-angel-exclusions)
  (compile-angel-on-load-mode +1))

;; Ensure local elisp packages are up-to-date.
;; Do not do this when we check out Emacs from Git.
(unless emacs-repository-version
  (add-hook! 'emacs-lisp-mode-hook
    (add-hook 'after-save-hook #'cae-compile-rebuild-package nil t)))
