;;; early-init.el -*- lexical-binding: t; -*-

;; Sometimes packages are not compiled. This is a workaround.
(use-package! compile-angel
  :defer nil :init
  (compile-angel-on-load-mode +1)
  (setq native-comp-async-query-on-exit t)
  (setq confirm-kill-processes t)
  (setq package-native-compile t)

  ;; Ensure that the value of `savehist-file` is updated before proceeding
  (with-eval-after-load "savehist"
    (let ((filename (concat "/" (file-name-nondirectory savehist-file))))
      (unless (member filename compile-angel-excluded-files)
        (push filename compile-angel-excluded-files))))

  ;; Ensure that the value of `recentf-save-file` is updated before proceeding
  (with-eval-after-load "recentf"
    (let ((filename (concat "/" (file-name-nondirectory recentf-save-file))))
      (unless (member filename compile-angel-excluded-files)
        (push filename compile-angel-excluded-files))))

  ;; Ensure that the value of `custom-file` is updated before proceeding
  (with-eval-after-load "cus-edit"
    (when (stringp custom-file)
      (let ((filename (concat "/" (file-name-nondirectory custom-file))))
        (unless (member filename compile-angel-excluded-files)
          (push filename compile-angel-excluded-files))))))

;; Local Variables:
;; after-save-hook: cae-doom-patch-emacs-early-init
;; End:
