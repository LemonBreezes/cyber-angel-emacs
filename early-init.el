;;; early-init.el -*- lexical-binding: t; -*-

;; Sometimes packages are not compiled. This is a workaround.
(when-let ((compile-angel-path (concat doom-local-dir
                                       (format "straight/build-%s/compile-angel/"
                                               emacs-version)))
           (_ (file-exists-p compile-angel-path)))
  (setq compile-angel-verbose t)
  (add-to-list 'load-path compile-angel-path)
  (require 'compile-angel)
  (compile-angel-on-load-mode +1)
  (setq native-comp-async-query-on-exit t)
  (setq confirm-kill-processes t)
  (setq package-native-compile t)

  ;; Exclude system directories/files from native compilation (idempotent)
  (dolist (path '("/usr/share/emacs/31.0.50/lisp/international/"
                  "/usr/share/emacs/31.0.50/lisp/leim/"
                  "/usr/share/emacs/31.0.50/lisp/subdirs.el"
                  "/usr/share/emacs/site-lisp/subdirs.el"))
    (add-to-list 'compile-angel-excluded-files path))
  (with-eval-after-load "savehist"
    (let ((filename (concat "/" (file-name-nondirectory savehist-file))))
      (unless (member filename compile-angel-excluded-files)
        (push filename compile-angel-excluded-files))))
  (with-eval-after-load "recentf"
    (let ((filename (concat "/" (file-name-nondirectory recentf-save-file))))
      (unless (member filename compile-angel-excluded-files)
        (push filename compile-angel-excluded-files))))
  (with-eval-after-load "cus-edit"
    (when (stringp custom-file)
      (let ((filename (concat "/" (file-name-nondirectory custom-file))))
        (unless (member filename compile-angel-excluded-files)
          (push filename compile-angel-excluded-files))))))

;; Local Variables:
;; after-save-hook: cae-doom-patch-emacs-early-init
;; End:
