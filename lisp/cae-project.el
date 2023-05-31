;;; lisp/cae-project.el -*- lexical-binding: t; -*-

;; (vc-git--symbolic-ref (buffer-file-name))

(defun cae-project-root ()
  ;; TODO Handle the case where the current buffer is not visiting a file.
  (doom-project-root))

(defun cae-project--bookmark-file ()
  (concat (cae-project-root)
          ".bookmarks/"
          (vc-git--symbolic-ref (buffer-file-name))))

(defun cae-project-bookmark-load-h ()
  (let ((bookmark-default-file (cae-project--bookmark-file))
        (bookmark-alist nil))
    (when (file-exists-p bookmark-default-file)
      (bookmark-load bookmark-default-file)
      (set-persp-parameter 'bookmark-alist bookmark-alist))))

(defun cae-project-bookmark-save-h ()
  (let ((bookmark-default-file (cae-project--bookmark-file)))
    (bookmark-save)))

(add-hook 'projectile-after-switch-project-hook #'cae-project-bookmark-load)

(setq bookmark-save-flag 1)


;; TODO make the bookmark file update when the branch changes
