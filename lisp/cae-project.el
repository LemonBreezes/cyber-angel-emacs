;;; lisp/cae-project.el -*- lexical-binding: t; -*-

;; (vc-git--symbolic-ref (buffer-file-name))

(defun cae-project--bookmark-file (&optional project-root)
  (concat (or project-root
              (doom-project-root)
              (persp-parameter 'last-project-root))
          ".bookmarks/"
          (vc-git--symbolic-ref (buffer-file-name))))

(defun cae-project-bookmark-load-h (_)
  (when-let ((bookmark-default-file (cae-project--bookmark-file)))
    (when (file-exists-p bookmark-default-file)
      (bookmark-load bookmark-default-file)
      (set-persp-parameter 'bookmark-file bookmark-default-file))))

(defun cae-project-bookmark-save-h (_)
  (when-let ((bookmark-default-file (cae-project--bookmark-file)))
    (when (and (string= (doom-project-name) (persp-name (get-current-persp)))
               (cae-project--bookmark-file))
      (set-persp-parameter 'bookmark-file bookmark-default-file)
      (bookmark-save))))

(add-hook 'persp-before-deactivate-functions #'cae-project-bookmark-save-h)
(add-hook 'persp-activated-functions #'cae-project-bookmark-load-h)

;; TODO make the bookmark file update when the branch changes
