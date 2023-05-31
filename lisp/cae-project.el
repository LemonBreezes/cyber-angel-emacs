;;; lisp/cae-project.el -*- lexical-binding: t; -*-

;; (vc-git--symbolic-ref (buffer-file-name))

(defun cae-project-root ()
  ;; TODO Handle the case where the current buffer is not visiting a file.
  (doom-project-root))

(defun cae-project--bookmark-file ()
  (concat (cae-project-root)
          ".bookmarks/"
          (vc-git--symbolic-ref (buffer-file-name))))

(defun cae-project-bookmark-load ()
  (let ((bookmark-default-file (cae-project--bookmark-file))
        (bookmark-alist nil))
    (when (file-exists-p bookmark-default-file)
      (bookmark-load bookmark-default-file)
      (set-persp-parameter 'bookmark-alist bookmark-alist))))

(advice-add #'+workspaces-switch-to-project-h :after #'cae-project-bookmark-load)

(defun cae-project-bookmark-jump ()
  (interactive)
  (let ((bookmark-alist (persp-parameter 'bookmark-alist)))
      (call-interactively #'bookmark-jump)))

(defun cae-project-bookmark-set ()
  (interactive)
  (let ((bookmark-default-file (cae-project--bookmark-file))
        (bookmark-alist (persp-parameter 'bookmark-alist))
        (bookmark-save-flag 1))
  (call-interactively #'bookmark-set)))

(map! :leader
      :prefix "w"
      :desc "Jump to project bookmark" "j" #'cae-project-bookmark-jump
      :desc "Set project bookmark" "m" #'cae-project-bookmark-set)

;; TODO make the bookmark file update when the branch changes
