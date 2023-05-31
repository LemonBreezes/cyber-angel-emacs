;;; lisp/cae-project.el -*- lexical-binding: t; -*-

(defvar cae-project-bookmark-dir (concat doom-cache-dir "cae-project-bookmarks/")
  "Directory to store project bookmarks.")

(defvar cae-project-bookmark-cache (make-hash-table :test 'equal)
  "Cache of project bookmarks.")

(defvar cae-project-bookmark-separate-into-branches t
  "If non-nil, separate bookmarks into Git branches.")

(defun cae-project--get-bookmark-file (&optional project)
  "Return the bookmark file for PROJECT."
  (expand-file-name (concat (doom-project-name project)
                            "/"
                            (if cae-project-bookmark-separate-into-branches
                                (vc-git--symbolic-ref
                                 (or project
                                     (doom-project-root)))
                              "default")
                            ".bmk")
                    cae-project-bookmark-dir))

(defun cae-project--bookmark-alist-from-file (file)
  "Return a bookmark alist from FILE."
  (let ((bookmark-default-file file)
        (bookmark-alist nil))
    (when (file-exists-p file)
      (bookmark-load bookmark-default-file)
      bookmark-alist)))

(defun cae-project--bookmark-alist (&optional project)
  "Return the bookmark alist for the current project."
  (let ((file (cae-project--get-bookmark-file project)))
    (or (gethash file cae-project-bookmark-cache)
        (puthash file (cae-project--bookmark-alist-from-file file)
                 cae-project-bookmark-cache))))

(defun cae-project-bookmark-jump ()
  "Jump to a bookmark in the current project."
  (interactive)
  (let ((bookmark-alist (cae-project--bookmark-alist))
        (bookmark-default-file (cae-project--get-bookmark-file)))
    (ignore bookmark-alist bookmark-default-file)
    (call-interactively #'bookmark-jump)))

(defun cae-project-bookmark-set ()
  "Set a bookmark in the current project."
  (interactive)
  (let ((bookmark-alist (cae-project--bookmark-alist))
        (bookmark-default-file (cae-project--get-bookmark-file)))
    (ignore bookmark-alist bookmark-default-file)
    (call-interactively #'bookmark-set)
    (puthash bookmark-default-file bookmark-alist cae-project-bookmark-cache)))

(defun cae-project-bookmark-delete ()
  "Delete a bookmark in the current project."
  (interactive)
  (let ((bookmark-alist (cae-project--bookmark-alist))
        (bookmark-default-file (cae-project--get-bookmark-file)))
    (ignore bookmark-alist bookmark-default-file)
    (call-interactively #'bookmark-delete)))

(defun cae-project-bookmark-rename ()
  "Rename a bookmark in the current project."
  (interactive)
  (let ((bookmark-alist (cae-project--bookmark-alist))
        (bookmark-default-file (cae-project--get-bookmark-file)))
    (ignore bookmark-alist bookmark-default-file)
    (call-interactively #'bookmark-rename)))

(defun cae-project-bookmark-save ()
  "Save the current project's bookmarks."
  (interactive)
  (let ((bookmark-alist (cae-project--bookmark-alist))
        (bookmark-default-file (cae-project--get-bookmark-file)))
    (ignore bookmark-alist)
    (make-directory (file-name-directory bookmark-default-file) t)
    (bookmark-write-file bookmark-default-file)))

(defun cae-project-bookmark-save-all ()
  "Save all project bookmarks."
  (interactive)
  (maphash (lambda (bookmark-default-file bookmark-alist)
             (ignore bookmark-alist)
             (when bookmark-alist
               (make-directory (file-name-directory bookmark-default-file) t)
               (bookmark-write-file bookmark-default-file)))
           cae-project-bookmark-cache))

(add-hook 'kill-emacs-hook #'cae-project-bookmark-save-all)

(define-prefix-command 'cae-project-bookmark-map)
(map! :map cae-project-bookmark-map
      :desc "Jump to bookmark" "j" #'cae-project-bookmark-jump
      :desc "Set bookmark" "s" #'cae-project-bookmark-set
      :desc "Delete bookmark" "d" #'cae-project-bookmark-delete
      :desc "Rename bookmark" "r" #'cae-project-bookmark-rename
      :desc "Save bookmarks" "S" #'cae-project-bookmark-save)
(map! :leader
      :prefix "p"
      :desc "Project bookmarks" "C-b" #'cae-project-bookmark-map)
