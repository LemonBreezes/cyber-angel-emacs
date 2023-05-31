;;; lisp/cae-project.el -*- lexical-binding: t; -*-

(defvar cae-project-bookmark-dir (concat doom-cache-dir "cae-project-bookmarks/")
  "Directory to store project bookmarks.")

(defvar cae-project-bookmark-cache (make-hash-table :test 'equal)
  "Cache of project bookmarks.")

(defvar cae-project-bookmark-separate-into-branches t
  "If non-nil, separate bookmarks into branches.")

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
  (let ((project (doom-project-name))
        (bookmark (completing-read "Jump to bookmark: "
                                   (cae-project--bookmark-alist))))
    (bookmark-jump bookmark)))

(defun cae-project-bookmark-set ())
