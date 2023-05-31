;;; lisp/cae-project.el -*- lexical-binding: t; -*-

(defvar cae-project-bookmark-dir (concat doom-cache-dir "cae-project-bookmarks/")
  "Directory to store project bookmarks.")

(defun cae-project-get-bookmark-file (project)
  "Return the bookmark file for PROJECT."
  (expand-file-name "bookmarks" (doom-project-root project)))

(defun cae-project-bookmark-jump ())

(defun cae-project-bookmark-set ())
