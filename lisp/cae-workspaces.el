;;; lisp/cae-workspaces.el -*- lexical-binding: t; -*-

(defun cae-workspaces-save-bookmarks-h (_))

(defun cae-workspaces-load-bookmarks-h (_))

(add-hook 'persp-before-deactivate-functions #'cae-workspaces-save-bookmarks-h)
(add-hook 'persp-before-activate-functions #'cae-workspaces-load-bookmarks-h)
