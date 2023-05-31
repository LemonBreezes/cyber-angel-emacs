;;; lisp/cae-workspaces.el -*- lexical-binding: t; -*-

(defun cae-workspaces-save-bookmarks-h ())

(defun cae-workspaces-save-load-bookmarks-h ())

(add-hook 'persp-before-deactivate-functions #'cae-workspaces-save-bookmarks-h)
(add-hook 'persp-before-activate-functions #'cae-workspaces-save-load-bookmarks-h)
