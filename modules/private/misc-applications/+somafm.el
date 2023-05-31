;;; private/misc-applications/somafm.el -*- lexical-binding: t; -*-

(use-package! somafm
  :defer-incrementally t                ; Somafm is _really_ slow to start up so
                                        ; we lazily load Somafm and refresh the
                                        ; channels.
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        "@" #'somafm)
  :config
  (somafm--refresh-channels)
  (map! :map somafm-mode-map
        :n "q" #'bury-buffer
        :n "r" #'somafm--refresh-channels
        :n "R" #'somafm--refresh-and-show-channels-buffer
        :n "s" #'somafm--sort
        :n "x" #'somafm--stop))
