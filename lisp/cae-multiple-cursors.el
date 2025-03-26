;;; lisp/cae-multiple-cursors.el -*- lexical-binding: t; -*-

(after! evil-mc
  (add-to-list 'evil-mc-known-commands
               '(doom/delete-backward-word . ((:default . evil-mc-execute-default-call-with-count)))))
