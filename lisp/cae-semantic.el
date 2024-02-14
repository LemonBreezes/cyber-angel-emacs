;;; lisp/cae-semantic.el -*- lexical-binding: t; -*-

;; Disable Semantic.
(defadvice! cae--semantic-disable-a (&rest _)
  :override #'semantic-mode
  (backtrace))
