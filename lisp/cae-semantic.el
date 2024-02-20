;;; lisp/cae-semantic.el -*- lexical-binding: t; -*-

;; Disable Semantic.
(defadvice! cae-semantic-disable-a (&rest _)
  :override #'semantic-mode
  ;; If something tries to enable semantic-mode, emit a backtrace.
  (backtrace))
