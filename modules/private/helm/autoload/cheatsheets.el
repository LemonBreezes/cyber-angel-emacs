;;; private/helm/autoload/cheatsheets.el -*- lexical-binding: t; -*-

;;;###autoload
(defhydra +helm-top-cheatsheet-hydra (:color pink :foreign-keys run)
  ("M-C" helm-top-run-sort-by-com "sort by com")
  ("M-M" helm-top-run-sort-by-mem "sort by mem")
  ("M-P" helm-top-run-sort-by-cpu "sort by cpu")
  ("M-U" helm-top-run-sort-by-user "sort by user"))
