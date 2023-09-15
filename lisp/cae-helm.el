;;; lisp/cae-helm.el -*- lexical-binding: t; -*-

(advice-add #'helm-packages-install :around #'cae-always-yes-a)
