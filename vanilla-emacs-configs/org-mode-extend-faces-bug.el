;;; vanilla-emacs-configs/org-mode-extend-faces-bug.el -*- lexical-binding: t; -*-

(let ((default-directory (file-name-directory load-file-name)))
  (when (file-exists-p (expand-file-name "default.el" default-directory))
    (load-file (expand-file-name "default.el" default-directory))))

(require 'org)
;;
;;
;;
;;
;;
;;
;;
;;;
(load-theme 'leuven t)

(dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5
                org-level-6 org-level-7 org-level-8))
  (set-face-attribute face nil :extend t))

(setq org-fontify-whole-heading-line t)
(scratch-buffer)
(org-mode)
(insert "#+title: (Infty, 2) Reading Group
#+ROAM_TAGS: \"Quasicategories\"

* [2020-09-11 Fri] Links passed around in today's meeting
:PROPERTIES:
:CREATED_TIME: [2020-09-12 Sat 11:10]
:END:

** [[https://arxiv.org/abs/2006.07997][Internal enriched categories]]
:PROPERTIES:
:CREATED_TIME: [2020-09-12 Sat 11:11]
:END:

** [[https://www.i2m.univ-amu.fr/perso/dimitri.ara/files/qcatsup.pdf][Higher quasi-categories vs higher Rezk spaces]]
:PROPERTIES:
:CREATED_TIME: [2020-09-12 Sat 11:12]
:END:

** [[https://arxiv.org/pdf/1712.06469.pdf][∞-operads as analytic monads]]
:PROPERTIES:
:CREATED_TIME: [2020-09-12 Sat 11:13]
:END:

** [[https://arxiv.org/pdf/2002.01037.pdf][On lax transformations, adjunctions, and monads in (∞,2)-categories]]
:PROPERTIES:
:CREATED_TIME: [2020-09-12 Sat 11:14]
:END:

** [[https://acmbl.github.io/straight_slides.pdf][A modular proof of the straightening theorem]]
:PROPERTIES:
:CREATED_TIME: [2020-09-20 Sun 13:20]
:END:

* [2020-09-18 Fri 14:29] Links passed around
:PROPERTIES:
:CREATED_TIME: [2020-09-18 Fri 14:29]
:END:

** [[https://arxiv.org/abs/1502.06526][Twisted TQFTs]]
:PROPERTIES:
:CREATED_TIME: [2020-09-18 Fri 14:29]
:END:

** [[https://arxiv.org/pdf/2003.11757.pdf][Lax Gray tensor product for 2-categories]]
:PROPERTIES:
:CREATED_TIME: [2020-09-18 Fri 14:30]
:END:

")
