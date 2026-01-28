;;; cae/org/+latex.el -*- lexical-binding: t; -*-

(after! org
  (add-to-list 'org-latex-packages-alist '("" "amsmath"))
  (add-to-list 'org-latex-packages-alist '("" "amssymb"))
  (add-to-list 'org-latex-packages-alist '("" "mathrsfs"))
  (add-to-list 'org-latex-packages-alist '("" "mathtools"))
  (add-to-list 'org-latex-packages-alist '("" "tikz"))
  (add-to-list 'org-latex-packages-alist '("" "tikz-cd"))
  (add-to-list 'org-latex-packages-alist '("" "bbm"))
  (add-to-list 'org-latex-packages-alist '("" "bbold"))
  (add-to-list 'org-latex-packages-alist '("" "txfonts" t))
  (add-to-list 'org-latex-packages-alist '("mathcal" "eucal" t))
  (add-to-list 'org-latex-packages-alist '("" "newunicodechar"))
  (add-to-list 'org-latex-packages-alist '("" "ntheorem"))
  (add-to-list 'org-latex-packages-alist '("all" "xy"))
  (add-to-list 'org-latex-packages-alist 
               '("backend=biber, style=alphabetic, sorting=ynt" "biblatex" t))

  (setq org-highlight-latex-and-related '(native latex script entities))
  (setq org-pretty-entities-include-sub-superscripts nil)
  (setq org-latex-pdf-process
        '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f")
        org-preview-latex-default-process 'dvisvgm)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0)))

(use-package! lattie-unicode
  :defer t
  (load! "+latex-unicode"))
