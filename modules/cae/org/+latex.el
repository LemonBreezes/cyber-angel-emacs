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

(use-package! lattie
  :defer t :init
  (add-transient-hook! 'org-cdlatex-mode-hook
    (map! :map org-cdlatex-mode-map
          "]" #'lattie-close-bracket
          "[" #'lattie-open-bracket
          "(" #'lattie-open-paren
          "j" #'special-lattie-down
          ;; "-" #'special-lattie-punctuation
          "-" #'lattie-self-insert-command
          "SPC" #'special-lattie-space
          "RET" #'special-lattie-newline-and-indent
          "^" #'special-lattie-underscore-caret
          [remap org-cdlatex-underscore-caret] #'special-lattie-underscore-caret
          "_" #'special-lattie-underscore-caret
          [return] #'special-lattie-newline-and-indent
          "." #'special-lattie-space
          "," #'special-lattie-space
          "+" #'lattie-insert-dollar
          "k" #'special-lattie-up
          "h" #'special-lattie-backward
          "l" #'special-lattie-forward
          "f" #'special-lattie-flow
          "e" #'special-lattie-toggle-latex-fragment
          "$" #'special-lattie-dollar
          "{" #'special-lattie-open-brace
          "}" #'special-lattie-close-brace
          ;; "c" #'special-lattie-compile
          "`" #'special-lattie-grave
          "'" #'special-lattie-grave
          "0" #'special-lattie-digit-or-bol
          "1" #'special-lattie-digit
          "^" #'special-lattie-back-to-heading
          ;; "^" #'org-cdlatex-underscore-caret
          "2" #'special-lattie-digit
          "3" #'special-lattie-digit
          "4" #'special-lattie-digit
          "5" #'special-lattie-digit
          "6" #'special-lattie-digit
          "7" #'special-lattie-digit
          "8" #'special-lattie-digit
          "9" #'special-lattie-digit
          ;; "=" #'special-lattie-equals
          ;; "/" #'special-lattie-slash
          "DEL" #'lattie-delete-backward
          [remap backward-kill-word] #'backward-kill-word
          [remap org-self-insert-command] #'lattie-self-insert-command
          [remap self-insert-command] #'lattie-self-insert-command
          ";" #'lattie-self-insert-command
          "?" #'lattie-self-insert-command
          "=" #'lattie-self-insert-command
          ")" #'lattie-self-insert-command
          [tab] #'special-lattie-tab
          "C-c C-k" #'lattie-kill-note-or-show-branches
          ;; "C-c C-t" #'lattie-org-todo
          ;;:map evil-org-mode-map
          ;;:i [return] #'special-lattie-newline-and-indent
          ;;:i "DEL" #'lattie-delete-backward
          ;;:i "RET" #'special-lattie-newline-and-indent
          )

    ;; Commented because I have since created other Org return DWIM
    ;; functionality that is not synced up with the Lattie codebase.
    ;;(add-hook 'evil-org-mode-hook
    ;;          (defun lattie--remap-return ()
    ;;            (map! :map evil-org-mode-map
    ;;                  :i [return] #'special-lattie-newline-and-indent
    ;;                  :i "RET" #'special-lattie-newline-and-indent)))
    ))
