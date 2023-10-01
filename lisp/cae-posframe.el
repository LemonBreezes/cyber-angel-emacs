;;; lisp/cae-posframe.el -*- lexical-binding: t; -*-

;; A dedicated file where I handle the usage of posframes

(defvar cae-theme-posframe-enabled nil)

(remove-hook 'vertico-mode-hook #'vertico-posframe-mode)
(after! vertico-multiform
  (setq vertico-multiform-categories
        `((embark-keybinding grid)
          (consult-grep
           ,(if (and (cae-display-graphic-p)
                     (modulep! :completion vertico +childframe))
                'posframe 'buffer))
          (imenu ,@(if (and (cae-display-graphic-p)
                            (modulep! :completion vertico +childframe))
                       '(posframe grid) '(grid)))
          (consult-location ,(if (and (cae-display-graphic-p)
                                      (modulep! :completion vertico +childframe))
                                 'posframe 'buffer))
          ,@(if (cae-display-graphic-p)
                (if (modulep! :completion vertico +childframe)
                    (list t 'posframe)
                  nil)
              (list t 'flat)))))
