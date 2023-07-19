;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((fundamental-mode . ((eval .
                           (progn
                             (when (fboundp 'git-auto-commit-mode)
                               (git-auto-commit-mode 1)
                               (setq-local gac-automatically-push-p t))
                             (conf-mode)))))
 ((nil . ((eval . (progn
                    (when (fboundp 'git-auto-commit-mode)
                      (git-auto-commit-mode 1)
                      (setq-local gac-automatically-push-p t))))))))
