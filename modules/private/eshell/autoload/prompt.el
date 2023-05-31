;;; private/eshell/autoload/prompt.el -*- lexical-binding: t; -*-

(defun cae-eshell-prompt ()
  (require 'shrink-path)
  (concat (let ((pwd (eshell/pwd)))
            (propertize (if (equal pwd "~")
                            pwd
                          (abbreviate-file-name (shrink-path-file pwd)))
                        'face '+eshell-prompt-pwd))
          (propertize (+eshell--current-git-branch)
                      'face '+eshell-prompt-git-branch)
          (propertize " λ" 'face (if (zerop eshell-last-command-status) 'success 'error))
          " "))
