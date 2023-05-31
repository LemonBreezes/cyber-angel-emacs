;;; private/eshell/autoload/eshell.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-eshell-input-filter (str)
  "Filter some trivial commands from the input history."
  (not (or (string-blank-p str)
           (equal "cd" str)
           (string-prefix-p "cd " str)
           (string-prefix-p " " str))))

;;;###autoload
(defun cae-eshell-set-up-autocompletion ()
  (add-hook 'completion-at-point-functions
            #'cape-file nil t))

;;;###autoload
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
