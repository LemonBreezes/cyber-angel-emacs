;;; private/misc-applications/autoload/paradox.el -*- lexical-binding: t; -*-

(defun cae-paradox-menu-quick-help ()
  (interactive)
  (if (and (memq last-command '(cae-paradox-menu-quick-help
                                paradox-menu-quick-help))
           ;;(string= (current-message)
           ;;         (mapconcat 'paradox--prettify-key-descriptor
           ;;                    paradox--key-descriptors "\n"))
           )
      (progn (clear-minibuffer-message)
             (message ""))
    (call-interactively #'paradox-menu-quick-help)))
