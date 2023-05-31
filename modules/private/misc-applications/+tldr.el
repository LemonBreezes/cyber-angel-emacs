;;; private/misc-applications/tldr.el -*- lexical-binding: t; -*-

(use-package! tldr
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-lookup-prefix
        "t" #'tldr)
  :config
  (setq tldr-directory-path
        (expand-file-name "tldr" doom-data-dir))
  (when (modulep! :ui popup)
    (advice-add #'tldr :around
                (cae-defun +tldr-use-popup-buffer-a (fn &rest args)
                  (cl-letf (((symbol-function #'switch-to-buffer-other-window)
                             (symbol-function #'display-buffer)))
                    (apply fn args))))))
