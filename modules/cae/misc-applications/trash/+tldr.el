;;; private/misc-applications/tldr.el -*- lexical-binding: t; -*-

;; All the lookup packages got removed because ChatGPT essentially made them
;; obsolete.

(use-package! tldr
  :defer t
  :init
  (map! :map +misc-applications-lookup-map
        "t" #'tldr)
  :config
  (setq tldr-directory-path
        (expand-file-name "tldr" doom-data-dir))
  (when (modulep! :ui popup)
    (defadvice! +tldr-use-popup-buffer-a (fn &rest args)
      :around #'tldr
      (cl-letf (((symbol-function #'switch-to-buffer-other-window)
                 (symbol-function #'display-buffer)))
        (apply fn args)))))
