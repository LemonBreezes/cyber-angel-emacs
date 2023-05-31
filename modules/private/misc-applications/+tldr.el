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
    (set-popup-rule! "^\\*tldr\\*$" :size #'cae-popup-resize-help-buffer
      :side 'right :ttl t :select t :quit t :ttl 0) ; which slot/vslot?
    (advice-add #'tldr :around
                (cae-defun +tldr-use-popup-buffer-a (fn &rest args)
                  (cl-letf (((symbol-function #'switch-to-buffer-other-window)
                             (symbol-function #'display-buffer)))
                    (apply fn args))))))
