;;; lisp/cae-vlf.el -*- lexical-binding: t; -*-

;; Copied pretty much verbatim from
;; https://github.com/tecosaur/emacs-config/blob/master/config.org
(use-package! vlf-setup
  :defer t :init
  (advice-add #'files--ask-user-about-large-file :override #'cae-files--ask-about-large-file-vlf)
  :config
  (advice-remove 'abort-if-file-too-large #'ad-Advice-abort-if-file-too-large)
  (defvar-local cae-vlf-cumulative-linenum '((0 . 0))
    "An alist keeping track of the cumulative line number.")
  (add-hook 'vlf-after-chunk-update-hook #'cae-vlf-update-linum)

  ;; Since this only works with absolute line numbers, let's make sure we use them.
  (setq-hook! 'vlf-mode-hook display-line-numbers t)

  (add-hook! 'vlf-mode-hook (setq-local isearch-wrap-function #'cae-vlf-isearch-wrap))
  (after! vlf
    (map-keymap (lambda (k v)
                  (when (commandp v)
                    (eval `(map! :map vlf-prefix-map
                                 :localleader
                                 ,(if (vectorp k)
                                      (key-description k)
                                    (key-description (vector k)))
                                 ',v)
                          t)))
                vlf-mode-map)))
