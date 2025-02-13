;;; Term configuration

(when cae-init-term-enabled-p
  (after! em-glob
    (setq eshell-error-if-no-glob nil))
  (after! vterm
    (setq vterm-max-scrollback 100000))
  ;; ... (Term configuration continues) ...
  (use-package! comint-histories
    :after comint :config
    ;; ... (comint-histories configuration) ...
    ))
