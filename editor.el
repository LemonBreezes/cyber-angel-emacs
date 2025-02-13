;;; Editor configuration

(when cae-init-editor-enabled-p
  (load! "lisp/cae-repeat")
  (load! "lisp/cae-vlf")
  (load! "lisp/cae-restore-point")
  ;; ... (Editor configuration continues) ...
  (advice-add #'kill-this-buffer :around #'doom-set-jump-a))
