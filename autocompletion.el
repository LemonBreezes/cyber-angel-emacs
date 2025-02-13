;;; Autocompletion configuration

(when cae-init-autocompletion-enabled-p
  (when (modulep! :completion corfu)
    (load! "lisp/cae-corfu"))
  (setq ido-save-directory-list-file (concat doom-cache-dir "ido.last"))
  ;; ... (Autocompletion configuration continues) ...
  (after! helm
    (setq helm-split-window-default-side 'right)))
