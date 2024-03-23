((nil
  . ((eval
      . (progn
          (when (or (derived-mode-p 'lua-mode)
                    (derived-mode-p 'fennel-mode))
            (add-hook 'before-save-hook #'editorconfig-format-buffer nil t))

          ;; Set compile and test commands for the ATLAS project.
          (when buffer-file-name
            (setq-local projectile-project-run-cmd "love ."
                        projectile-run-use-comint-mode t))
          )))))

;; Local Variables:
;; no-byte-compile: t
;; End:
