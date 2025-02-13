;;; Appendix configuration

(when cae-init-appendix-enabled-p
  (doom-load-packages-incrementally
   `(,@(when (modulep! :completion corfu)
         '(corfu))
     ;; ... (additional package load lists) ...
     ,@(when (modulep! :cae misc-applications)
         (nconc '(emms elfeed-tube empv somafm helm-emms helm-linux-disks helm-rage)))
     ;; ... possibly more entries ...
     )
   t)
  (unless (or (executable-find "termux-setup-storage")
              (not (cae-display-graphic-p)))
    (after! pdf-tools
      (pdf-tools-install t nil t)))
  (after! copilot
    (unless (or (not copilot-node-executable)
                (file-exists-p copilot-install-dir))
      (copilot-install-server)))
  ;; Do not spam me with warnings.
  (unless init-file-debug
    (setq warning-minimum-level :error
          warning-minimum-log-level :error)))
