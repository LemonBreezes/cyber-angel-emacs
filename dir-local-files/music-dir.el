((nil
  . ((eval
      . (progn
          (when (derived-mode-p 'dired-mode)
            (when (fboundp #'+emms-quick-access)
              (local-set-key (kbd "a") #'+emms-quick-access)
              (when (featurep 'evil)
                (evil-local-set-key 'normal (kbd "a")
                                    (cmd! () (if buffer-read-only
                                                 (call-interactively #'+emms-quick-access)
                                               (call-interactively (lookup-key evil-normal-state-map "a")))))))
            (when (fboundp #'emms-play-dired)
              (local-set-key (kbd "e")
                             (lambda ()
                               (interactive)
                               (call-interactively #'emms-play-dired)
                               (emms-shuffle)))
              (when (featurep 'evil)
                (evil-local-set-key 'normal (kbd "e")
                                    (lambda ()
                                      (interactive)
                                      (if buffer-read-only
                                          (progn
                                            (call-interactively #'emms-play-dired)
                                            (emms-shuffle))
                                        (call-interactively (lookup-key evil-motion-state-map "e"))))))))
          )))))
