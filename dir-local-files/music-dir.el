((nil
  . ((eval
      . (progn
          ;; Now that I know about `menu-item' keybindings, I could make this code
          ;; more robust but I'm too lazy to do it for now.
          (when (derived-mode-p 'dired-mode)
            (when (fboundp #'cae-emms-quick-access)
              (local-set-key (kbd "a") #'cae-emms-quick-access)
              (when (featurep 'evil)
                (evil-local-set-key 'normal (kbd "a")
                                    (cmd! () (if buffer-read-only
                                                 (call-interactively #'cae-emms-quick-access)
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
