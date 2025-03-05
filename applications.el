;;; applications.el -*- lexical-binding: t; -*-

(when (modulep! :ui workspaces)
  (add-hook 'circe-channel-mode-hook
            (lambda ()
              (when (+workspace-exists-p +irc--workspace-name)
                (persp-add-buffer (current-buffer)))))
  (cae-defadvice! cae-ir-inhibit-workspace-saving-a (&optional inhibit-workspace)
    :after #'+irc-setup-wconf
    (when (and (modulep! :ui workspaces)
               (not inhibit-workspace))
      (set-persp-parameter 'dont-save-to-file t (+workspace-get +irc--workspace-name)))))
