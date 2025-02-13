;;; Applications configuration

(when cae-init-applications-enabled-p
  (when (modulep! :ui workspaces)
    (add-hook! 'circe-channel-mode-hook
      (defun cae-circe-add-channel-to-workspace-h ()
        (when (+workspace-exists-p +irc--workspace-name)
          (persp-add-buffer (current-buffer)))))
    (defadvice! cae-irc-inhibit-workspace-saving-a (&optional inhibit-workspace)
      :after #'+irc-setup-wconf
      (when (and (modulep! :ui workspaces)
                 (not inhibit-workspace))
        (set-persp-parameter 'dont-save-to-file t (+workspace-get +irc--workspace-name))))))
