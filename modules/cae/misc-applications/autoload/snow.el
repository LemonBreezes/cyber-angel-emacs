;;; private/misc-applications/autoload/snow.el -*- lexical-binding: t; -*-

(cae-define-launcher
  cae-snow
  :launch-fn #'snow
  :buffer-name "*snow*"
  :workspace-name cae-snow-workspace-name
  :setup-fn
  (lambda ()
    (with-current-buffer "*snow*"
      (local-set-key (kbd "q") #'cae-snow-quit)
      (when (featurep 'evil)
        (evil-local-set-key 'normal (kbd "q") #'cae-snow-quit)))))
