;;; private/misc-applications/autoload/snow.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'cae-snow "cae/misc-applications/autoload/snow" nil t)
;;;###autoload (autoload 'cae-snow-quit "cae/misc-applications/autoload/snow" nil t)

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
