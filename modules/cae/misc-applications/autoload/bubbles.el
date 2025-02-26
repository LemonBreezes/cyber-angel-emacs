;;; cae/misc-applications/autoload/bubbles.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'cae-bubbles "cae/misc-applications/autoload/bubbles" nil t)
;;;###autoload (autoload 'cae-bubbles-quit "cae/misc-applications/autoload/bubbles" nil t)

(cae-define-launcher
 cae-bubbles
 :launch-fn #'bubbles
 :buffer-name "*bubbles*"
 :workspace-name cae-bubbles-workspace-name
 :setup-fn
 (lambda ()
   (with-current-buffer "*bubbles*"
     (local-set-key (kbd "q") #'cae-bubbles-quit)
     (when (featurep 'evil)
       (evil-local-set-key 'normal (kbd "q") #'cae-bubbles-quit)))))
