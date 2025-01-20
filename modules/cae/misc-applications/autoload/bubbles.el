;;;###autoload (autoload 'cae-bubbles "cae/misc-applications/autoload/bubbles" nil t)
;;;###autoload (autoload 'cae-bubbles-quit "cae/misc-applications/autoload/bubbles" nil t)

;;; private/misc-applications/autoload/bubbles.el -*- lexical-binding: t; -*-

(cae-define-launcher
  cae-bubbles
  :launch-fn #'bubbles
  :buffer-name "*bubbles*"
  :workspace-name cae-bubbles-workspace-name)
