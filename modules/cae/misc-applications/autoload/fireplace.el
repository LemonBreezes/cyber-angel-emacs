;;;###autoload (autoload 'cae-fireplace "cae/misc-applications/autoload/fireplace" nil t)
;;;###autoload (autoload 'cae-fireplace-quit "cae/misc-applications/autoload/fireplace" nil t)

;;; private/misc-applications/autoload/fireplace.el -*- lexical-binding: t; -*-

(cae-define-launcher
  cae-fireplace
  :launch-fn #'fireplace
  :buffer-name fireplace-buffer-name
  :workspace-name cae-fireplace-workspace-name)
