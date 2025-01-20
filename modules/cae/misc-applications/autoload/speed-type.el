;;; private/misc-applications/autoload/speed-type.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'cae-speed-type-text "cae/misc-applications/autoload/speed-type" nil t)
;;;###autoload (autoload 'cae-speed-type-text-quit "cae/misc-applications/autoload/speed-type" nil t)

(cae-define-launcher
  cae-speed-type-text
  :launch-fn #'speed-type-text
  :buffer-name "speed-type"
  :workspace-name cae-speed-type-workspace-name)
