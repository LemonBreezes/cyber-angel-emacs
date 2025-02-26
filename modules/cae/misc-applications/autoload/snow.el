;;; cae/misc-applications/autoload/snow.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'cae-snow "cae/misc-applications/autoload/snow" nil t)
;;;###autoload (autoload 'cae-snow-quit "cae/misc-applications/autoload/snow" nil t)

(cae-define-game-launcher
 snow
 :launch-fn #'snow
 :buffer-name "*snow*"
 :workspace-name cae-snow-workspace-name)
