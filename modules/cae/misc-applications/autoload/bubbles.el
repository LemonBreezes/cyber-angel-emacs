;;; cae/misc-applications/autoload/bubbles.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'cae-bubbles "cae/misc-applications/autoload/bubbles" nil t)
;;;###autoload (autoload 'cae-bubbles-quit "cae/misc-applications/autoload/bubbles" nil t)

(cae-define-game-launcher
 bubbles
 :launch-fn #'bubbles
 :buffer-name "*bubbles*"
 :workspace-name cae-bubbles-workspace-name)
