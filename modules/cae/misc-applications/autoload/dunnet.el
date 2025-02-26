;;; cae/misc-applications/autoload/dunnet.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'cae-dunnet "cae/misc-applications/autoload/dunnet" nil t)
;;;###autoload (autoload 'cae-dunnet-quit "cae/misc-applications/autoload/dunnet" nil t)

(cae-define-game-launcher
 dunnet
 :launch-fn #'dunnet
 :buffer-name "*dungeon*"
 :workspace-name cae-dunnet-workspace-name)
