;;; cae/misc-applications/autoload/flappymacs.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'cae-flappymacs "cae/misc-applications/autoload/flappymacs" nil t)
;;;###autoload (autoload 'cae-flappymacs-quit "cae/misc-applications/autoload/flappymacs" nil t)

(cae-define-game-launcher
 flappymacs
 :launch-fn #'flappymacs
 :buffer-name "*Flappymacs*"
 :workspace-name cae-flappymacs-workspace-name
 :evil-state 'emacs
 :save-scores nil)
