;;; cae/misc-applications/autoload/snake.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'cae-snake "cae/misc-applications/autoload/snake" nil t)
;;;###autoload (autoload 'cae-snake-quit "cae/misc-applications/autoload/snake" nil t)

(cae-define-game-launcher
 snake
 :launch-fn #'snake
 :buffer-name "*Snake*"
 :workspace-name cae-snake-workspace-name
 :save-scores t
 :scores-file "snake-scores")
