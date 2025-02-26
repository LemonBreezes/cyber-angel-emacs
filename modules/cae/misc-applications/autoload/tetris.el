;;; cae/misc-applications/autoload/tetris.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'cae-tetris "cae/misc-applications/autoload/tetris" nil t)
;;;###autoload (autoload 'cae-tetris-quit "cae/misc-applications/autoload/tetris" nil t)

(cae-define-game-launcher
 tetris
 :launch-fn #'tetris
 :buffer-name "*Tetris*"
 :workspace-name cae-tetris-workspace-name
 :save-scores t
 :scores-file "tetris-scores")
