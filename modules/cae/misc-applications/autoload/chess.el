;;; cae/misc-applications/autoload/chess.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'cae-chess "cae/misc-applications/autoload/chess" nil t)
;;;###autoload (autoload 'cae-chess-quit "cae/misc-applications/autoload/chess" nil t)


(cae-define-launcher
  cae-chess
  :launch-fn #'chess
  :buffer-name "*chess*"
  :workspace-name cae-chess-workspace-name
  :setup-fn
  (lambda ()
    (dolist (win (window-list))
      (if (parent-mode-is-derived-p (buffer-local-value 'major-mode (window-buffer win))
                                    'chess-display-mode)
          (when (modulep! :ui workspaces)
            (persp-add-buffer (window-buffer win)))
        (delete-window win)))))
