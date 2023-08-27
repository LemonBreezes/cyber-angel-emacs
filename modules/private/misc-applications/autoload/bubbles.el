;;; private/misc-applications/autoload/bubbles.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+bubbles-hydra/body "private/misc-applications/autoload/bubbles" nil t)
(defhydra +bubbles-hydra (:color pink :hint nil)
  ("<f6>" nil "Exit" :exit t)
  ("q" +bubbles-quit nil :exit t)
  ("f" forward-char "Move right" :column "Movement")
  ("b" backward-char "Move left" :column "Movement")
  ("n" next-line "Move down" :column "Movement")
  ("p" previous-line "Move up" :column "Movement")
  ("ta" bubbles-set-graphics-theme-ascii "ASCII" :column "Set theme")
  ("tb" bubbles-set-graphics-theme-balls "Balls" :column "Set theme")
  ("te" bubbles-set-graphics-theme-emacs "Emacs" :column "Set theme")
  ("tc" bubbles-set-graphics-theme-circles "Circles" :column "Set theme")
  ("ts" bubbles-set-graphics-theme-squares "Squares" :column "Set theme")
  ("td" bubbles-set-graphics-theme-diamonds "Diamonds" :column "Set theme")
  ("dh" bubbles-set-game-hard "Hard" :column "Set difficulty")
  ("de" bubbles-set-game-easy "Easy" :column "Set difficulty")
  ("dm" bubbles-set-game-medium "Medium" :column "Set difficulty")
  ("dd" bubbles-set-game-difficult "Difficult" :column "Set difficulty")
  ("du" bubbles-set-game-userdefined "Userdefined" :column "Set difficulty")
  ("S" bubbles-save-settings "Save settings" :column "Misc"))

;;;###autoload
(defun +bubbles (&optional arg)
  (interactive "P")
  (if arg
      (setq +bubbles--old-wconf nil)
    (if (modulep! :ui workspaces)
        ;; delete current workspace if empty
        ;; this is useful when mu4e is in the daemon
        ;; as otherwise you can accumulate empty workspaces
        (progn
          (unless (+workspace-buffer-list)
            (+workspace-delete (+workspace-current-name)))
          (+workspace-switch +bubbles-workspace-name t))
      (setq +bubbles--old-wconf (current-window-configuration))
      (delete-other-windows)
      (switch-to-buffer (doom-fallback-buffer))))
  (call-interactively #'bubbles))

;;;###autoload
(defun +bubbles-quit ()
  (interactive)
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p +bubbles-workspace-name)
        (+workspace/delete +bubbles-workspace-name))
    (when +bubbles--old-wconf
      (set-window-configuration +bubbles--old-wconf)))
  (kill-buffer "*bubbles*"))
