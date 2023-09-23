;;; lisp/cae-dashboard.el -*- lexical-binding: t; -*-

(map! :map +doom-dashboard-mode-map
      :desc "Find file" :ng "f" #'find-file
      :desc "Recent files" :ng "r" #'consult-recent-file
      :desc "Reload last session" :ng "R" #'doom/quickload-session
      :desc "Config dir" :ng "C" #'doom/open-private-config
      :desc "Open Org agenda" :ng "a" #'org-agenda
      :desc "Open project" :ng "P" #'projectile-switch-project
      :desc "Open bookmarks" :ng "B" #'bookmark-jump
      :desc "Open config.org" :ng "c" (cmd! (find-file (expand-file-name "config.org" doom-user-dir)))
      :desc "Open org-mode root" :ng "O" (cmd! (find-file (expand-file-name "lisp/org/" doom-user-dir)))
      :desc "Open dotfile" :ng "." (cmd! (doom-project-find-file "~/.config/"))
      :desc "Notes (roam)" :ng "n" #'org-roam-node-find
      :desc "Switch buffer" :ng "b" #'+vertico/switch-workspace-buffer
      :desc "Switch buffers (all)" :ng "B" #'switch-to-buffer
      :desc "IBuffer" :ng "i" #'ibuffer
      :desc "Previous buffer" :ng "p" #'previous-buffer
      :desc "Set theme" :ng "t" #'consult-theme
      :desc "Quit" :ng "Q" #'save-buffers-kill-terminal
      :desc "Show keybindings" :ng "h" (cmd!
                                        (if (and (featurep 'evil) (evil-normal-state-p))
                                            (cae-show-normal-state-bindings)
                                          (which-key-show-keymap '+doom-dashboard-mode-map))))

(map! :leader :desc "Dashboard" "d" #'+doom-dashboard/open)

(defun +doom-dashboard-tweak (&optional _)
  (with-current-buffer (get-buffer +doom-dashboard-name)
    (setq-local line-spacing 0.2
                mode-line-format nil
                evil-normal-state-cursor (list nil))
    (hl-line-mode +1)))

(add-hook '+doom-dashboard-mode-hook #'+doom-dashboard-tweak)
