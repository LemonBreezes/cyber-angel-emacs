;;; lisp/cae-dashboard.el -*- lexical-binding: t; -*-

(defun cae-dashboard-help ()
  (interactive)
  (if (and (featurep 'evil) (evil-normal-state-p))
      (cae-which-key-show-state-keymap)
    (which-key-show-keymap '+doom-dashboard-mode-map)))

(map! :map +doom-dashboard-mode-map
      :desc "Find file" :ng "f" #'find-file
      :desc "Recent files" :ng "r" #'consult-recent-file
      :desc "Reload last session" :ng "R" #'doom/quickload-session
      :desc "Config dir" :ng "C" #'doom/open-private-config
      :desc "Open Org agenda" :ng "a" #'org-agenda
      :desc "Open project" :ng "P" #'projectile-switch-project
      :desc "Open bookmark" :ng "m" #'bookmark-jump
      :desc "Open directory" :ng "d" #'consult-dir
      :desc "Open documentation":ng "D" #'doom/help
      :desc "Magit status" :ng "v" #'magit-status
      :desc "Open config.el/org" :ng "c" #'doom/goto-private-config-file
      :desc "Open init.el" :ng "I" #'doom/goto-private-init-file
      :desc "Open org-mode root" :ng "O" (cmd! (require 'org) (find-file org-directory))
      :desc "Open dotfile" :ng "." (cmd! (doom-project-find-file "~/.config/"))
      :desc "Notes (roam)" :ng "n" #'org-roam-node-find
      :desc "Switch buffer" :ng "b" #'persp-switch-to-buffer
      :desc "Switch buffers (all)" :ng "B" #'switch-to-buffer
      :desc "IBuffer" :ng "i" #'ibuffer
      :desc "Previous buffer" :ng "q" #'previous-buffer
      :desc "Set theme" :ng "t" #'consult-theme
      :desc "Quit" :ng "Q" #'save-buffers-kill-terminal
      :desc "Show keybindings" :ng "h" #'cae-dashboard-help
      :desc "Show keybindings" :ng "?" #'cae-dashboard-help
      :desc "Find file in .emacs.d" :ng "e" #'doom/find-file-in-emacsd
      :desc "Browse .emacs.d" :ng "E" #'doom/browse-in-emacsd)

(map! :leader :desc "Dashboard" "d" #'+doom-dashboard/open)

(defun +doom-dashboard-tweak (&optional _)
  (with-current-buffer (get-buffer +doom-dashboard-name)
    (setq-local line-spacing 0.2
                mode-line-format nil)))

(add-hook '+doom-dashboard-mode-hook #'+doom-dashboard-tweak)
