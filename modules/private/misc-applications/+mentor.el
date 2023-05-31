;;; private/misc-applications/mentor.el -*- lexical-binding: t; -*-

(defvar +mentor-workspace-name "*mentor*")

(use-package! mentor
  :commands mentor
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        "T" #'+mentor)
  :config
  (advice-add
   #'mentor :before
   (defun +mentor-a ()
     (setq! mentor-rtorrent-keep-session t
            mentor-rtorrent-external-rpc (expand-file-name "~/.rtorrent-rpc.socket"))
     (mentor-setup-rtorrent)
     ;; I got a void variable error for some reason.
     (defvar mentor-rtorrent-client-version)))

  (map! :map mentor-mode-map
        "q" #'+mentor-quit))
