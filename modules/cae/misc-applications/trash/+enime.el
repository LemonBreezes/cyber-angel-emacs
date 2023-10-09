;;; private/misc-applications/enime.el -*- lexical-binding: t; -*-

;; This package is broken.

(defvar +enime-workspace-name "*enime*")

(use-package! enime
  :commands enime-main-transient
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        "A" #'enime-main-transient)
  :config
  (let ((script
         (expand-file-name "video_scrapping.sh"
                           (file-name-directory (locate-library "enime")))))
    (chmod script #o700))
  (advice-add #'enime--start-mpv-playback :after #'+enime--start-mpv-playback-a))
