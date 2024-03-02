;;; private/dired/trash/dirvish.el -*- lexical-binding: t; -*-

(when (modulep! :cae dirvish)
  (after! dirvish
    (advice-add #'find-file :around #'cae-dired-find-file-a)
    (advice-add #'find-file-other-window :around #'cae-dired-find-file-other-window-a)
    (advice-add #'consult--jump :around #'cae-dired-consult-jump-a)

    (after! dirvish-side
      (dirvish-side-follow-mode 1))
    (add-hook! 'dirvish-setup-hook
      (when (string-match-p "downloads"
                            (file-name-nondirectory
                             (directory-file-name default-directory)))
        (dirvish-emerge-mode 1)))
    (add-hook 'dirvish-emerge-mode-hook #'doom-auto-revert-buffer-h)
    (after! dirvish-emerge
      (setq dirvish-emerge-groups
            '(("Recent files" (predicate . recent-files-2h))
              ("Documents" (extensions "pdf" "tex" "bib" "epub"))
              ("Video" (extensions "mp4" "mkv" "webm"))
              ("Pictures" (extensions "jpg" "png" "svg" "gif" "jpeg" "avif"))
              ("Audio" (extensions "mp3" "flac" "wav" "ape" "aac"))
              ("Archives" (extensions "gz" "rar" "zip")))))

    (after! dirvish-widgets
      (setq dirvish-show-media-properties t))

    (after! dirvish-quick-access
      (setq! dirvish-quick-access-entries
             `(("h" "~/" "Home")
               ("e" ,doom-emacs-dir "Emacs user directory")
               ("d" "~/Downloads/" "Downloads")
               ("m" "/mnt/" "Drives")
               ("s" "~/src/" "Source code")
               ("t" "~/.local/share/Trash/files/" "TrashCan"))))


    (after! dirvish-fd
      (setq dirvish-fd-default-dir "~/"))

    (after! hl-line
      (setq global-hl-line-modes (delq 'dired-mode global-hl-line-modes)))

    (after! wdired
      (setq wdired-allow-to-change-permissions t))

    (autoload 'vc-create-repo "vc" nil t)
    (map! :map dirvish-mode-map
          :n "?" #'dirvish-dispatch
          :n "q" #'dirvish-quit
          :ng "a" #'dirvish-quick-access
          :ng "y" #'dirvish-yank-menu
          :ng "s" #'dirvish-quicksort
          :g "f" #'dirvish-file-info-menu
          :g "TAB" #'dirvish-subtree-toggle
          :g "M-t" #'dirvish-layout-toggle
          :g "M-b" #'dirvish-history-go-backward
          :g "M-f" #'dirvish-history-go-forward
          :g "M-n" #'dirvish-narrow
          :g "M-m" #'dirvish-mark-menu
          :ng "M-s" #'dirvish-setup-menu
          :g "M-e" #'dirvish-emerge-menu
          :g "M-r" #'dirvish-rsync-transient
          :g "M-c" #'dirvish-chxxx-menu
          :g "M-h" #'dirvish-history-menu
          :g "M-;" #'dirvish-epa-dired-menu
          :n "h" #'dired-up-directory
          :n "b" #'dirvish-history-jump ; Swapped with "h" to have hjkl
          :n "l" #'dired-find-file
          :m "e" (lookup-key dirvish-mode-map "e")
          :m "E" (lookup-key dirvish-mode-map "E") ; nil
          :m "w" (lookup-key dirvish-mode-map "w")
          :m "f" (lookup-key dirvish-mode-map "f")
          :m "F" (lookup-key dirvish-mode-map "F")
          :m "v" (lookup-key dirvish-mode-map "v")
          :m "V" (lookup-key dirvish-mode-map "V")
          :m "$" (lookup-key dirvish-mode-map "$")
          :m "0" (lookup-key dirvish-mode-map "0")
          :n "p" #'dirvish-history-last)

    (map! :map dirvish-mode-map
          :ng "e" #'find-file
          :ng "w" #'+default/search-buffer
          "<backtab>" #'dirvish-subtree-clear
          "M-l" #'dirvish-ls-switches-menu
          "M-j" #'dirvish-fd-jump
          "M-k" #'dirvish-fd
          "M-i" #'dirvish-fd-switches-menu
          [remap dirvish-emerge-menu] #'cae-dired-dirvish-emerge-menu
          "v" #'dirvish-vc-menu
          "h" #'dirvish-history-jump
          "j" #'+default/search-buffer))
  (map! :leader
        "o-" #'cae-dired-jump
        (:when (not (or (modulep! :ui neotree)
                        (modulep! :ui treemacs)))
         "op" #'dirvish-side))

  (add-hook! 'find-directory-functions :append
    (defun cae-dired-load-dirvish-h (dir)
      (remove-hook 'find-directory-functions #'cae-dired-load-dirvish-h)
      (require 'dirvish nil t)
      (unless (memq #'dired-noselect find-directory-functions)
        (add-hook 'find-directory-functions #'dired-noselect :append))
      (dired-noselect dir)))

  (setq find-directory-functions
        (delq 'dired-noselect find-directory-functions)))
