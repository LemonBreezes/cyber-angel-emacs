;;; private/dired/config.el -*- lexical-binding: t; -*-

;; Show Rsync progress in the modeline.
(after! dired-rsync
  (unless global-mode-string (push "" global-mode-string))
  (add-to-list 'global-mode-string 'dired-rsync-modeline-status 'append))

(after! dired
  ;; Use parallel versions of comression programs.
  ;; Install `pigz', pbzip2, `pixz', `'
  (setq dired-compress-file-alist
        '(("\\.gz\\'" . "pigz -9f %i")
          ("\\.bz2\\'" . "pbzip2 -9f %i")
          ("\\.xz\\'" . "pixz -9f %i")
          ("\\.zst\\'" . "zstd -qf -19 --rm -o %o %i"))
        dired-compress-files-alist
        '(("\\.tar\\.gz\\'" . "tar -cf - %i | pigz -c9 > %o")
          ("\\.tar\\.bz2\\'" . "tar -cf - %i | pbzip2 -c9 > %o")
          ("\\.tar\\.xz\\'" . "tar -cf - %i | pixz -c9 > %o")
          ("\\.tar\\.zst\\'" . "tar -cf - %i | zstd -19 -o %o")
          ("\\.tar\\.lz\\'" . "tar -cf - %i | plzip -c9 > %o")
          ("\\.tar\\.lzo\\'" . "tar -cf - %i | lzop -c9 > %o")
          ("\\.zip\\'" . "zip %o -r --filesync %i")
          ("\\.pax\\'" . "pax -wf %o %i"))
        dired-compress-file-suffixes
        '(("\\.tar\\.gz\\'" "" "pigz -dc %i | tar -xf -")
          ("\\.tar\\.xz\\'" "" "pixz -dc %i | tar -xf -")
          ("\\.tgz\\'" "" "pigz -dc %i | tar -xf -")
          ("\\.gz\\'" "" "pigz -d")
          ("\\.lz\\'" "" "plzip -d")
          ("\\.Z\\'" "" "uncompress")
          ("\\.z\\'" "" "pigz -d")
          ("\\.dz\\'" "" "dictunzip")
          ("\\.tbz\\'" ".tar" "pbunzip2")
          ("\\.bz2\\'" "" "pbunzip2")
          ("\\.xz\\'" "" "pixz -d")
          ("\\.zip\\'" "" "unzip -o -d %o %i")
          ("\\.tar\\.zst\\'" "" "unzstd -c %i | tar -xf -")
          ("\\.tzst\\'" "" "unzstd -c %i | tar -xf -")
          ("\\.zst\\'" "" "unzstd --rm")
          ("\\.7z\\'" "" "7z x -aoa -o%o %i")
          ("\\.tar\\'" ".tgz" nil))

        ;; From Dirvish's documentation.
        dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")

  (when (>= emacs-major-version 29)
    (setq dired-mouse-drag-files t
          mouse-drag-and-drop-region-cross-program t))
  (when (>= emacs-major-version 28)
    (setq dired-kill-when-opening-new-dired-buffer t))

  (map! :map dired-mode-map
        "C-M-k" #'dired-kill-subdir
        "K" #'dired-kill-subdir
        "I" #'dired-insert-subdir
        "_" #'dired-up-directory
        "[" #'dired-prev-dirline
        "]" #'dired-next-dirline
        "," #'dired-create-empty-file))

(after! dirvish
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
    (setopt dirvish-quick-access-entries
            '(("h" "~/" "Home")
              ("e" "~/.emacs.d/" "Emacs user directory")
              ("d" "~/Downloads/" "Downloads")
              ("m" "/mnt/" "Drives")
              ("s" "~/src/" "Source code")
              ("t" "~/.local/share/Trash/files/" "TrashCan"))))

  (after! dirvish-fd
    ;; On both of my computers, using "/" instead crashes Emacs.
    (setq dirvish-fd-default-dir "~/"))

  (setq global-hl-line-modes (delq 'dired-mode global-hl-line-modes))

  (after! wdired
    (setq wdired-allow-to-change-permissions t))

  (autoload 'vc-create-repo "vc" nil t)
  (map! :map dirvish-mode-map
        "e" #'cae-dired-find-file
        "<backtab>" #'dirvish-subtree-clear
        "M-l" #'dirvish-ls-switches-menu
        "M-j" #'dirvish-fd-jump
        "v" #'dirvish-vc-menu
        "h" #'dirvish-history-jump
        "M-n" nil                       ; for `avy-goto-line-below'
        "N" #'dirvish-narrow
        "j" #'consult-line)

  (defmacro cae-dired-find-file-wrapper (fn)
    "Wrap FN to exit Dirvish sessions when opening files."
    `(defun ,(intern (format "cae-dired-%s" (symbol-name fn))) ()
       (let ((dir default-directory))
         (advice-add #'find-file :around #'cae-dired-find-file-a)
         (unwind-protect (call-interactively #',fn)
           (advice-remove #'find-file #'cae-dired-find-file-a)))))

  (dolist (fn '(find-file
                projectile-find-file
                projectile-find-file-in-directory
                doom/find-file-in-other-project
                magit-find-file
                project-find-file
                doom/sudo-find-file
                find-file-other-tab
                find-file-read-only
                find-file-other-frame
                find-file-other-window
                doom/find-file-in-emacsd
                projectile-find-file-dwim
                projectile-find-file-dwim-other-frame
                projectile-find-file-dwim-other-window
                +default/find-file-under-here
                +default/find-in-notes
                +default/find-in-templates
                find-file-read-only-other-tab
                project-or-external-find-file
                find-file-read-only-other-frame
                doom/find-file-in-private-config
                find-file-read-only-other-window
                projectile-find-file-other-window
                projectile-find-file-other-frame
                projectile-find-file-in-known-projects
                projectile-find-other-file
                projectile-find-other-file-other-frame
                projectile-find-other-file-other-window
                magit-find-git-config-file
                magit-find-git-config-file-other-frame
                magit-find-git-config-file-other-window
                find-file-literally
                find-alternate-file
                ido-find-file
                consult-locate))
    (define-key dirvish-mode-map (vector 'remap fn)
      (cae-dired-find-file-wrapper fn)))

  (add-hook 'doom-switch-buffer-hook #'cae-dired-set-layout-h))

(add-to-list 'find-directory-functions #'cae-dired-load-dirvish-h t)
(setq find-directory-functions
      (delq 'dired-noselect find-directory-functions))
