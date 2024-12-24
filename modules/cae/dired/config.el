;;; private/dired/config.el -*- lexical-binding: t; -*-

(require 'cae-lib)

;; Show Rsync progress in the modeline.
(after! dired-rsync
  (unless global-mode-string (push "" global-mode-string))
  (add-to-list 'global-mode-string 'dired-rsync-modeline-status 'append))

(after! dired
  ;; TODO Investigate `p7zip' for `rar'.
  ;; Use parallel versions of comression programs.
  ;; Install `pigz', `pbzip2', `pixz', `plzip', and `lzop' for parallel decompression.
  ;; Install `dpkg', `rar', and `unrar' as well.
  ;; `pax' and `7z' I don't install unless I need them.
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
          ("\\.zip\\'" . "pigz --zip -f9 %o -r --filesync %i")
          ("\\.pax\\'" . "pax -wf %o %i"))
        dired-compress-file-suffixes
        '(("\\.tar\\.gz\\'" "" "pigz -dc %i | tar -xf -")
          ("\\.tar\\.xz\\'" "" "pixz -dc %i | tar -xf -")
          ("\\.tgz\\'" "" "pigz -dc %i | tar -xf -")
          ("\\.gz\\'" "" "pigz -d")
          ("\\.lz\\'" "" "plzip -d")
          ("\\.Z\\'" "" "uncompress")
          ("\\.deb\\'" "" "dpkg-deb -x %i %o")
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

  (setq dired-mouse-drag-files t
        dired-movement-style 'cycle)
  (after! dired-aux
    (setq dired-isearch-filenames 'dwim))

  ;; These are from before the Dired module was revamped and for non-Evil.
  ;;(map! :map dired-mode-map
  ;;      "C-M-k" #'dired-kill-subdir
  ;;      "K" #'dired-kill-subdir
  ;;      "I" #'dired-insert-subdir
  ;;      [remap dired-do-man] #'woman-dired-find-file
  ;;      :ng "_" #'dired-up-directory
  ;;      "[" #'dired-prev-dirline
  ;;      "]" #'dired-next-dirline
  ;;      "," #'dired-create-empty-file)

  (map! :map dired-mode-map
        "C-M-k" #'dired-kill-subdir))

(after! wdired
  (setq wdired-allow-to-change-permissions t))

(after! dirvish-widgets
  (setq dirvish-show-media-properties t))

(after! dirvish-quick-access
  (setq! dirvish-quick-access-entries
         `(("h" "~/" "Home")
           ("e" ,doom-emacs-dir "Emacs user directory")
           ("d" "~/Downloads/" "Downloads")
           ,(list "m" (format "/run/media/%s/" (user-login-name)) "Drives")
           ("s" "~/src/" "Source code")
           ("t" "~/.local/share/Trash/files/" "TrashCan"))))

(after! dirvish-side
  (dirvish-side-follow-mode 1))

(add-hook! 'dirvish-setup-hook
  (when (string-match-p "Downloads"
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

(after! dirvish-fd
  (setq dirvish-fd-default-dir "~/"))

(after! dirvish
  (setq dirvish-hide-cursor nil)
  ;; I think the header line is harder to read in terminal Emacs. Also, it
  ;; conflicts with the new `dired-movement-style' equal to `cycle' and with
  ;; `beginend-dired-mode', as they expect the files to begin on the second line
  ;; of the buffer.
  (setq dirvish-use-header-line nil))

;;(map! [remap dired-jump] #'cae-dired-jump) ; I prefer manually activating the
                                        ;fullscreen layout.
;;(advice-add #'find-file :around #'cae-dired-find-file-a)
;;(advice-add #'find-file-other-window :around #'cae-dired-find-file-other-window-a)
;;(advice-add #'consult--jump :around #'cae-dired-consult-jump-a)
