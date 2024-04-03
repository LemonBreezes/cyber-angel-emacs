;;; private/dired/config.el -*- lexical-binding: t; -*-

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

  (map! :map dired-mode-map
        "C-M-k" #'dired-kill-subdir
        "K" #'dired-kill-subdir
        "I" #'dired-insert-subdir
        [remap dired-do-man] #'woman-dired-find-file
        :ng "_" #'dired-up-directory
        "[" #'dired-prev-dirline
        "]" #'dired-next-dirline
        "," #'dired-create-empty-file))
