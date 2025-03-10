;;; cae/dired/doctor.el -*- lexical-binding: t; -*-

;; List of external compression/decompression programs used in the Dired config.
(defvar cae-dired-compression-programs
  '("pigz" "pbzip2" "pixz" "zstd" "rar"    ; compression programs used in dired-compress-file-alist
    "plzip" "lzop" "pax"                  ; additional compression tools in dired-compress-files-alist
    "uncompress" "dpkg-deb" "dictunzip" "pbunzip2" "unzip" "unzstd" "7z") ; decompression commands
  "List of programs required for Dired compression features.")

(dolist (prog cae-dired-compression-programs)
  (unless (executable-find prog)
    (warn! "Couldn't find `%s`. Some compression/decompression features may be disabled." prog)))
