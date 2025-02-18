;;; cae/ai/doctor.el -*- lexical-binding: t; -*-

(unless (or (not (modulep! +copilot)) (executable-find "node"))
  (warn! "Couldn't find node executable. Copilot code completion is disabled."))

;; List of external compression/decompression programs used in the Dired config.
(defvar cae-dired-compression-programs
  '("pigz" "pbzip2" "pixz" "zstd" "rar"    ; compression programs used in dired-compress-file-alist
    "plzip" "lzop" "pax"                  ; additional compression tools in dired-compress-files-alist
    "uncompress" "dpkg-deb" "dictunzip" "pbunzip2" "unzip" "unzstd" "7z" "unrar") ; decompression commands
  "List of programs required for Dired compression features.")

(dolist (prog cae-dired-compression-programs)
  (unless (executable-find prog)
    (warn! "Dired Compression Warning: `%s` not found. Some compression/decompression features may be disabled." prog)))
