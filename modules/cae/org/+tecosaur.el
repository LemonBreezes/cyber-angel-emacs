;;; cae/org/+tecosaur.el -*- lexical-binding: t; -*-

(defun cae-locally-defer-font-lock ()
  "Set jit-lock defer and stealth parameters when buffer is large.
Especially useful for large Org files with complex structure."
  (when (> (buffer-size) 100000) ;; Increased threshold to 100KB
    (setq-local jit-lock-defer-time 0.1
                jit-lock-stealth-time 2
                jit-lock-stealth-load 200  ;; Process fewer chars during idle time
                jit-lock-chunk-size 10000  ;; Process in larger chunks
                jit-lock-stealth-nice 0.5))) ;; Lower CPU usage during stealth fontification

(add-hook 'org-mode-hook #'cae-locally-defer-font-lock)

(after! ox
  (add-to-list 'org-export-filter-final-output-functions #'cae-org-export-remove-zero-width-space t))
