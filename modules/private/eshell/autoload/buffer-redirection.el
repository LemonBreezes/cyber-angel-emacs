;;; private/eshell/autoload/buffer-redirection.el -*- lexical-binding: t; -*-

;; From https://emacs.stackexchange.com/questions/42113/customize-eshell-redirection-to-buffer
;;;###autoload
(defun cae-eshell-syntax-buffer-redirect ()
  "Parse buffer redirection > #buf and >#."
  (when (and (not eshell-current-argument)
             (not eshell-current-quoted)
             ;; Don't overwrite `eshell-parse-special-reference'
             (not (looking-at "#<\\(\\(buffer\\|process\\)\\s-\\)?"))
             (looking-at "#\\(\\S-+\\)?"))
    (goto-char (match-end 0)) ;; Go to the end of the match.
    (list #'get-buffer-create
          (or
           (match-string 1)
           (format "*eshell export: %s*"
                   (replace-regexp-in-string
                    "\\s-*>+\\s-*#.*\\'" ""
                    (buffer-substring-no-properties (line-beginning-position) (point))))))))

;;; Colorize ansi escape sequences in exported buffers
;;;###autoload
(defun cae-eshell-ansi-buffer-output (fun object target)
  "Interpret ansi escape sequences when redirecting to buffer."
  (let* ((buf (and (markerp target) (marker-buffer target)))
         (str (and buf (stringp object) (string-match-p "\e\\[" object) object)))
    (funcall fun (if str str object) target)
    (when buf
      (with-current-buffer buf
        (goto-char (point-min))
        ;; For some reason applying this on the string and then inserting the
        ;; colorized string is not the same as colorizing the region.
        (ansi-color-apply-on-region (point-min) (point-max))
        (font-lock-mode)
        (pop-to-buffer buf)))))
