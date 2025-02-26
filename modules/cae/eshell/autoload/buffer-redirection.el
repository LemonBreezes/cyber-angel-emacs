;;; cae/eshell/autoload/buffer-redirection.el -*- lexical-binding: t; -*-

;;;; Buffer Redirection Utilities for Eshell

;;;###autoload
(defun cae-eshell-syntax-buffer-redirect ()
  "Parse buffer redirection syntax for Eshell.
Handles two forms of buffer redirection:
1. >#buffer-name - Redirect to a specific buffer
2. ># - Redirect to an auto-named buffer based on the command

Originally from: https://emacs.stackexchange.com/questions/42113"
  (when (and (not eshell-current-argument)
             (not eshell-current-quoted)
             ;; Don't overwrite `eshell-parse-special-reference'
             (not (looking-at "#<\\(\\(buffer\\|process\\)\\s-\\)?"))
             (looking-at "#\\(\\S-+\\)?"))
    (goto-char (match-end 0)) ;; Go to the end of the match.
    (list #'get-buffer-create
          (or
           ;; Case 1: Named buffer (#buffer-name)
           (match-string 1)
           ;; Case 2: Auto-named buffer (#)
           (format "*eshell export: %s*"
                   (replace-regexp-in-string
                    "\\s-*>+\\s-*#.*\\'" ""
                    (buffer-substring-no-properties (line-beginning-position) (point))))))))

;;;###autoload
(defun cae-eshell-ansi-buffer-output (fun object target)
  "Interpret ANSI escape sequences when redirecting to buffer.
Wraps around the original output function FUN, processing OBJECT
before sending it to TARGET. When TARGET is a buffer, ANSI color
sequences in OBJECT are interpreted to provide colorized output.

Arguments:
  FUN: The original output function
  OBJECT: The content to output
  TARGET: The destination (buffer marker)"
  (let* ((buf (and (markerp target) (marker-buffer target)))
         (str (and buf (stringp object) (string-match-p "\e\\[" object) object)))
    ;; Call the original function
    (funcall fun (if str str object) target)
    
    ;; Process ANSI sequences if we're redirecting to a buffer
    (when buf
      (with-current-buffer buf
        (goto-char (point-min))
        ;; Apply ANSI color interpretation to the entire buffer
        (ansi-color-apply-on-region (point-min) (point-max))
        (font-lock-mode)
        (pop-to-buffer buf)))))
