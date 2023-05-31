;;; private/lisp/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-lisp-check-parens-before-save-h ()
  (add-hook 'write-file-functions 'check-parens nil t))

;;;###autoload
(defun cae-lispy-newline-and-indent-plain ()
  (interactive)
  (if (and (minibufferp)
           (string= (minibuffer-prompt) "Eval: ")
           ;; Insert a newline if either the sexp is unbalanced or we are in a
           ;; commented/empty line.
           (or (comment-only-p (line-beginning-position) (line-end-position))
               (condition-case error
                   (scan-sexps (point-min) (point-max))
                 (scan-error t))))
      (progn
        (insert-char ?\n 1)
        (indent-according-to-mode))
    (call-interactively 'lispy-newline-and-indent)))
