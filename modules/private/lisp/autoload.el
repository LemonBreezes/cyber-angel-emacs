;;; private/lisp/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-lisp-check-parens-before-save-h ()
  (add-hook 'write-file-functions 'check-parens nil t))

;;;###autoload
(defun cae-lisp-newline-and-indent ()
  (interactive)
  (cond ((and (minibufferp)
              (string= (minibuffer-prompt) "Eval: ")
              ;; Insert a newline if either the sexp is unbalanced or we are in a
              ;; commented/empty line.
              (or (comment-only-p (line-beginning-position) (line-end-position))
                  (condition-case error
                      (scan-sexps (point-min) (point-max))
                    (scan-error t))))
         (progn
           (insert-char ?\n 1)
           (indent-according-to-mode)))
        ((minibufferp)
         (call-interactively #'exit-minibuffer))
        ((bound-and-true-p lispy-mode)
         (call-interactively #'lispy-newline-and-indent))
        (t
         (call-interactively #'newline-and-indent))))
