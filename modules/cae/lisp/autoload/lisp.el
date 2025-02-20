;;; cae/lisp/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-lisp-newline-and-indent ()
  (interactive)
  (cond ((and (minibufferp nil t)
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
        ((minibufferp nil t)
         (call-interactively #'exit-minibuffer))
        ((bound-and-true-p lispy-mode)
         (call-interactively #'lispy-newline-and-indent))
        (t
         (call-interactively #'newline-and-indent))))

;;;###autoload
(defun cae-lispy-which-key-cheatsheet () (interactive))
(after! lispy
  (hercules-def :show-funs #'cae-lispy-which-key-cheatsheet
                :hide-funs '(doom/escape evil-change-state)
                :keymap 'lispy-mode-map
                :transient t
                :flatten t))

;;;###autoload
(defun cae-insert-closing-paren ()
  "Inserts a closing paren if the sexps in the buffer are
unbalanced, otherwise acts like `self-insert-command'. Works with
Lispy."
  (interactive)
  (cond ((condition-case error
             (scan-sexps (point-min) (point-max))
           (scan-error t))
         (insert-char ?\)))
        ((bound-and-true-p lispy-mode)
         (call-interactively #'lispy-right-nostring))
        (t (call-interactively #'self-insert-command))))

;;;###autoload
(defun cae-delete-char ()
  "Like `delete-char', but works on the region if active, and
deletes the following char if the sexps in the buffer are
unbalanced. Works with Lispy and Smartparens."
  (interactive)
  (let ((delete-fn
         (cond ((condition-case _
                    (scan-sexps (point-min) (point-max))
                  (scan-error t))
                #'delete-char)
               ((bound-and-true-p lispy-mode)
                #'lispy-delete)
               ((bound-and-true-p smartparens-mode)
                (if (region-active-p)
                    #'sp-delete-region
                  #'sp-delete-char))
               (t #'delete-char))))
    (call-interactively delete-fn)))

;;;###autoload
(defun cae-edebug-compute-previous-result-a (_ &rest r)
  "Adviced `edebug-compute-previous-result'."
  (let ((previous-value (nth 0 r)))
    (if edebug-unwrap-results
        (setq previous-value
              (edebug-unwrap* previous-value)))
    (setq edebug-previous-result
          (edebug-safe-prin1-to-string previous-value))))

;;;###autoload
(defun cae-edebug-previous-result-a (_ &rest r)
  "Adviced `edebug-previous-result'."
  (eros--make-result-overlay edebug-previous-result :where (point)
                             :duration eros-eval-result-duration))

;;;###autoload
(defun cae-eval-last-sexp (arg)
  ;; Call `pp-eval-last-sexp' when called with a negative
  ;; prefix argument
  (interactive "P")
  (let ((debug-on-error t))
    (cond ((or (eq arg '-)
               (and (numberp arg)
                    (< arg 0)))
           (funcall #'pp-eval-last-sexp (if (numberp arg) nil)))
          ((bound-and-true-p eros-mode)
           (funcall #'eros-eval-last-sexp arg))
          (t (funcall #'eval-last-sexp arg)))))

;;;###autoload
(defun cae-eval-expression (arg)
  ;; Call `pp-eval-expression' when called with a negative
  ;; prefix argument
  (interactive "P")
  (cond ((or (eq arg '-)
             (and (numberp arg)
                  (< arg 0)))
         (setq current-prefix-arg (and (numberp arg)
                                       (- arg)))
         (call-interactively #'pp-eval-expression))
        (t (call-interactively #'eval-expression))))

;;https://emacsninja.com/posts/forbidden-emacs-lisp-knowledge-block-comments.html
;; This code is not complete but it's a reminder of how to use this.
;; I can also use #@00 to comment out the rest of a file.
;;;###autoload
(defun cae-comment-elisp-block (beg end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      ;; account for space and terminator
      (insert (format "#@%d " (+ (- end beg) 2)))
      (goto-char (point-max))
      (insert "\037"))))
