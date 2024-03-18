;;; private/lisp/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-lisp-check-parens-before-save-h ()
  (add-hook 'write-file-functions #'check-parens nil t))

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
(defun cae-emacs-lisp-extend-imenu-h ()
  "Improve imenu support in `emacs-lisp-mode' for Doom's APIs."
  (setq imenu-generic-expression
        `(("Section" "^[ \t]*;;\\(?:;+\\|\\*+\\)[ \t]+\\([^\n]+\\)" 1)
          ("Evil commands" "^\\s-*(evil-define-\\(?:command\\|operator\\|motion\\) +\\(\\_<[^ ()\n]+\\_>\\)" 1)
          ("Unit tests" "^\\s-*(\\(?:ert-deftest\\|describe\\) +\"\\([^\")]+\\)\"" 1)
          ("Package" "^\\s-*\\(?:;;;###package\\|(\\(?:package!\\|use-package!?\\|after!\\)\\) +\\(\\_<[^ ()\n]+\\_>\\)" 1)
          ("Major modes" "^\\s-*(define-derived-mode +\\([^ ()\n]+\\)" 1)
          ("Minor modes" "^\\s-*(define-\\(?:global\\(?:ized\\)?-minor\\|generic\\|minor\\)-mode +\\([^ ()\n]+\\)" 1)
          ("Modelines" "^\\s-*(def-modeline! +\\([^ ()\n]+\\)" 1)
          ("Modeline segments" "^\\s-*(def-modeline-segment! +\\([^ ()\n]+\\)" 1)
          ("Advice" "^\\s-*(\\(?:def\\(?:\\(?:ine-\\)?advice!?\\)\\) +\\([^ )\n]+\\)" 1)
          ("Macros" "^\\s-*(\\(?:cl-\\)?def\\(?:ine-compile-macro\\|macro\\) +\\([^ )\n]+\\)" 1)
          ("Inline functions" "\\s-*(\\(?:cl-\\)?defsubst +\\([^ )\n]+\\)" 1)
          ("CLI Command" "^\\s-*(\\(def\\(?:cli\\|alias\\|obsolete\\|autoload\\)! +\\([^\n]+\\)\\)" 1)
          ("Functions" "^\\s-*(\\(?:cl-\\)?def\\(?:un\\|un\\*\\|method\\|generic\\|-memoized!\\) +\\([^ ,)\n]+\\)" 1)
          ("Variables" "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\|var\\(?:-local\\)?\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
          ("Types" "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|error\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2))))

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
