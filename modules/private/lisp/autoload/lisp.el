;;; private/lisp/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-lisp-check-parens-before-save-h ()
  (add-hook 'write-file-functions #'check-parens nil t))

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
(defun cae-lispy-cheatsheet () (interactive))
(hercules-def :toggle-funs #'cae-lispy-cheatsheet
              :keymap 'lispy-mode-map
              ;;:blacklist-funs '(special-digit-argument)
              :transient t
              :flatten t)
