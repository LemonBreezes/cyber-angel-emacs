;;; private/org/autoload/commands.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org-rich-yank ()
  (interactive)
  (require 'org-rich-yank)
  (let* ((source-mode
          (or (and (buffer-live-p org-rich-yank--buffer)
                   (let ((mode (buffer-local-value 'major-mode org-rich-yank--buffer)))
                     (parent-mode-is-derived-p
                      (buffer-local-value 'major-mode org-rich-yank--buffer)
                      'prog-mode))
                   (buffer-local-value 'major-mode org-rich-yank--buffer))
              (pcase (language-detection-string (current-kill 0))
                ('ada 'ada-mode) ('c 'c-mode) ('cpp 'c++-mode)
                ('clojure 'clojure-mode) ('csharp 'csharp-mode)
                ('css 'css-mode) ('dart 'dart-mode)
                ('delphi 'delphi-mode) ('emacslisp 'emacs-lisp-mode)
                ('erlang 'erlang-mode) ('fortran 'fortran-mode)
                ('fsharp 'fsharp-mode) ('go 'go-mode)
                ('groovy 'groovy-mode) ('haskell 'haskell-mode)
                ('html 'html-mode) ('java 'java-mode)
                ('javascript 'javascript-mode) ('json 'json-mode)
                ('latex 'latex-mode) ('lisp 'lisp-mode)
                ('lua 'lua-mode) ('matlab 'matlab-mode)
                ('objc 'objc-mode) ('perl 'perl-mode)
                ('php 'php-mode) ('prolog 'prolog-mode)
                ('python 'python-mode) ('r 'r-mode)
                ('ruby 'ruby-mode) ('rust 'rust-mode)
                ('scala 'scala-mode) ('shell 'shell-script-mode)
                ('smalltalk 'smalltalk-mode) ('sql 'sql-mode)
                ('swift 'swift-mode) ('visualbasic 'visual-basic-mode)
                ('xml 'nxml-mode)
                (_ 'text-mode))))
         (paste
          (concat
           (format "#+begin_src %s\n"
                   (replace-regexp-in-string
                    "-mode$" ""
                    (symbol-name source-mode)))
           (thread-last (current-kill 0)
                        (org-rich-yank--trim-nl)
                        (funcall (lambda (s)
                                   (with-temp-buffer
                                     (insert s)
                                     (funcall source-mode)
                                     (indent-region (point-min) (point-max))
                                     (buffer-substring-no-properties (point-min) (point-max))))))
           (format "\n#+end_src\n"))))
    (insert paste)))
