;;; private/org/autoload/commands.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-org-rich-yank ()
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
                ('javascript 'javascript-mode) ('json 'js-json-mode)
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

;;;###autoload
(defun cae-org-insert-checkbox-or-bracket (arg)
  (interactive "p")
  (if (and (= arg 1)
           (ignore-errors
             (<= (point)
                 (save-excursion
                   (beginning-of-line)
                   (re-search-forward
                    (rx bol
                        (or (+ "*") "-")
                        (* whitespace)
                        (? "[" (group any) "]")
                        (* whitespace))
                    (pos-eol))
                   (point)))))
      (progn (if (match-string 1)
                 (delete-region (1- (match-beginning 1))
                                (progn (goto-char (1+ (match-end 1)))
                                       (skip-chars-forward "\s\t")
                                       (point)))
               (insert "[ ] "))
             (goto-char (pos-eol)))
    (org-self-insert-command arg)))


;;;###autoload
(defun cae-org-insert-file-link ()
  "Insert a file link.  At the prompt, enter the filename."
  (interactive)
  (insert (format "[[%s]]" (org-link-complete-file))))

;;;###autoload
(defun cae-org-syntax-convert-keyword-case-to-lower ()
  "Convert all #+KEYWORDS to #+keywords."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0)
          (case-fold-search nil))
      (while (re-search-forward "^[ \t]*#\\+[A-Z_]+" nil t)
        (unless (s-matches-p "RESULTS" (match-string 0))
          (replace-match (downcase (match-string 0)) t)
          (setq count (1+ count))))
      (message "Replaced %d occurances" count))))
