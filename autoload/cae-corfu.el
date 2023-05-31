;;; autoload/cae-corfu.el -*- lexical-binding: t; -*-

;;;###autoload
(defun orderless-escapable-split-on-space-or-ampersand (s)
  (mapcar
   (lambda (piece)
     (thread-last piece
                  (replace-regexp-in-string
                   (concat (string 0) "\\|" (string 1))
                   (lambda (x)
                     (pcase x
                       ("\0" " ")
                       ("\1" "&")
                       (_ x))))
                  (replace-regexp-in-string (string 1) "&")))
   (split-string (replace-regexp-in-string
                  "\\\\\\\\\\|\\\\ \\|\\\\&"
                  (lambda (x)
                    (pcase x
                      ("\\ " "\0")
                      ("\\&" "\1")
                      (_ x)))
                  s 'fixedcase 'literal)
                 "[ &]+" t)))

;;;###autoload
(defun yas-setup-capf ()
  (make-variable-buffer-local 'completion-at-point-functions)
  (cl-pushnew 'cape-yasnippet
              completion-at-point-functions
              :test #'eq))

;;;###autoload
(defun cae-corfu-enable-in-minibuffer-h ()
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (cl-member (minibuffer-prompt)
                         '("I-search: "
                           "Query replace "
                           "Align regexp"
                           "Expansion for ")
                         :test #'string-match-p)
              (memq this-command '(evil-ex
                                   evil-ex-search-forward
                                   evil-ex-search-backward))
              (and (featurep 'helm-core)
                   (helm--alive-p))
              (corfu-mode +1))))
