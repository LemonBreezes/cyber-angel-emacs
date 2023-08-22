;;; autoload/cae-corfu.el -*- lexical-binding: t; -*-

;;;###autoload
(defmacro cae-orderless-escapable-split-fn (char)
  `(defun cae-orderless-escapable-split-on-space-or-char (s)
     (mapcar
      (lambda (piece)
        (thread-last piece
                     (replace-regexp-in-string
                      (concat (string 0) "\\|" (string 1))
                      (lambda (x)
                        (pcase x
                          ("\0" " ")
                          ("\1" ,(string char))
                          (_ x))))
                     (replace-regexp-in-string (string 1) ,(string char)
                                               'fixedcase 'literal)))
      (split-string (replace-regexp-in-string
                     "\\\\\\\\\\|\\\\ \\|\\\\&"
                     (lambda (x)
                       (pcase x
                         ("\\ " "\0")
                         (,(concat "\\" (string char)) "\1")
                         (_ x)))
                     s 'fixedcase 'literal)
                    (concat "[ " (string char) "]+") t))))

;;;###autoload
(defun cae-yas-setup-capf ()
  (make-variable-buffer-local 'completion-at-point-functions)
  (cl-pushnew 'cape-yasnippet
              completion-at-point-functions
              :test #'eq))

;;;###autoload
(defun cae-corfu-enable-in-minibuffer-h ()
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (and (minibuffer-prompt)
                   (cl-member (minibuffer-prompt)
                              '("I-search: "
                                "Query replace "
                                "Align regexp"
                                "Expansion for ")
                              :test #'string-match-p))
              (memq this-command '(evil-ex
                                   evil-ex-search-forward
                                   evil-ex-search-backward))
              (and (featurep 'helm-core)
                   (helm--alive-p))
              (corfu-mode +1))))

;;;###autoload
(defun cae-corfu-visible-p ()
  (or (and (frame-live-p corfu--frame)
           (frame-visible-p corfu--frame))
      (and (featurep 'corfu-terminal)
           (popon-live-p corfu-terminal--popon))))
