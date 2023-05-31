;;; lisp/cae-vlf.el -*- lexical-binding: t; -*-

;; Copied pretty much verbatim from
;; https://github.com/tecosaur/emacs-config/blob/master/config.org
(use-package! vlf-setup
  :commands vlf vlf-mode
  :init
  (defadvice! +files--ask-about-large-file-vlf (size op-type filename offer-raw)
    "Like `files--ask-user-about-large-file', but with support for `vlf'."
    :override #'files--ask-user-about-large-file
    (require 'vlf-setup)
    (if (eq vlf-application 'dont-ask)
        (progn (vlf filename) (error ""))
      (let ((prompt (format "File %s is large (%s), really %s?"
                            (file-name-nondirectory filename)
                            (funcall byte-count-to-string-function size) op-type)))
        (if (not offer-raw)
            (if (y-or-n-p prompt) nil 'abort)
          (let ((choice
                 (car
                  (read-multiple-choice
                   prompt '((?y "yes")
                            (?n "no")
                            (?l "literally")
                            (?v "vlf"))
                   (files--ask-user-about-large-file-help-text
                    op-type (funcall byte-count-to-string-function size))))))
            (cond ((eq choice ?y) nil)
                  ((eq choice ?l) 'raw)
                  ((eq choice ?v)
                   (vlf filename)
                   (error ""))
                  (t 'abort)))))))
  :config
  (advice-remove 'abort-if-file-too-large #'ad-Advice-abort-if-file-too-large)
  (defvar-local +vlf-cumulative-linenum '((0 . 0))
    "An alist keeping track of the cumulative line number.")

  (defun +vlf-update-linum ()
    "Update the line number offset."
    (let ((linenum-offset (alist-get vlf-start-pos +vlf-cumulative-linenum)))
      (setq display-line-numbers-offset (or linenum-offset 0))
      (when (and linenum-offset (not (assq vlf-end-pos +vlf-cumulative-linenum)))
        (push (cons vlf-end-pos (+ linenum-offset
                                   (count-lines (point-min) (point-max))))
              +vlf-cumulative-linenum))))

  (add-hook 'vlf-after-chunk-update-hook #'+vlf-update-linum)

  ;; Since this only works with absolute line numbers, let's make sure we use them.
  (add-hook! 'vlf-mode-hook (setq-local display-line-numbers t))

  (defun +vlf-next-chunk-or-start ()
    (if (= vlf-file-size vlf-end-pos)
        (vlf-jump-to-chunk 1)
      (vlf-next-batch 1))
    (goto-char (point-min)))

  (defun +vlf-last-chunk-or-end ()
    (if (= 0 vlf-start-pos)
        (vlf-end-of-file)
      (vlf-prev-batch 1))
    (goto-char (point-max)))

  (defun +vlf-isearch-wrap ()
    (if isearch-forward
        (+vlf-next-chunk-or-start)
      (+vlf-last-chunk-or-end)))

  (add-hook! 'vlf-mode-hook (setq-local isearch-wrap-function #'+vlf-isearch-wrap))

  (after! vlf
    (map-keymap (lambda (k v)
                  (when (commandp v)
                    (eval `(map! :map vlf-prefix-map
                                 :localleader
                                 ,(if (vectorp k)
                                      (key-description k)
                                    (key-description (vector k)))
                                 ',v))))
                vlf-mode-map)))
