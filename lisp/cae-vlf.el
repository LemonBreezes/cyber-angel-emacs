;;; lisp/cae-vlf.el -*- lexical-binding: t; -*-

;; Copied pretty much verbatim from
;; https://github.com/tecosaur/emacs-config/blob/master/config.org
(use-package! vlf-setup
  :defer t :init
  (advice-add #'files--ask-user-about-large-file :override #'cae-files--ask-about-large-file-vlf)
  :config
  (advice-remove 'abort-if-file-too-large #'ad-Advice-abort-if-file-too-large)
  (defvar-local cae-vlf-cumulative-linenum '((0 . 0))
    "An alist keeping track of the cumulative line number.")

  (defun cae-vlf-update-linum ()
    "Update the line number offset."
    (let ((linenum-offset (alist-get vlf-start-pos +vlf-cumulative-linenum)))
      (setq display-line-numbers-offset (or linenum-offset 0))
      (when (and linenum-offset (not (assq vlf-end-pos +vlf-cumulative-linenum)))
        (push (cons vlf-end-pos (+ linenum-offset
                                   (count-lines (point-min) (point-max))))
              +vlf-cumulative-linenum))))

  (add-hook 'vlf-after-chunk-update-hook #'cae-vlf-update-linum)

  ;; Since this only works with absolute line numbers, let's make sure we use them.
  (setq-hook! 'vlf-mode-hook display-line-numbers t)

  (defun cae-vlf-next-chunk-or-start ()
    (if (= vlf-file-size vlf-end-pos)
        (vlf-jump-to-chunk 1)
      (vlf-next-batch 1))
    (goto-char (point-min)))

  (defun cae-vlf-last-chunk-or-end ()
    (if (= 0 vlf-start-pos)
        (vlf-end-of-file)
      (vlf-prev-batch 1))
    (goto-char (point-max)))

  (defun cae-vlf-isearch-wrap ()
    (if isearch-forward
        (cae-vlf-next-chunk-or-start)
      (cae-vlf-last-chunk-or-end)))

  (add-hook! 'vlf-mode-hook (setq-local isearch-wrap-function #'cae-vlf-isearch-wrap))

  (after! vlf
    (map-keymap (lambda (k v)
                  (when (commandp v)
                    (eval `(map! :map vlf-prefix-map
                                 :localleader
                                 ,(if (vectorp k)
                                      (key-description k)
                                    (key-description (vector k)))
                                 ',v)
                          t)))
                vlf-mode-map)))
