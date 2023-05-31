;;; lisp/cae-kill-buffer.el -*- lexical-binding: t; -*-

;; My kill buffer improvements adapted from this stackoverflow answer:
;; https://emacs.stackexchange.com/questions/3245/kill-buffer-prompt-with-option-to-diff-the-changes

(defun cae-kill-buffer (orig-func &optional buffer-or-name)
  "Kill buffer with prompt to save if modified."
  (catch 'quit
    (save-window-excursion
      (with-current-buffer buffer-or-name
        (let (done (buf (current-buffer)))
          (when (and buffer-file-name (buffer-modified-p))
            (while (not done)
              (let ((response (read-char-choice
                               (format "Save file %s? (y, n, d, q) " (buffer-file-name buf))
                               '(?y ?n ?d ?q))))
                (setq done (cond
                            ((eq response ?q) (throw 'quit nil))
                            ((eq response ?y) (save-buffer) t)
                            ((eq response ?n) (set-buffer-modified-p nil) t)
                            ((eq response ?d) (diff-buffer-with-file) nil))))))
          (apply orig-func (list (current-buffer))))))))

(advice-add #'kill-buffer :around #'cae-kill-buffer)
