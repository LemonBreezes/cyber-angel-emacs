;;; lisp/cae-kill-buffer.el -*- lexical-binding: t; -*-

;; My kill buffer improvements adapted from this stackoverflow answer:
;; https://emacs.stackexchange.com/questions/3245/kill-buffer-prompt-with-option-to-diff-the-changes

(defun cae-kill-buffer (&optional buffer-or-name)
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
          (kill-buffer buffer-or-name))))))

(defun cae-kill-buffer-fixup-windows (buffer)
  "Kill the BUFFER and ensure all the windows it was displayed in have switched
to a real buffer or the fallback buffer."
  (let ((windows (get-buffer-window-list buffer)))
    (cae-kill-buffer buffer)
    (doom-fixup-windows (cl-remove-if-not #'window-live-p windows))))

(advice-add #'doom-kill-buffer-fixup-windows :override #'cae-kill-buffer-fixup-windows)
