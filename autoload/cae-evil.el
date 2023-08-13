;;; autoload/cae-evil.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-forward-page (&optional count)
  (interactive "p")
  (if (> count 0)
      (when (and (eq (point) (progn (forward-page 1) (1- (point)))))
        (forward-page count))
    (forward-page count)))

;;;###autoload
(defun cae-show-normal-state-bindings ()
  (interactive)
  (require 'dash)
  (-some--> (symbol-value (intern (format "%S-map" major-mode)))
    (evil-get-auxiliary-keymap it 'normal)
    (which-key--show-keymap "Normal state bindings"
                            it
                            nil nil nil)))
