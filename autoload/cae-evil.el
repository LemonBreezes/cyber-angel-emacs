;;; autoload/cae-evil.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-forward-page (&optional count)
  (interactive "p")
  (if (> count 0)
      (when (and (eq (point) (progn (forward-page 1) (1- (point)))))
        (forward-page count))
    (forward-page count)))

;;;###autoload
;; show the current evil normal state map for this major mode
;; use annalist
(defun cae-evil-show-normal-state-map ()
  (interactive)
  (when-let ((buf (get-buffer-create "*evil normal state map*"))
             (map (symbol-value (intern (concat (symbol-name major-mode) "-map")))))
    (with-current-buffer buf
      (erase-buffer)
      (evil-normal-state)
      (evil-normalize-keymaps)
      (pp (evil-get-auxiliary-keymap map 'normal)
          buf)
      (pop-to-buffer buf))))
