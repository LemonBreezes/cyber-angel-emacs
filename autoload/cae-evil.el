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
  (if-let ((map (or (evil-get-auxiliary-keymap
                     (cond (((bound-and-true-p git-timemachine-mode)
                             git-time-machine-mode-map))
                           (t (current-local-map)))
                     'normal))))
      (which-key--show-keymap "Normal state bindings" map nil nil nil)
    (message "No %s normal state bindings are defined." major-mode)))
