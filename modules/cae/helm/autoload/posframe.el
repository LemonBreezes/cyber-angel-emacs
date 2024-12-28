;;; cae/helm/autoload/posframe.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-helm-display-function-based-on-frame-width (buffer &optional resume)
  "Display helm BUFFER using posframe if (frame-width) >= 120."
  (if (>= (frame-width) 120)
      (progn
        ;; Set up helm-posframe if not already done
        (unless (advice-member-p #'helm-posframe-cleanup #'helm-cleanup)
          ;; Add helm-posframe advice functions
          (advice-add #'helm-cleanup :around #'helm-posframe-cleanup)
          (advice-add #'helm-show-action-buffer :after #'helm-posframe--focus-minibuffer)
          (advice-add #'helm-display-mode-line :override #'ignore))
        ;; Use helm-posframe-display
        (helm-posframe-display buffer resume))
    (progn
      ;; Remove helm-posframe advice if present
      (when (advice-member-p #'helm-posframe-cleanup #'helm-cleanup)
        ;; Remove helm-posframe advice functions
        (advice-remove #'helm-cleanup #'helm-posframe-cleanup)
        (advice-remove #'helm-show-action-buffer #'helm-posframe--focus-minibuffer)
        (advice-remove #'helm-display-mode-line #'ignore))
      ;; Use default display function
      (let ((helm-full-frame t))
        (helm-default-display-buffer buffer)))))
