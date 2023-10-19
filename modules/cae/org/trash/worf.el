;;; private/org/trash/worf.el -*- lexical-binding: t; -*-

(use-package! worf
  :defer t :init
  (add-hook 'org-mode-hook #'worf-mode)
  :config
  (setq worf-recenter t)
  (define-key worf-mode-map (kbd "C-M-g") #'consult-org-heading)
  (keymap-set worf-mode-map "]" nil)
  (keymap-set worf-mode-map "[" nil)
  ;; I was getting an issue where these keys sometimes did not work but these
  ;; redundant keybindings seem to fix it.
  (define-key worf-mode-map (kbd "S-<iso-lefttab>") (lookup-key org-mode-map (kbd "<backtab>")))
  (define-key worf-mode-map (kbd "<tab>") (lookup-key org-mode-map (kbd "<tab>")))
  (defadvice! cae-worf-property-a ()
    :after #'worf-property
    ;; Jump infront of the property drawer
    (let ((parent (org-element-property :parent (org-element-at-point))))
      (when (eq 'property-drawer (car parent))
        (goto-char (org-element-property :begin parent)))))
  ;; Skip over vimish folds
  (defadvice! cae-worf-skip-vimish-fold-forward-a (oldfun arg)
    :around #'worf-down
    (let ((point-max (-some->> (and (require 'vimish-fold nil t)
                                    (overlays-at (point)))
                       (-filter #'vimish-fold--vimish-overlay-p)
                       (-map #'overlay-end)
                       (apply #'max)))
          (point (point)))
      (when point-max
        (setq arg (1+ arg)))
      (funcall oldfun arg)
      (when (<= (point) point-max)
        (goto-char point))))
  (advice-add #'worf-add :after #'cae-org-set-created-timestamp))
