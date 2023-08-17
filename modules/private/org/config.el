;;; private/org/config.el -*- lexical-binding: t; -*-

(use-package! org-rich-yank
  :defer t
  :init
  (map! :map org-mode-map
        "C-M-y" #'cae-org-rich-yank))

(defvar +org-exit-src-code-hook nil
  "Hook run just before exiting a org source block buffer.")
(advice-add #'org-edit-src-exit :before #'cae-org-run-exit-src-code-hooks)
(add-hook '+org-exit-src-code-hook #'ws-butler-trim-eob-lines)

(advice-add #'org-insert-heading :after #'cae-org-set-created-timestamp)
(add-hook 'org-capture-mode-hook
  (cae-defun org-capture--insert-timestamp ()
    (when (org-at-heading-p)
      (cae-org-set-created-timestamp))))

(advice-add #'+org-init-keybinds-h :after
            (cae-defun cae-org-fixup-doom-keybindings ()
              (remove-hook 'org-tab-first-hook #'+org-indent-maybe-h)))

(use-package! org-appear
  :defer t :init
  (add-hook 'org-mode-hook #'org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

(after! org
  (map! :map org-mode-map
        "]" #'cae-org-insert-checkbox-or-bracket))

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
  (advice-add #'worf-property
              :after
              (cae-defun cae-worf-property-a ()
                ;; Jump infront of the property drawer
                (let ((parent (org-element-property :parent (org-element-at-point))))
                  (when (eq 'property-drawer (car parent))
                    (goto-char (org-element-property :begin parent))))))
  (advice-add #'worf-down
              :around
              ;; Skip over vimish folds
              (cae-defun cae-worf-skip-vimish-fold-forward-a (oldfun arg)
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
                    (goto-char point)))))
  (advice-add #'worf-add :after #'cae-org-set-created-timestamp))

;; Giving some error
;;(use-package! org-tidy
;;  :defer t :init
;;  (add-hook 'org-mode-hook #'org-tidy-mode)
;;  :config
;;  (setq org-tidy-properties-inline-symbol "·"))

(defun locally-defer-font-lock ()
  "Set jit-lock defer and stealth, when buffer is over a certain size."
  (when (> (buffer-size) 50000)
    (setq-local jit-lock-defer-time 0.05
                jit-lock-stealth-time 1)))

(add-hook 'org-mode-hook #'locally-defer-font-lock)

;; An `org-indent' hack from Tecosaur's Emacs config. Refer there for
;; documentation.
(defadvice! +org-indent--reduced-text-prefixes ()
  :after #'org-indent--compute-prefixes
  (setq org-indent--text-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (when (> org-indent-indentation-per-level 0)
    (dotimes (n org-indent--deepest-level)
      (aset org-indent--text-line-prefixes
            n
            (org-add-props
                (concat (make-string (* n (1- org-indent-indentation-per-level))
                                     ?\s)
                        (if (> n 0)
                            (char-to-string org-indent-boundary-char)
                          "\u200b"))
                nil 'face 'org-indent)))))
