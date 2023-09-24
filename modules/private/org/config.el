;;; private/org/config.el -*- lexical-binding: t; -*-

(use-package! org-rich-yank
  :defer t :init
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

;;(advice-add #'+org-init-keybinds-h :after
;;            (cae-defun cae-org-fixup-doom-keybindings ()
;;              (remove-hook 'org-tab-first-hook #'+org-indent-maybe-h)))

;; This is giving me an error.
(remove-hook 'org-mode-hook #'+org-make-last-point-visible-h)

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
        "]" #'cae-org-insert-checkbox-or-bracket
        "C-c C-v" #'cae-org-babel-cheatsheet))

(defun locally-defer-font-lock ()
  "Set jit-lock defer and stealth, when buffer is over a certain size."
  (when (> (buffer-size) 500000)
    (setq-local jit-lock-defer-time 0.05
                jit-lock-stealth-time 1)))

(add-hook 'org-mode-hook #'locally-defer-font-lock)

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


(use-package! org-ol-tree
  :commands org-ol-tree
  :init
  (after! org
    (map! :map org-mode-map
          :localleader
          :desc "Outline" "O" #'org-ol-tree))
  :config
  (setq org-ol-tree-ui-icon-set
        (if (and (display-graphic-p)
                 (fboundp 'all-the-icons-material))
            'all-the-icons
          'unicode))
  (org-ol-tree-ui--update-icon-set))

(after! org
  (map! :map org-mode-map
        (:when (modulep! :editor evil)
         :nie "M-SPC M-SPC" (cmd! (insert "\u200B")))
        :localleader
        "l f" #'cae-org-insert-file-link))

(after! ox
  (add-to-list 'org-export-filter-final-output-functions #'cae-org-export-remove-zero-width-space t))
