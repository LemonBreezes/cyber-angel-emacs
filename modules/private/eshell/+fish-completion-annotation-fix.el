;;; private/eshell/+fish-completion-fix.el -*- lexical-binding: t; -*-

;; (after! fish-completion
;;   (defun +fish-completion--list-completions-a (raw-prompt)
;;     (mapcar (lambda (e)
;;               (string-match "\\`\\([^\t]*\\)\t?\\(.*\\)\\'" e)
;;               (propertize (match-string 1 e) 'fish-completion--annotation (match-string 2 e)))
;;             (split-string
;;              (fish-completion--list-completions-with-desc raw-prompt)
;;              "\n" t)))
;;   (advice-add #'fish-completion--list-completions :override #'+fish-completion--list-completions-a)

;;   (defun fish-completion--annotate (cand)
;;     (when-let* ((pos (or (next-single-property-change 0 'fish-completion--annotation cand)
;;                          0))
;;                 (ann (get-text-property pos 'fish-completion--annotation cand)))
;;       (concat (propertize " " 'display '(space :align-to center)) ann)))

;;   (defun fish-completion--provide-annotation-function (table)
;;     (nconc table (list :annotation-function #'fish-completion--annotate)))

;;   (advice-add #'pcomplete-completions-at-point :filter-return #'fish-completion--provide-annotation-function))

(when (modulep! :completion vertico)
  ;; Code is mostly from  https://github.com/minad/marginalia/issues/87
  ;; But we implement a capf because getting annotations from fish is
  ;; difficult if we stick with pcomplete. The capf is non-exclusive
  ;; so fallback to pcomplete machinery happens if there are no candidates.
  (defun +eshell-fish-completion-list (raw-prompt)
    "Return list of completion candidates for RAW-PROMPT."
    (mapcar (lambda (e) (let ((res (split-string e "\t")))
                     (propertize (car res) 'fish-annotation (cadr res))))
            (split-string
             (fish-completion--list-completions-with-desc raw-prompt)
             "\n" t)))

  (defun +eshell-fish-capf ()
    "A a capf for fish-completion."
    (when-let (((not (file-remote-p default-directory)))
               (args (ignore-errors (eshell-complete-parse-arguments)))
               (table (+eshell-fish-completion-list
                       (buffer-substring (or (cadr args) (point)) (point))))
               ((not (file-exists-p (car table)))))
      (list (car (last args)) (point) table
            :exclusive 'no
            :annotation-function #'+eshell-fish-completion-annotate
            :exit-function (lambda (&rest _) (insert " ")))))

  (defun +eshell-fish-completion-annotate (cand)
    (when-let* ((ann (get-text-property 0 'fish-annotation cand)))
      (concat (propertize " " 'display '(space :align-to center)) ann)))

  (defun +eshell-use-annotated-completions-h ()
    "Use annotaed fish completions."
    (if fish-completion-mode
        (add-hook 'completion-at-point-functions #'+eshell-fish-capf nil t)
      (remove-hook 'completion-at-point-functions #'+eshell-fish-capf t)))

  (add-hook 'fish-completion-mode-hook #'+eshell-use-annotated-completions-h))

(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode)
  :config
  (defadvice! +eshell-filter-history-from-highlighting-a (&rest _)
    "Selectively inhibit `eshell-syntax-highlighting-mode'.
So that matches from history show up with highlighting."
    :before-until #'eshell-syntax-highlighting--enable-highlighting
    (memq this-command '(eshell-previous-matching-input-from-input
                         eshell-next-matching-input-from-input)))

  (defun +eshell-syntax-highlight-maybe-h ()
    "Hook added to `pre-command-hook' to restore syntax highlighting
when inhibited to show history matches."
    (when (and eshell-syntax-highlighting-mode
               (memq last-command '(eshell-previous-matching-input-from-input
                                    eshell-next-matching-input-from-input)))
      (eshell-syntax-highlighting--enable-highlighting)))

  (defun +eshell-syntax-highlighting-mode-h ()
    "Hook to enable `+eshell-syntax-highlight-maybe-h'."
    (if eshell-syntax-highlighting-mode
        (add-hook 'pre-command-hook #'+eshell-syntax-highlight-maybe-h nil t)
      (remove-hook 'pre-command-hook #'+eshell-syntax-highlight-maybe-h t)))

  (add-hook 'eshell-syntax-highlighting-mode-hook #'+eshell-syntax-highlighting-mode-h))
