;;; lisp/cae-fixup-leader-key.el -*- lexical-binding: t; -*-

;; Do not override other keymaps with `general-override-mode'. This was created
;; because Doom's leader key was overriding Eat's `eat-self-input' keybinding.
(defvar cae-general-override--override-mode-p nil)
(after! general
  (define-minor-mode cae-general-override-mode
    "Minor mode to enable `general-override-mode-map' without
overriding other keymaps."
    :global t
    :init-value nil
    :lighter nil
    :keymap general-override-mode-map)
  (add-hook 'cae-general-override-mode-hook
            (cae-defun cae-general--unbind-keys ()
              ;; Do not override `org-edit-special' in Org mode.
              (define-key general-override-mode-map (kbd "C-c '") nil)))
  (add-hook 'cae-general-override-mode-hook
            (cae-defun cae-general-override--disable-override-mode ()
              (if cae-general-override-mode
                  (progn
                    (setq cae-general-override--override-mode-p
                          general-override-mode)
                    (general-override-mode -1))
                (general-override-mode
                 (if cae-general-override--override-mode-p 1 -1))
                (setq cae-general-override--override-mode-p nil))))
  (add-hook 'doom-after-init-hook #'cae-general-override-mode t))


;; Doom should not bind leader key prefixes to keys which are not alphanumeric
;; because then they can be overwriting other packages' keybindings. As an
;; example, Org mode has `C-c !' bound to `org-time-stamp-inactive' and `C-c &'
;; bound to `org-mark-ring-goto'.
(when (modulep! :checkers syntax)
  (after! which-key
    (setq which-key-replacement-alist
          (delete '(("\\`C-c !\\'") nil . "checkers")
                  which-key-replacement-alist)))
  (after! flycheck
    (define-key flycheck-mode-map flycheck-keymap-prefix nil)
    (setq flycheck-keymap-prefix (kbd "C-c C"))
    (define-key flycheck-mode-map flycheck-keymap-prefix
      flycheck-command-map)
    (map! :leader
          (:prefix ("C" . "checkers")))))
(when (modulep! :editor snippets)
  (dolist (p (cdr (lookup-key doom-leader-map "&")))
    (cl-destructuring-bind (key . binding) p
      (define-key doom-leader-map (kbd (concat "S " (char-to-string key))) binding)))
  (after! yasnippet (define-key yas-minor-mode-map (kbd "C-c &") nil))
  (define-key doom-leader-map "&" nil)
  (after! which-key
    (setq which-key-replacement-alist
          (let ((case-fold-search nil))
            (cl-mapcar (lambda (x)
                         (when (car-safe (car x))
                           (setf (car (car x))
                                 (replace-regexp-in-string "C-c &"
                                                           "C-c S"
                                                           (car-safe (car x)))))
                         x)
                       which-key-replacement-alist)))))
