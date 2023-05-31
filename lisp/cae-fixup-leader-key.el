;;; lisp/cae-fixup-leader-key.el -*- lexical-binding: t; -*-

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

;; I don't use Deft.
 (when (and (not (modulep! :ui deft))
            (eq (lookup-key doom-leader-map "nd")
                'deft))
   (define-key doom-leader-map "nd" nil))

(map! :leader
      :desc "help" "h" help-map)
