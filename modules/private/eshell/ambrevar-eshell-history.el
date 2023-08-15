;;; private/eshell/global-ambrevar-eshell-history.el -*- lexical-binding: t; -*-

;; Global Eshell history. From https://gitlab.com/ambrevar/dotfiles/-/blob/master/.emacs.d/lisp/init-eshell.el.
(defvar ambrevar/eshell-history-global-ring nil
  "The history ring shared across Eshell sessions.")

(defun ambrevar/eshell-hist-use-global-history ()
  "Make Eshell history shared across different sessions."
  (unless ambrevar/eshell-history-global-ring
    (when eshell-history-file-name
      (eshell-read-history nil t))
    (setq ambrevar/eshell-history-global-ring (or eshell-history-ring (make-ring eshell-history-size))))
  (setq eshell-history-ring ambrevar/eshell-history-global-ring))
(add-hook 'eshell-mode-hook 'ambrevar/eshell-hist-use-global-history)

(defun ambrevar/ring-delete-first-item-duplicates (ring)
  "Remove duplicates of last command in history.
Return RING.

This should be faster then `seq-uniq'.  Unlike
`eshell-hist-ignoredups' or `comint-input-ignoredups', it does
not allow duplicates ever.
Surrounding spaces are ignored when comparing."
  (let ((first (ring-ref ring 0))
        (index 1))
    (while (<= index (1- (ring-length ring)))
      (if (string= (string-trim first)
                   (string-trim (ring-ref ring index)))
          ;; REVIEW: We could stop at the first match, it would be faster and it
          ;; would eliminate duplicates if we started from a fresh history.
          ;; From an existing history that would not clean up existing
          ;; duplicates beyond the first one.
          (ring-remove ring index)
        (setq index (1+ index))))
    ring))

(defun ambrevar/eshell-history-remove-duplicates ()
  (ambrevar/ring-delete-first-item-duplicates eshell-history-ring))
(add-hook 'eshell-pre-command-hook 'ambrevar/eshell-history-remove-duplicates)

;; Always save history
(add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
