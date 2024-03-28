;;; autoload/cae-corfu.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-corfu-visible-p ()
  (or (and (frame-live-p corfu--frame)
           (frame-visible-p corfu--frame))
      (and (featurep 'corfu-terminal)
           (popon-live-p corfu-terminal--popon))))

;;;###autoload
(defun cae-cape-lsp ()
  (interactive)
  (cond ((and (modulep! :tools lsp)
              (not (modulep! :tools lsp +eglot)))
         (cape-interactive #'lsp-completion-at-point))
        (t (cape-interactive #'eglot-completion-at-point))))

;;;###autoload
(defun cae-yasnippet-capf ()
  (interactive)
  (if (or (not (char-before))
          (eq (char-syntax (char-before))
              ?\s))
      (call-interactively #'+default/insert-snippet)
    (call-interactively #'yasnippet-capf)))

;;;###autoload
(defun +corfu-insert-wildcard-separator ()
  ;; I had to rename this command so that it doesn't start with "corfu-".
  ;; Otherwise, it does not insert the completion when +tng is enabled.
  (interactive)
  (setq this-command #'corfu-insert-separator)
  (call-interactively #'corfu-insert-separator))

;;;###autoload
(defun cae-cape-history-or-line ()
  (interactive)
  (let ((capf (cape-capf-super #'cape-history #'cape-line)))
    (cape-interactive capf)))

;;;###autoload
(defun cae-cape-keyword-or-dict ()
  (interactive)
  (let ((capf (cape-capf-super #'cape-keyword #'cape-dict)))
    (cape-interactive capf)))

;;;###autoload
(defun cae-corfu-popup-and-first ()
  "Trigger corfu popup and select the first candidate."
  ;; HACK using `corfu--auto-complete-deferred' to trigger the completion popup
  ;; without explicitly inserting any candidate
  (interactive)
  (let ((corfu-auto-prefix 1))
    (corfu--auto-complete-deferred))
  (when (> corfu--total 0)
    (corfu--goto 0)))

;;;###autoload
(defun cae-corfu-popup-and-last ()
  "Trigger corfu popup and select the last candidate."
  ;; HACK using `corfu--auto-complete-deferred' to trigger the completion popup
  ;; without explicitly inserting any candidate
  (interactive)
  (corfu--auto-complete-deferred)
  (when (> corfu--total 0)
    (corfu-last)))

;;;###autoload
(defun cae-corfu-complete-in-minibuffer ()
  (interactive)
  (let  ((completion-in-region-function #'consult-completion-in-region))
    (call-interactively #'complete-symbol)))
