;;; autoload/cae-corfu.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-yas-setup-capf ()
  (make-variable-buffer-local 'completion-at-point-functions)
  (cl-pushnew 'cape-yasnippet
              completion-at-point-functions
              :test #'eq))

;;;###autoload
(defun cae-corfu-enable-in-minibuffer-h ()
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map)
              (and (featurep 'helm-core)
                   (helm--alive-p))
              (where-is-internal 'minibuffer-complete (list (current-local-map))))
    (corfu-mode +1)))

;;;###autoload
(defun cae-corfu-visible-p ()
  (or (and (frame-live-p corfu--frame)
           (frame-visible-p corfu--frame))
      (and (featurep 'corfu-terminal)
           (popon-live-p corfu-terminal--popon))))

;;;###autoload
(defun corfu-send-shell (&rest _)
  "Send completion candidate when inside comint/eshell."
  (cond
   ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
    (eshell-send-input))
   ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
    (comint-send-input))))

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
