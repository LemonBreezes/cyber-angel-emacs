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
              (and (minibuffer-prompt)
                   (cl-member (minibuffer-prompt)
                              '("I-search: "
                                "Query replace "
                                "Align regexp"
                                "Expansion for ")
                              :test #'string-match-p))
              (and (featurep 'helm-core)
                   (helm--alive-p))
              (corfu-mode +1))))

;;;###autoload
(defun cae-corfu-visible-p ()
  (or (and (frame-live-p corfu--frame)
           (frame-visible-p corfu--frame))
      (and (featurep 'corfu-terminal)
           (popon-live-p corfu-terminal--popon))))

;;;;;###autoload
;;(defun cae-cape-line-buffers ()
;;  (cl-loop for buf in (buffer-list)
;;           if (or (eq major-mode (buffer-local-value 'major-mode buf))
;;                  (< (buffer-size buf) (* 1 1024 1024)))
;;           collect buf))

;;;###autoload
(defun corfu-send-shell (&rest _)
  "Send completion candidate when inside comint/eshell."
  (cond
   ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
    (eshell-send-input))
   ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
    (comint-send-input))))

;;;###autoload
(defun cape-lsp ()
  (interactive)
  (cape-interactive #'lsp-completion-at-point))

;;;###autoload
(defun cae-yasnippet-capf ()
  (interactive)
  (if (or (not (char-before))
          (eq (char-syntax (char-before))
              ?\s))
      (call-interactively #'+default/insert-snippet)
    (call-interactively #'yasnippet-capf)))
