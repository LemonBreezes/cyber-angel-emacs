;;; trash/cae-avy.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-embark-act ()
  (interactive)
  (require 'embark)
  (let ((embark-cycle-key (key-description (this-command-keys))))
    (call-interactively 'embark-act)))

;;;###autoload
(defun cae-avy-do (action pt)
  (save-mark-and-excursion
    (goto-char pt)
    (cond ((or (eq avy-command 'avy-goto-line)
               (memq this-command '(avy-goto-line-above
                                    avy-goto-line-below)))
           (progn (goto-char (line-beginning-position))
                  (set-mark (point))
                  (goto-char (line-end-position))))
          ((eq this-command 'cae-avy-symbol-at-point)
           (er/mark-symbol))
          (t (if (fboundp 'eri/expand-region)
                 (eri/expand-region 1)
               (er/expand-region 1))))
    (call-interactively action))
  (run-at-time 0.0 nil
               (lambda ()
                 (cae-pop-mark)
                 (when (bound-and-true-p hl-line-mode)
                   (hl-line-highlight))
                 (when (bound-and-true-p beacon-mode)
                   (beacon-blink)))))

;;;###autoload
(defalias 'cae-avy-action-embark-act
  (apply-partially #'cae-avy-do #'embark-act))

;;;###autoload
(defalias 'cae-avy-action-kill
  (apply-partially #'cae-avy-do #'cae-kill-region))
