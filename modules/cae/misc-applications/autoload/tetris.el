;;;###autoload (autoload 'cae-tetris "cae/misc-applications/autoload/tetris" nil t)
;;;###autoload (autoload 'cae-tetris-quit "cae/misc-applications/autoload/tetris" nil t)

;;; private/misc-applications/autoload/tetris.el -*- lexical-binding: t; -*-

(cae-define-launcher
  cae-tetris
  :launch-fn #'tetris
  :buffer-name "*Tetris*"
  :workspace-name cae-tetris-workspace-name
  :setup-fn
  (lambda ()
    (let* ((saves-buf (find-file-noselect (expand-file-name "tetris-scores" shared-game-score-directory)))
           (highest-score (with-current-buffer saves-buf
                            (local-set-key (kbd "q") #'cae-tetris-quit)
                            (when (featurep 'evil)
                              (evil-local-set-key 'normal (kbd "q") #'cae-tetris-quit))
                            (buffer-substring-no-properties (goto-char (point-min))
                                                            (line-end-position)))))
      (setq-local header-line-format highest-score)))
  :cleanup-fn
  (lambda ()
    (when-let ((saves-buf (get-file-buffer (expand-file-name "tetris-scores" shared-game-score-directory))))
      (with-current-buffer saves-buf
        (save-buffer)
        (kill-buffer)))))
