;;; private/misc-applications/autoload/snake.el -*- lexical-binding: t; -*-

(cae-define-launcher
 cae-snake
 :launch-fn #'snake
 :buffer-name "*Snake*"
 :workspace-name cae-snake-workspace-name
 :setup-fn
 (lambda ()
   (let* ((saves-buf (find-file-noselect (expand-file-name "snake-scores" shared-game-score-directory)))
          (highest-score (with-current-buffer saves-buf
                           (local-set-key (kbd "q") #'cae-snake-quit)
                           (when (featurep 'evil)
                             (evil-local-set-key 'normal (kbd "q") #'cae-snake-quit))
                           (buffer-substring-no-properties (goto-char (point-min))
                                                           (line-end-position)))))
     (setq-local header-line-format highest-score)))
 :cleanup-fn
 (lambda ()
   (when-let ((saves-buf (get-file-buffer (expand-file-name "snake-scores" shared-game-score-directory))))
     (with-current-buffer saves-buf
       (save-buffer)
       (kill-buffer)))))
