;;; lisp/cae-workspaces.el -*- lexical-binding: t; -*-

(defun cae-cheatsheets-hydra-pause-h (&rest _)
  (when (bound-and-true-p hydra-curr-map)
    (set-persp-parameter 'hydra-pause-ring
                         (progn (ring-insert hydra-pause-ring hydra-curr-body-fn)
                                hydra-pause-ring))
    (hydra-keyboard-quit)))

(defun cae-cheatsheets-hydra-resume-h (&rest _)
  (unless (or (null (persp-parameter 'hydra-pause-ring))
              (zerop (ring-length (persp-parameter 'hydra-pause-ring))))
    (run-with-timer 0.001 nil (ring-remove (persp-parameter 'hydra-pause-ring) 0))))

(when (modulep! :ui workspaces)
  (after! persp-mode
    (add-hook 'persp-before-switch-functions #'cae-cheatsheets-hydra-pause-h)
    (add-hook 'persp-activated-functions #'cae-cheatsheets-hydra-resume-h)))
