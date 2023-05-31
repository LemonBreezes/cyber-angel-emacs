;;; lisp/cae-workspaces.el -*- lexical-binding: t; -*-

(defun cae-hacks-hydra-pause-h (&rest _)
  (when hydra-curr-map
    (set-persp-parameter 'hydra-pause-ring
                         (progn (ring-insert hydra-pause-ring hydra-curr-body-fn)
                                hydra-pause-ring))
    (hydra-keyboard-quit)))

(defun cae-hacks-hydra-resume-h (&rest _)
  (unless (or (null (persp-parameter 'hydra-pause-ring))
              (zerop (ring-length (persp-parameter 'hydra-pause-ring))))
    (run-with-timer 0.001 nil (ring-remove (persp-parameter 'hydra-pause-ring) 0))))

(add-hook 'persp-before-switch-functions #'cae-hacks-hydra-pause-h)
(add-hook 'persp-activated-functions #'cae-hacks-hydra-resume-h)

;;; Tab bar

(after! hercules
  (add-hook 'cae-tab-bar-before-switch-hook #'hercules--hide))
(add-hook 'cae-tab-bar-before-switch-hook #'cae-hacks-hydra-quit-h)
