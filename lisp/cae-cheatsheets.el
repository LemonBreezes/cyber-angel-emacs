;;; lisp/cae-workspaces.el -*- lexical-binding: t; -*-

;;; Pause and resume cheatsheets with the minibuffer

(defun cae-cheatsheets-hydra-pause-h (&rest _)
  (when hydra-curr-map
    (if (modulep! :ui workspaces)
        (ring-insert hydra-pause-ring hydra-curr-body-fn))
    (hydra-keyboard-quit)))

(defun cae-cheatsheets-hydra-resume-h (&rest _)
  (unless (zerop (ring-length hydra-pause-ring))
    (run-with-timer 0.001 nil hydra-pause-ring)))

(after! hydra
  (add-hook 'minibuffer-setup-hook #'cae-cheatsheets-hydra-pause-h)
  (add-hook 'minibuffer-exit-hook #'cae-cheatsheets-hydra-resume-h))

;; Hercules already does this for us since I am using a fork of Hercules with
;; that feature.

;;; Pause and resume with workspaces

(defvar cae-cheatsheets-workspace--last-hydra nil)

(defun cae-cheatsheets-workspace-hydra-pause-h (&rest _)
  (when (featurep 'hydra)
    (set-persp-parameter 'hydra-pause-ring hydra-pause-ring)
    (when hydra-curr-map
      (set-persp-parameter 'cae-cheatsheets-workspace--last-hydra
                           hydra-curr-body-fn))
    (hydra-keyboard-quit)))

(defun cae-cheatsheets-workspace-hydra-resume-h (&rest _)
  (setq hydra-pause-ring (persp-parameter 'hydra-pause-ring))
  (when (persp-parameter 'cae-cheatsheets-workspace--last-hydra)
    (run-with-timer 0.001 nil (persp-parameter 'cae-cheatsheets-workspace--last-hydra)))
  (set-persp-parameter 'cae-cheatsheets-workspace--last-hydra nil))

(when (modulep! :ui workspaces)
  (after! persp-mode
    (add-hook 'persp-before-switch-functions
              #'cae-cheatsheets-workspace-hydra-pause-h)
    (add-hook 'persp-activated-functions
              #'cae-cheatsheets-workspace-hydra-resume-h)))

;; Haven't done this for Hercules yet but I might never get around to it since I
;; don't use Hercules that much.

;;; Quit before switching tabs

(defun cae-cheatsheets-hydra-quit-h (&rest _)
  (hydra-keyboard-quit))

(add-hook 'cae-tab-bar-before-switch-hook #'cae-hacks-hydra-quit-h)

(after! hercules
  (add-hook 'cae-tab-bar-before-switch-hook #'hercules--hide))
