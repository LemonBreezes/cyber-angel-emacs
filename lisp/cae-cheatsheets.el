;;; lisp/cae-cheatsheets.el -*- lexical-binding: t; -*-

;;; Pause and resume cheatsheets with the minibuffer

(defvar cae-cheatsheets-minibuffer--last-hydra nil)

(defun cae-cheatsheets-minibuffer-hydra-pause-h (&rest _)
  (when hydra-curr-map
    (setq cae-cheatsheets-minibuffer--last-hydra hydra-curr-body-fn)
    (hydra-keyboard-quit)))

(defun cae-cheatsheets-minibuffer-hydra-resume-h (&rest _)
  (when cae-cheatsheets-minibuffer--last-hydra
    (run-with-timer 0.001 nil hydra-pause-ring)
    (setq cae-cheatsheets-minibuffer--last-hydra nil)))

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
  (when (persp-parameter 'cae-cheatsheets-workspace--last-hydra)
    ;; In my testing, using a timer prevented Hydra from clobbering my
    ;; workspace-switching repeat map.
    (run-with-timer 0.001 nil (persp-parameter 'cae-cheatsheets-workspace--last-hydra)))
  (set-persp-parameter 'cae-cheatsheets-workspace--last-hydra nil))

(when (modulep! :ui workspaces)
  (add-hook 'persp-before-switch-functions
            #'cae-cheatsheets-workspace-hydra-pause-h)
  (add-hook 'persp-activated-functions
            #'cae-cheatsheets-workspace-hydra-resume-h))

;; Haven't done this for Hercules yet and I might never get around to it since I
;; don't use Hercules that much.

;;; Quit before switching tabs

(defvar cae-cheatsheets-tab-bar-hydra-alist nil)

(defun cae-sheetsheets-tab-bar-store-hydra-h (&rest _)
  (when hydra-curr-map
    (setf (alist-get (tab-bar--current-tab) cae-cheatsheets-tab-bar-hydra-alist
                     nil nil #'equal)
          hydra-curr-body-fn)
    (hydra-keyboard-quit)))

(defun cae-cheatsheets-tab-bar-resume-hydra-h (&rest _)
  (when-let ((hydra (alist-get (tab-bar--current-tab)
                               cae-cheatsheets-tab-bar-hydra-alist
                               nil nil #'equal)))
    (setf (alist-get (tab-bar--current-tab) cae-cheatsheets-tab-bar-hydra-alist
                     nil t #'equal)
          nil)
    (run-with-timer 0.001 nil hydra)))

(defun cae-cheatsheets-hydra-quit-h (&rest _)
  (hydra-keyboard-quit))

(add-hook 'cae-tab-bar-before-switch-hook #'cae-sheetsheets-tab-bar-store-hydra-h)
(add-hook 'cae-tab-bar-after-switch-hook #'cae-cheatsheets-tab-bar-resume-hydra-h)

(defun cae-cheatsheets-tab-bar-workspace-store-hydra-h (&rest _)
  (set-persp-parameter 'cae-cheatsheets-tab-bar-hydra-alist
                         cae-cheatsheets-tab-bar-hydra-alist))

(defun cae-cheatsheets-tab-bar-workspace-resume-hydra-h (&rest _)
  (setq cae-cheatsheets-tab-bar-hydra-alist
        (persp-parameter 'cae-cheatsheets-tab-bar-hydra-alist)))

(when (modulep! :ui workspaces)
  (add-hook 'persp-before-switch-functions #'cae-cheatsheets-tab-bar-workspace-store-hydra-h)
  (add-hook 'persp-activated-functions #'cae-cheatsheets-tab-bar-workspace-resume-hydra-h))

(after! hercules
  (add-hook 'cae-tab-bar-before-switch-hook #'hercules--hide))
