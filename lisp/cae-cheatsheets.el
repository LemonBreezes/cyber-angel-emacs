;;; lisp/cae-cheatsheets.el -*- lexical-binding: t; -*-

;;; Pause and resume hydras with the minibuffer

(defvar cae-cheatsheets-minibuffer--last-hydra nil)
(defvar cae-cheatsheets-minibuffer--last-workspace nil)
(defvar cae-cheatsheets-minibuffer--last-tab nil)

(defun cae-cheatsheets-minibuffer-hydra-pause-h (&rest _)
  (when (bound-and-true-p hydra-curr-map)
    (setq cae-cheatsheets-minibuffer--last-hydra hydra-curr-body-fn
          cae-cheatsheets-minibuffer--last-workspace (and (featurep 'persp-mode)
                                                          (persp-name persp-curr))
          cae-cheatsheets-minibuffer--last-tab (tab-bar--current-tab))
    (hydra-keyboard-quit)))

(defun cae-cheatsheets-minibuffer-hydra-resume-h (&rest _)
  (run-with-timer 0.001 nil
                  (lambda ()
                    (when (and cae-cheatsheets-minibuffer--last-hydra
                               (eq (tab-bar--current-tab)
                                   cae-cheatsheets-minibuffer--last-tab)
                               (or (not (featurep 'persp-mode))
                                   (string= (persp-name persp-curr)
                                            cae-cheatsheets-minibuffer--last-workspace)))
                      (funcall cae-cheatsheets-minibuffer--last-hydra)
                      (setq cae-cheatsheets-minibuffer--last-hydra nil)))))

(after! hydra
  (add-hook 'minibuffer-setup-hook #'cae-cheatsheets-minibuffer-hydra-pause-h)
  (add-hook 'minibuffer-exit-hook #'cae-cheatsheets-minibuffer-hydra-resume-h))

(when (modulep! :ui workspaces)
  (add-hook 'persp-before-switch-functions
            (cae-defun cae-cheatsheets-minibuffer-hydra-store-h (_ _)
              (set-persp-parameter 'cae-cheatsheets-workspace--last-hydra
                                   cae-cheatsheets-workspace--last-hydra)))
  (add-hook 'persp-activated-functions
            (cae-defun cae-cheatsheets-minibuffer-resume-h (_)
              (setq cae-cheatsheets-minibuffer--last-hydra
                    (persp-parameter 'cae-cheatsheets-minibuffer--last-hydra)))))

;; Hercules already does this for us since I am using a fork of Hercules with
;; that feature.

;;; Pause and resume hydras with workspaces

(defvar cae-cheatsheets-workspace--last-hydra nil)

(defun cae-cheatsheets-workspace-hydra-pause-h (&rest _)
  (when (bound-and-true-p hydra-curr-map)
    (hydra-keyboard-quit)
    (set-persp-parameter 'cae-cheatsheets-workspace--last-hydra
                         hydra-curr-body-fn)))

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
;; don't use Hercules that much. Same for the tab bar stuff.

;;; Save and restore hydras before switching tabs

(defvar cae-cheatsheets-tab-bar-hydra-alist nil)

(defun cae-sheetsheets-tab-bar-store-hydra-h (&rest _)
  (when (bound-and-true-p hydra-curr-map)
    (setf (alist-get (tab-bar--current-tab) cae-cheatsheets-tab-bar-hydra-alist
                     nil nil #'eq)
          hydra-curr-body-fn)
    (hydra-keyboard-quit)))

(defun cae-cheatsheets-tab-bar-resume-hydra-h (&rest _)
  (when (featurep 'hydra)
    (hydra-keyboard-quit))
  (when-let ((hydra (alist-get (tab-bar--current-tab)
                               cae-cheatsheets-tab-bar-hydra-alist
                               nil nil #'eq)))
    (setf (alist-get (tab-bar--current-tab) cae-cheatsheets-tab-bar-hydra-alist
                     nil t #'eq)
          nil)
    (run-with-timer 0.001 nil hydra)))

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

;;; Hide Hercules when switching tabs
