;;; private/misc-applications/autoload/misc-applications.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-misc-applications-hide-cursor-h ()
  (setq-local cursor-type nil)
  (hl-line-mode +1)
  (setq-local evil-normal-state-cursor '(bar . 0)
              evil-insert-state-cursor '(bar . 0)
              evil-visual-state-cursor '(box . 0)
              evil-motion-state-cursor '(box . 0)
              evil-replace-state-cursor '(hbar . 0)
              evil-operator-state-cursor '(hbar . 0)
              evil-emacs-state-cursor '(hbar . 0)))

(defmacro cae-define-launcher (name &key
                             launch-fn
                             buffer-name
                             workspace-name
                             quit-fn
                             setup-fn
                             cleanup-fn
                             (use-workspace t)
                             (save-window-config t))
  "Define launcher and quit functions for applications.

NAME is the base name for the functions.
LAUNCH-FN is the function to start the application.
BUFFER-NAME is the name of the application's buffer.
WORKSPACE-NAME is the name of the workspace to use.
QUIT-FN is the name of the quit function to define.
SETUP-FN is a function to run after launching the application.
CLEANUP-FN is a function to run when quitting the application.
USE-WORKSPACE determines whether to use workspaces.
SAVE-WINDOW-CONFIG determines whether to save and restore window configurations."
  (let ((launcher-fn-name (intern (format "%s" name)))
        (quit-fn-name (or quit-fn (intern (format "%s-quit" name))))
        (old-wconf-var (intern (format "%s--old-wconf" name))))
    `(progn
       (defvar ,old-wconf-var nil)
       (defun ,launcher-fn-name (&optional arg)
         (interactive "P")
         (if arg
             (setq ,old-wconf-var nil)
           (if (and ,use-workspace (modulep! :ui workspaces))
               (progn
                 (+workspace-switch ,workspace-name t)
                 (set-persp-parameter 'dont-save-to-file t
                                      (+workspace-get ,workspace-name)))
             (when ,save-window-config
               (setq ,old-wconf-var (current-window-configuration)))
             (let ((ignore-window-parameters t))
               (delete-other-windows))
             (switch-to-buffer (doom-fallback-buffer))))
         (call-interactively ,launch-fn)
         (when (and ,use-workspace (modulep! :ui workspaces))
           (persp-add-buffer (current-buffer)))
         (when ,setup-fn
           (funcall ,setup-fn)))
       (defun ,quit-fn-name ()
         (interactive)
         (if (and ,use-workspace (modulep! :ui workspaces))
             (when (+workspace-exists-p ,workspace-name)
               (+workspace/kill ,workspace-name))
           (when ,save-window-config
             (when ,old-wconf-var
               (set-window-configuration ,old-wconf-var))))
         (when ,buffer-name
           (kill-buffer ,buffer-name))
         (when ,cleanup-fn
           (funcall ,cleanup-fn))))))
