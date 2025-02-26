;;; cae/misc-applications/autoload/misc-applications.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

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

;;;###autoload
(defmacro cae-misc-applications-with-cursor-hidden (&rest body)
  `(let ((cursor-type-old cursor-type)
         (evil-normal-state-cursor-old evil-normal-state-cursor)
         (evil-insert-state-cursor-old evil-insert-state-cursor)
         (evil-visual-state-cursor-old evil-visual-state-cursor)
         (evil-motion-state-cursor-old evil-motion-state-cursor)
         (evil-replace-state-cursor-old evil-replace-state-cursor)
         (evil-operator-state-cursor-old evil-operator-state-cursor)
         (evil-emacs-state-cursor-old evil-emacs-state-cursor))
     (setq-local cursor-type nil
                 evil-normal-state-cursor '(bar . 0)
                 evil-insert-state-cursor '(bar . 0)
                 evil-visual-state-cursor '(box . 0)
                 evil-motion-state-cursor '(box . 0)
                 evil-replace-state-cursor '(hbar . 0)
                 evil-operator-state-cursor '(hbar . 0)
                 evil-emacs-state-cursor '(hbar . 0))
     (unwind-protect (progn ,@body)
       (setq-local cursor-type cursor-type-old
                   evil-normal-state-cursor evil-normal-state-cursor-old
                   evil-insert-state-cursor evil-insert-state-cursor-old
                   evil-visual-state-cursor evil-visual-state-cursor-old
                   evil-motion-state-cursor evil-motion-state-cursor-old
                   evil-replace-state-cursor evil-replace-state-cursor-old
                   evil-operator-state-cursor evil-operator-state-cursor-old
                   evil-emacs-state-cursor evil-emacs-state-cursor-old))))

;;;###autoload
(defun cae-misc-applications-with-cursor-hidden-fn (fn &rest args)
  "Run FN with ARGS with the cursor hidden."
  (cae-misc-applications-with-cursor-hidden
   (apply fn args)))

;;;###autoload
(cl-defmacro cae-define-launcher (name &key
                                       launch-fn
                                       buffer-name
                                       workspace-name
                                       quit-fn
                                       setup-fn
                                       cleanup-fn
                                       (use-workspace t)
                                       (save-window-config t)
                                       (hide-cursor t)
                                       keymap
                                       keymap-setup-fn)
  "Define launcher and quit functions for applications.

NAME is the base name for the functions.
LAUNCH-FN is the function to start the application.
BUFFER-NAME is the name of the application's buffer.
WORKSPACE-NAME is the name of the workspace to use.
QUIT-FN is the name of the quit function to define.
SETUP-FN is a function to run after launching the application.
CLEANUP-FN is a function to run when quitting the application.
USE-WORKSPACE determines whether to use workspaces.
SAVE-WINDOW-CONFIG determines whether to save and restore window configurations.
HIDE-CURSOR determines whether to hide the cursor in the application buffer.
KEYMAP is the keymap to use for the application.
KEYMAP-SETUP-FN is a function to set up keybindings for the application."
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
         (when ,hide-cursor
           (with-current-buffer (or ,buffer-name (current-buffer))
             (cae-misc-applications-hide-cursor-h)))
         (when ,keymap
           (with-current-buffer (or ,buffer-name (current-buffer))
             (use-local-map ,keymap)))
         (when ,keymap-setup-fn
           (with-current-buffer (or ,buffer-name (current-buffer))
             (funcall ,keymap-setup-fn)))
         (when ,setup-fn
           (funcall ,setup-fn)))
       (defun ,quit-fn-name ()
         (interactive)
         (when ,cleanup-fn
           (funcall ,cleanup-fn))
         (if (and ,use-workspace (modulep! :ui workspaces))
             (when (+workspace-exists-p ,workspace-name)
               (+workspace/kill ,workspace-name))
           (when ,save-window-config
             (when ,old-wconf-var
               (set-window-configuration ,old-wconf-var))))
         (when (and ,buffer-name (get-buffer ,buffer-name))
           (kill-buffer ,buffer-name))))))

;;;###autoload
(cl-defmacro cae-define-game-launcher (name &key
                                           launch-fn
                                           buffer-name
                                           workspace-name
                                           (setup-keys t)
                                           (save-scores nil)
                                           scores-file)
  "Define a launcher for a game with common setup patterns.
NAME is the base name for the functions.
LAUNCH-FN is the function to start the game.
BUFFER-NAME is the name of the game's buffer.
WORKSPACE-NAME is the name of the workspace to use.
SETUP-KEYS determines whether to set up quit key bindings.
SAVE-SCORES determines whether to save and load high scores.
SCORES-FILE is the name of the scores file in `shared-game-score-directory'."
  (let* ((quit-fn-name (intern (format "cae-%s-quit" name)))
         (keymap-setup-fn
          (when setup-keys
            `(lambda ()
               (local-set-key (kbd "q") ',quit-fn-name)
               (when (featurep 'evil)
                 (evil-local-set-key 'normal (kbd "q") ',quit-fn-name)))))
         (setup-fn
          (if save-scores
              `(lambda ()
                 (let* ((saves-buf (find-file-noselect (expand-file-name ,scores-file shared-game-score-directory)))
                        (highest-score (with-current-buffer saves-buf
                                         (local-set-key (kbd "q") ',quit-fn-name)
                                         (when (featurep 'evil)
                                           (evil-local-set-key 'normal (kbd "q") ',quit-fn-name))
                                         (buffer-substring-no-properties (goto-char (point-min))
                                                                         (line-end-position)))))
                   (setq-local header-line-format highest-score)))
            nil))
         (cleanup-fn
          (when save-scores
            `(lambda ()
               (when-let ((saves-buf (get-file-buffer (expand-file-name ,scores-file shared-game-score-directory))))
                 (with-current-buffer saves-buf
                   (save-buffer)
                   (kill-buffer)))))))
    `(cae-define-launcher
      ,(intern (format "cae-%s" name))
      :launch-fn ,launch-fn
      :buffer-name ,buffer-name
      :workspace-name ,workspace-name
      :keymap-setup-fn ,keymap-setup-fn
      :setup-fn ,setup-fn
      ,@(when cleanup-fn
          `(:cleanup-fn ,cleanup-fn)))))

