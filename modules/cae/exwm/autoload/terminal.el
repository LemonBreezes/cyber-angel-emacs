;;; cae/exwm/autoload/terminal.el -*- lexical-binding: t; -*-

;; cae-exwm-terminal-command is defined in config.el

;;;###autoload
(defun cae-exwm-run-terminal-in-current-workspace (&optional initial-input)
  "Run terminal in current workspace without auto-persp.
  Renames the buffer to *terminal:project-name* where project-name is
  the current projectile project name or 'default' if no project.
  When called non-interactively, INITIAL-INPUT is sent to the terminal."
  (interactive)
  (unless cae-exwm-terminal-command
    (error "cae-exwm-terminal-command is not defined. Please configure it in your exwm config."))
  (require 'projectile)
  (when (one-window-p)
    (split-window-right)
    (other-window 1))
  (let ((process-name (file-name-nondirectory cae-exwm-terminal-command)))
    (start-process process-name nil cae-exwm-terminal-command)
    
    ;; Set up hook to inhibit auto-persp when terminal buffer appears
    (add-hook 'exwm-manage-finish-hook #'cae-exwm-inhibit-terminal-auto-persp)
    
    ;; If initial-input is provided, send it after a delay
    (when initial-input
      (run-with-timer 0.5 nil (lambda (input)
                                (let ((project-name (or (projectile-project-name) "default"))
                                      (terminal-class (file-name-nondirectory cae-exwm-terminal-command)))
                                  (when-let ((buffer (get-buffer (format "*%s:%s*" terminal-class project-name))))
                                    (with-current-buffer buffer
                                      ;; Ensure the window is focused
                                      (select-window (get-buffer-window buffer))
                                      ;; Send each character with proper delay and special character handling
                                      (dolist (char (string-to-list input))
                                        (exwm-input--fake-key char)
                                        (sit-for 0.01))
                                      ;; Send Return at the end
                                      (exwm-input--fake-key 'return)))))
                      initial-input))))

;;;###autoload
(defun cae-exwm-inhibit-terminal-auto-persp ()
  "Inhibit auto-persp for terminal and rename buffer.
  This function runs during exwm-manage-finish-hook, setting the
  buffer-local inhibition variable BEFORE the auto-persp predicate runs."
  (unless cae-exwm-terminal-command
    (error "cae-exwm-terminal-command is not defined. Please configure it in your exwm config."))
  (require 'projectile)
  (let ((terminal-class (file-name-nondirectory cae-exwm-terminal-command)))
    (when (string= exwm-class-name terminal-class)
      ;; Set buffer-local inhibition BEFORE auto-persp runs
      (setq-local cae-exwm-inhibit-auto-persp t)
      (setq-local cae-exwm-inhibit-title-renaming t)

      ;; Rename buffer to *terminal:project-name*
      (let ((project-name (or (projectile-project-name) "default")))
        (rename-buffer (format "*%s:%s*" terminal-class project-name))
        (persp-add-buffer (current-buffer)))

      ;; Clean up hook
      (remove-hook 'exwm-manage-finish-hook #'cae-exwm-inhibit-terminal-auto-persp))))

;;;###autoload
(defun cae-exwm-run-kitty-in-current-workspace (&optional initial-input)
  "Run kitty in current workspace without auto-persp.
  Renames the buffer to *kitty:project-name* where project-name is
  the current projectile project name or 'default' if no project.
  When called non-interactively, INITIAL-INPUT is sent to the terminal."
  (interactive)
  (let ((cae-exwm-terminal-command "kitty"))
    (cae-exwm-run-terminal-in-current-workspace initial-input)))
