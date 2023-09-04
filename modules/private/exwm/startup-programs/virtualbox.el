;;; startup/virtualbox.el -*- lexical-binding: t; -*-

(defvar startup/virtualbox-process nil)
(defvar startup/virtualbox-workspace "VirtualBox")
(defvar startup/virtualbox-vm "win32")
(defvar startup/virtualbox-executable (executable-find "VirtualBox"))
(defvar startup/virtualbox--timer nil)

(defun startup/start-virtualbox (&optional arg no-vm-p)
  (when startup/virtualbox-executable
    (setq startup/virtualbox-process
          (if no-vm-p
              (start-process "virtualbox"
                             " *startup/virtualbox*"
                             startup/virtualbox-executable)
            (start-process-shell-command "virtualbox"
                                         " *startup/virtualbox*"
                                         (format "vboxmanage startvm --type=gui %s"
                                                 startup/virtualbox-vm))))
    (when arg (+workspace-switch startup/virtualbox-workspace t))))

(defun startup/kill-virtualbox (&optional arg)
  (interactive "p")
  (when (process-live-p startup/virtualbox-process)
    (shell-command-to-string (format "vboxmanage controlvm %s savestate"
                                     startup/virtualbox-uuid)))
  (when (and arg (+workspace-exists-p startup/virtualbox-workspace))
    (when (string= startup/virtualbox-workspace
                   (+workspace-current-name))
      (+workspace/other))
    (+workspace-delete startup/virtualbox-workspace)))

(defun startup/restart-virtualbox (&optional arg)
  (interactive "p")
  (startup/kill-virtualbox)
  (startup/start-virtualbox arg))

(defun startup/manage-virtualbox ()
  (when (and (stringp exwm-class-name)
             (string-match-p "VirtualBox" exwm-class-name))
    (hide-mode-line-mode +1)
    (unless (string= (+workspace-current-name) startup/virtualbox-workspace)
      (previous-buffer))
    (unless (+workspace-exists-p startup/virtualbox-workspace)
      (+workspace-new startup/virtualbox-workspace)
      (set-persp-parameter 'dont-save-to-file t (persp-get-by-name startup/virtualbox-workspace)))))

(defun startup/select-virtualbox (&optional no-vm-p)
  (interactive "P")
  (unless (process-live-p startup/virtualbox-process)
    (startup/start-virtualbox nil no-vm-p))
  (+workspace-switch startup/virtualbox-workspace t)
  (setq startup/virtualbox--timer
        (run-at-time 1 0.05
                     (lambda ()
                       (if (and (stringp exwm-class-name)
                                (string-match-p "VirtualBox" exwm-class-name))
                           (cancel-timer startup/virtualbox--timer)
                         (+workspace-switch-to-exwm-buffer-maybe)))))
  (defadvice! tmp/cancel-discord-timer-a (&rest _)
    :before #'+workspace-switch
    (cancel-timer startup/virtualbox--timer)
    (advice-remove #'+workspace-switch #'tmp/cancel-discord-timer-a)))

(when (modulep! :completion vertico)
  ;; Do not grab input from Consult.
  (defadvice! +consult--buffer-preview-a (oldfun &rest args)
    :around #'consult--buffer-preview
    (-orfn (lambda (action cand)
             (and cand
                  (string-match-p " \\[Running\\] - Oracle VM VirtualBox"
                                  cand)
                  t))
           (apply oldfun args))))

(map! :map +startup-applications-map
      :prefix "v"
      "r" #'startup/restart-virtualbox
      "s" #'startup/select-virtualbox
      "x" #'startup/kill-virtualbox)
(after! which-key
  (which-key-add-keymap-based-replacements +startup-applications-map
    "v" "VirtualBox"
    "v r" "Restart VirtualBox"
    "v s" "Select VirtualBox"
    "v x" "Kill VirtualBox"))
(add-hook 'exwm-manage-finish-hook #'startup/manage-virtualbox)

;; (if (process-live-p startup/virtualbox-process)
;;     (startup/restart-virtualbox)
;;   (startup/start-virtualbox))
