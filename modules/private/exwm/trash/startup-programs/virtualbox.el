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
                                                 startup/virtualbox-vm))))))

(defun startup/kill-virtualbox (&optional arg)
  (interactive "p")
  (when (process-live-p startup/virtualbox-process)
    (shell-command-to-string (format "vboxmanage controlvm %s savestate"
                                     startup/virtualbox-uuid))))

(defun startup/restart-virtualbox (&optional arg)
  (interactive "p")
  (startup/kill-virtualbox)
  (startup/start-virtualbox arg))

(defun startup/select-virtualbox (&optional no-vm-p)
  (interactive "P")
  (unless (process-live-p startup/virtualbox-process)
    (startup/start-virtualbox nil no-vm-p))
  (+workspace-switch startup/virtualbox-workspace t))

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

;; (if (process-live-p startup/virtualbox-process)
;;     (startup/restart-virtualbox)
;;   (startup/start-virtualbox))
