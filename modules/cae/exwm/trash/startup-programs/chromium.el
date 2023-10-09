;;; startup/chromium.el -*- lexical-binding: t; -*-

(defvar startup/chromium-process nil)
(defvar startup/chromium-executable (or (executable-find "chromium-bin")
                        (executable-find "chromium")
                        (executable-find "google-chrome-unstable")
                        (executable-find "google-chrome-stable")))
(defvar startup/chromium-browser (if (or (executable-find "chromium-bin")
                           (executable-find "chromium"))
                     "chromium" "chrome"))
(defvar startup/chromium-workspace "Chrome")

(defun startup/start-chromium (&optional arg)
  (when startup/chromium-executable
    (setq startup/chromium-process
          (start-process "chromium"
                         " *startup/chromium*"
                         startup/chromium-executable
                         (if (eq (user-uid) 0) "--no-sandbox" "")))))

(defun startup/kill-chromium (&optional arg)
  (interactive "p")
  (when (process-live-p startup/chromium-process)
    (kill-process startup/chromium-process)))

(defun startup/restart-chromium (&optional arg)
  (interactive "p")
  (startup/kill-chromium)
  (startup/start-chromium arg))

(defun startup/select-chromium ()
  (interactive)
  (unless (and (process-live-p startup/chromium-process)
               (cl-find-if
                (lambda (buf)
                  (when-let
                      ((class (buffer-local-value 'exwm-class-name buf)))
                    (string-match-p startup/chromium-browser class)))
                (buffer-list)))
    (startup/restart-chromium))
  (+workspace-switch startup/chromium-workspace t)
  (+workspace/display))

(eval
 `(map! :map +startup-applications-map
        :prefix-map ("c" . ,startup/chromium-browser)
        "r" #'startup/restart-chromium
        "s" #'startup/select-chromium
        "x" #'startup/kill-chromium)
 t)
(after! which-key
  (which-key-add-keymap-based-replacements +startup-applications-map
                                           "c" "Chromium"
                                           "c r" "Restart Chromium"
                                           "c s" "Select Chromium"
                                           "c x" "Kill Chromium"))

(if (process-live-p startup/chromium-process)
    (startup/restart-chromium)
  (unless (getenv "SSH_TTY")
    (startup/start-chromium)))

(defadvice! +startup/chromium-browse-url-generic-a (&rest _)
  :before #'browse-url-generic
  (when (equal startup/chromium-executable browse-url-generic-program)
    (+workspace-switch startup/chromium-workspace t)
    (+workspace/display)))

(advice-add #'consult-gh--issue-browse-url-action :before #'+startup/chromium-browse-url-generic-a)
(advice-add #'consult-gh-embark-open-in-browser :before #'+startup/chromium-browse-url-generic-a)
(advice-add #'consult-gh--repo-browse-url-action :before #'+startup/chromium-browse-url-generic-a)
