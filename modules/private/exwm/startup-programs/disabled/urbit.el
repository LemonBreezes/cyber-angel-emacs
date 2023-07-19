;;; startup/urbit.el -*- lexical-binding: t; -*-

(defvar startup/urbit-buffer "*urbit*")
(cl-letf ((symbol-function #'y-or-n-p) (symbol-function #'always))
  (require 'vterm))

(defun startup/start-urbit (&optional arg)
  (save-window-excursion
    (vterm--internal #'ignore startup/urbit-buffer)
    (with-current-buffer startup/urbit-buffer
      (vterm-send-string "urbit ~/.config/Port/piers/my-ship/\n"))))

(defun startup/kill-urbit ()
  (interactive)
  (when (buffer-live-p (get-buffer startup/urbit-buffer))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer (get-buffer startup/urbit-buffer)))))

(defun startup/restart-urbit (&optional arg)
  (interactive "p")
  (startup/kill-urbit)
  (startup/start-urbit)
  (when arg (+workspace-switch startup/urbit-workspace)))

(defun startup/manage-urbit ()
  (when (equal (buffer-name) startup/urbit-buffer)
    (+workspace-switch startup/urbit-workspace t)
    (when (+popup-buffer-p)
      (other-window 1))
    (switch-to-buffer startup/urbit-buffer)
    (+workspace/other)))

(defun startup/select-urbit ()
  (interactive)
  (unless (process-live-p (get-buffer-process startup/urbit-buffer))
    (startup/restart-urbit))
  (display-buffer startup/urbit-buffer))

;; Uncomment if you want to have a dedicated workspace for Urbit.
;; (add-hook 'vterm-mode-hook #'startup/manage-urbit)

(set-popup-rule! startup/urbit-buffer :quit nil :select t :ttl nil)

(map! :leader
      :prefix +startup-prefix
      (:prefix ("u" . "Urbit")
       :desc "Restart Urbit" "r" #'startup/restart-urbit
       :desc "Select Urbit" "s" #'startup/select-urbit
       :desc "Kill Urbit" "x" #'startup/kill-urbit))

(if (buffer-live-p (get-buffer startup/urbit-buffer))
    (startup/restart-urbit)
  (startup/start-urbit))
