;;; cae/exwm/autoload/auto-persp.el -*- lexical-binding: t; -*-

(require 'persp-mode)

;;;###autoload
(defun cae-exwm-browse-url-generic-a (&rest _)
  "Switch to the appropriate workspace before opening a URL."
  (when-let ((workspace (cae-exwm--get-browser-workspace-name)))
    (+workspace-switch workspace t)
    (+workspace/display)))
