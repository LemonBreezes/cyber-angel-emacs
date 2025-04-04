;;; cae/exwm/autoload/debug.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-exwm-test-workspace-matching (program-name)
  "Test the workspace matching logic for PROGRAM-NAME.
Returns the matched workspace name or nil."
  (interactive "sProgram name to test: ")
  (let ((result (cae-exwm-find-workspace-for-program program-name)))
    (message "Program '%s' matches workspace: %s" program-name (or result "None"))
    result))
