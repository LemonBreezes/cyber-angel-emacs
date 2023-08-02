;;; private/misc-applications/autoload/somafm.el -*- lexical-binding: t; -*-

(eval-when-compile (require 'hydra nil t))

;;;###autoload
(defun +somafm ()
  (interactive)
  (require 'somafm)
  (if (cae-display-graphic-p)
      (call-interactively #'somafm)
    (call-interactively #'somafm-by-completion)))

;;;###autoload (autoload '+somafm-hydra/body "private/misc-applications/autoload/somafm" nil t)
(defhydra +somafm-hydra (:color pink :hint nil)
  ("<f6>" nil "Exit" :exit t)
  ("q" quit-window nil :exit t)
  ("RET" somafm--play "Play")
  ("l" somafm--sort "Sort")
  ("s" somafm--stop "Stop")
  ("g" somafm--refresh-and-show-channels-buffer "Refresh"))
