;;; private/misc-applications/autoload/pulseaudio-control.el -*- lexical-binding: t; -*-

;;;###autoload
(defun pulseaudio-control-decrease-volume-less ()
  (interactive)
  (let ((pulseaudio-control-volume-step "5%"))
    (pulseaudio-control-decrease-volume)))

;;;###autoload
(defun pulseaudio-control-increase-volume-less ()
  (interactive)
  (let ((pulseaudio-control-volume-step "5%"))
    (pulseaudio-control-increase-volume)))
