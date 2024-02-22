;;; vanilla-emacs-configs/put-text-property-right-endpoint.el -*- lexical-binding: t; -*-

(scratch-buffer)
(insert "hello world")
(font-lock-mode -1)
(put-text-property (point-min) (point-max) 'face '(:foreground "red"))
(message "%s" (get-text-property (point-max) 'face))
