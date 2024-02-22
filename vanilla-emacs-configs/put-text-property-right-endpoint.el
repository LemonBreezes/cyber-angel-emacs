;;; vanilla-emacs-configs/put-text-property-right-endpoint.el -*- lexical-binding: t; -*-

(scratch-buffer)
(insert "hello world")
(put-text-property (point) (point-max) 'face '(:foreground "red"))
