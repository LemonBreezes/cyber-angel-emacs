;;; vanilla-emacs-configs/put-text-property-right-endpoint.el -*- lexical-binding: t; -*-

(scratch-buffer)
(insert "hello world")
(font-lock-mode -1)
(put-text-property (point-min) (point-max) 'face '(:foreground "red"))
(message "get-text-property: %s, get-pos-property: %s"
         (get-text-property (point-max) 'face)
         (get-pos-property (point-max) 'face))
