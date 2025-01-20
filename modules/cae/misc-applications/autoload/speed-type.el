;;; private/misc-applications/autoload/speed-type.el -*- lexical-binding: t; -*-

(cae-define-launcher
  cae-speed-type-text
  :launch-fn #'speed-type-text
  :buffer-name "speed-type"
  :workspace-name cae-speed-type-workspace-name)
