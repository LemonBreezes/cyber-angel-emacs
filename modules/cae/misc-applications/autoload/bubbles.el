;;; private/misc-applications/autoload/bubbles.el -*- lexical-binding: t; -*-

(cae-define-launcher
  cae-bubbles
  :launch-fn #'bubbles
  :buffer-name "*bubbles*"
  :workspace-name cae-bubbles-workspace-name)
