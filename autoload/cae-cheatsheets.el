;;; autoload/cae-cheatsheets.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'cae-vertico-cheatsheet-hydra/body "autoload/cae-cheatsheets" nil t)
(defhydra cae-vertico-cheatsheet-hydra (:color pink :foreign-keys run)
  ("C-c ;" embark-export "Export")
  ("C-c C-e" +vertico/embark-export-write "Export writable")
  ("C-c C-l" embark-collect "Collect")
  ("q" nil "Exit" :exit t))

;;;###autoload (autoload 'cae-debugger-cheatsheet "autoload/cae-cheatsheets" nil t)
(hercules-def
 :toggle-funs #'cae-debugger-cheatsheet
 :keymap 'debugger-mode-map
 :package 'debug)

;;;###autoload (autoload 'cae-edebug-cheatsheet "autoload/cae-cheatsheets" nil t)
(hercules-def
 :toggle-funs #'cae-edebug-cheatsheet
 :keymap 'edebug-mode-map
 :package 'edebug)

;;;###autoload (autoload 'cae-macrostep-cheatsheet "autoload/cae-cheatsheets" nil t)
(hercules-def
 :toggle-funs #'cae-macrostep-cheatsheet
 :keymap 'macrostep-mode-keymap
 :package 'macrostep)

;;;###autoload (autoload 'cae-symbol-overlay-cheatsheet "autoload/cae-cheatsheets" nil t)
(hercules-def
 :toggle-funs #'cae-symbol-overlay-cheatsheet
 :keymap 'symbol-overlay-map
 :package 'symbol-overlay)
