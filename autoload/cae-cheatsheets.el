;;; autoload/cae-cheatsheets.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'cae-embark-collect-cheatsheet-hydra/body "autoload/cae-cheatsheets" nil t)
(defhydra cae-embark-collect-cheatsheet-hydra (:color pink :foreign-keys run)
  ("a" embark-act "Act" :column "Act")
  ("A" embark-act-all "Act on all" :column "Act")
  ("E" embark-export "Export" :column "Act")
  ("S" tabulated-list-sort "Sort" :column "Navigate")
  ("m" embark-collect-mark "Mark" :column "Act")
  ("s" isearch-forward "Search forward" :column "Navigate")
  ("{" outline-previous-heading "Previous heading" :column "Navigate")
  ("}" outline-next-heading "Next heading" :column "Navigate")
  ("u" embark-collect-unmark "Unmark" :column "Act")
  ("U" embark-collect-unmark-all "Unmark all" :column "Act")
  ("t" embark-collect-toggle-marks "Toggle marks" :column "Act")
  ("M-a" embark-collect-direct-action-minor-mode "Toggle direct action" :column "Act")
  ("M-<left>" tabulated-list-previous-column "Previous column" :column "Navigate")
  ("M-<right>" tabulated-list-next-column "Next column" :column "Navigate")
  ("<f6>" nil "Exit" :exit t :column nil))

;;;###autoload (autoload 'cae-embark-collect-cheatsheet "autoload/cae-cheatsheets" nil t)
(hercules-def
 :toggle-funs #'cae-embark-collect-cheatsheet
 :keymap 'embark-collect-mode-map
 :package 'embark)

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
 :keymap 'macrostep-keymap
 :package 'macrostep)
