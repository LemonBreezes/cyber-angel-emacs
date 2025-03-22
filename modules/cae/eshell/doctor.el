;;; cae/eshell/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "atuin")
  (warn! (concat "Couldn't find atuin executable. "
                 "Atuin will not be used for shell history.")))
(unless (executable-find "dtach")
  (warn! (concat "Couldn't find dtach executable. "
                 "Will not be able to detach shell commands from Emacs.")))
