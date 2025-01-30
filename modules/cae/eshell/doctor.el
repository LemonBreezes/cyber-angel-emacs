;;; cae/eshell/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "node")
  (warn! (concat "Couldn't find atuin executable. "
                 "Atuin will not be used for shell history.")))
