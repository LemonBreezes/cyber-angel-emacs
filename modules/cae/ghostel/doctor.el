;;; cae/ghostel/doctor.el -*- lexical-binding: t; -*-

(unless (fboundp 'module-load)
  (warn! "Your Emacs wasn't built with dynamic modules support. The ghostel module won't load"))
