;;; cae/ai/doctor.el -*- lexical-binding: t; -*-

(unless (or (not (modulep! +copilot)) (executable-find "node"))
  (warn! "Couldn't find node executable. Copilot code completion is disabled."))

(unless (executable-find "tmux")
  (warn! "Couldn't find tmux executable. Opening an AI coding assistant in the
terminal will not work."))
