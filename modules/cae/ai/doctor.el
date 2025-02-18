;;; cae/ai/doctor.el -*- lexical-binding: t; -*-

(unless (or (not (modulep! +copilot)) (executable-find "node"))
  (warn! "Couldn't find node executable. Copilot code completion is disabled."))

