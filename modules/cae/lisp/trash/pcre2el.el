;;; cae/lisp/trash/pcre2el.el -*- lexical-binding: t; -*-

;; This package was removed from Doom's core.

;; This tool helps us a lot with regular expressions
(after! pcre2el
  (after! which-key
    (which-key-add-keymap-based-replacements rxt-mode-map
      "C-c /" "pcre2el"
      "C-c / e" "elisp"
      "C-c / p" "pcre"))
  (map! :map rxt--read-pcre-mode-map
        "C-c C-i" #'rxt--toggle-i-mode
        "C-c C-t" #'rxt--toggle-s-mode
        "C-c C-x" #'rxt--toggle-x-mode)
  (undefine-key! rxt--read-pcre-mode-map
    "C-c i" "C-c s" "C-c x"))
(add-hook 'emacs-lisp-mode-hook #'rxt-mode)
