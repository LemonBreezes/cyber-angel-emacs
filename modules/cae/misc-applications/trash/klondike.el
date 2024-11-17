;;; cae/misc-applications/trash/klondike.el -*- lexical-binding: t; -*-

;; I have no idea how to play this game and I have not set up Evil keybindings
;; for it but it looks really cool. It's basically Solitaire.
(use-package! klondike
  :defer t :init
  (map! :map +misc-applications-games-map
        "k" #'klondike)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-games-map
      "k" "Solitaire"))
  (when (modulep! :editor evil)
    (after! evil
      (evil-set-initial-state 'klondike-mode 'emacs)
      (evil-set-initial-state 'klondike-select-mode 'emacs)
      (evil-set-initial-state 'klondike-picker-mode 'emacs)))
  :config
  (map! :map klondike-mode-map
        :g "?" #'describe-mode
        :ng "q" #'bury-buffer
        :map klondike-picker-mode-map
        :g "?" #'describe-mode
        :ng "q" #'bury-buffer
        :map klondike-select-mode-map
        :g "?" #'describe-mode
        :ng "q" #'bury-buffer))
