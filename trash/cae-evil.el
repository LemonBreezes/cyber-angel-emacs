;;; trash/cae-evil.el -*- lexical-binding: t; -*-

(use-package! evil-textobj-synax
  :defer t :init
  (after! evil
    (map! :map evil-inner-text-objects-map
          "h" #'evil-i-syntax
          :map evil-outer-text-objects-map
          "h" #'evil-a-syntax)))

(use-package! harpoon
  :defer t :init
  (map! :leader
        "1" #'harpoon-go-to-1
        "2" #'harpoon-go-to-2
        "3" #'harpoon-go-to-3
        "4" #'harpoon-go-to-4
        "5" #'harpoon-go-to-5
        "6" #'harpoon-go-to-6
        "7" #'harpoon-go-to-7
        "8" #'harpoon-go-to-8
        "9" #'harpoon-go-to-9)
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '(("" . "harpoon-go-to-[0-9]+") . ignore))))
