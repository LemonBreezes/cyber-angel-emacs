;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :recipe (:files ("*.el" "extensions/*.el")) :pin "3ba98519998e67b493e454fc0c3971c206b11bf9")
(package! cape :pin "5b28cd43f2efa19dbf5053f164cce622a4b5bdae")
(when (modulep! +icons)
  (package! kind-icon :pin "3fedd3054e9148fc0a13a06b267351fce6a385b1"))
(when (modulep! :os tty)
  (package! corfu-terminal :pin "5ce4c11b8efd4d2fd1b404b9422bb85b05476da0"))
(when (modulep! +orderless)
  (package! orderless :pin "e6784026717a8a6a7dcd0bf31fd3414f148c542e"))
