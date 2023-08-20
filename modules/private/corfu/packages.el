;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :recipe (:files ("*.el" "extensions/*.el")))
(package! cape)
(when (modulep! +icons)
  (package! kind-icon))
(when (modulep! :os tty)
  (package! corfu-terminal))
(when (modulep! +orderless)
  (package! orderless))
(package! yasnippet-capf)
(when (modulep! :editor snippets)
  (package! yasnippet-capf :recipe
    (:host github :repo "LuigiPiucco/yasnippet-capf")))
