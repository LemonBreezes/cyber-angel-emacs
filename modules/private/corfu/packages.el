;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :recipe (:files ("*.el" "extensions/*.el")))
(package! cape)
(when (modulep! +icons)
  (package! svg-lib)
  (package! kind-icon))
(when (modulep! +orderless)
  (package! orderless))
(when (modulep! :os tty)
  (package! corfu-terminal))
(when (modulep! :editor snippets)
  (package! yasnippet-capf))
