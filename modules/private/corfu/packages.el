;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :recipe (:files ("*.el" "extensions/*.el") :branch "swap-icon-index"))
(package! cape)
(package! dabbrev)
(when (modulep! +icons)
  (package! kind-icon))
(when (modulep! :os tty)
  (package! corfu-terminal))
