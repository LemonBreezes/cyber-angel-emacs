;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :recipe (:files ("*.el" "extensions/*.el")))
(package! cape)
(package! dabbrev)
(when (modulep! +icons)
  (package! kind-icon))
(when (modulep! :os tty)
  (package! corfu-terminal))
(when (modulep! :editor snippets)
  (package! company :recipe '(:files ("company.el" "company-yasnippet.el")) :pin "1005540b1cdf176cbcf893b2fa83d2075cbbe3ca"))
