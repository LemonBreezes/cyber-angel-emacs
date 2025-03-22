;;; doctor.el -*- lexical-binding: t; -*-

;; LSP performance booster
(unless (executable-find "emacs-lsp-booster")
  (warn! "Couldn't find emacs-lsp-booster executable. LSP performance will be degraded."))

;; LSP servers
(unless (executable-find "lua-language-server") 
  (warn! "Couldn't find lua-language-server executable. Lua LSP features will be unavailable."))

(unless (executable-find "clangd")
  (warn! "Couldn't find clangd executable. C/C++ LSP features will be unavailable."))

(unless (executable-find "fennel-ls")
  (warn! "Couldn't find fennel-ls executable. Fennel LSP features will be unavailable."))

;; Diff tools
(unless (executable-find "dwdiff")
  (warn! "Couldn't find dwdiff executable. Some diff features may be limited."))

;; Print tools
(unless (executable-find "lpr")
  (warn! "Couldn't find lpr executable. Printing functionality will be unavailable."))

;; Browsers - Only warn if none are available
(unless (or (executable-find "firefox") 
            (executable-find "firefox-beta") 
            (executable-find "firefox-bin")
            (executable-find "chromium-bin-browser")
            (executable-find "google-chrome-unstable")
            (executable-find "google-chrome-stable"))
  (warn! "Couldn't find any supported browser. External browsing capabilities will be limited."))

;; Already checked by Doom.
;;(unless (executable-find "rg")
;;  (warn! "Couldn't find ripgrep (rg) executable. Search functionality will be degraded."))
;;(when (and (modulep! :lang python) (not (executable-find "nose2")))
;;  (warn! "Couldn't find nose2 executable. Python nose testing will be unavailable."))
