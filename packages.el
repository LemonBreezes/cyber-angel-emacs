;; -*- no-byte-compile: t; -*-
(unpin! t)
(disable-packages! evil-escape
                   flyspell
                   flycheck
                   ccls
                   compat)
(unless (modulep! :config default +smartparens)
  (disable-packages! smartparens))
(when (modulep! :checkers syntax +flymake)
  (disable-packages! flycheck))

;; Packages that are having bugs being byte-compiled in Emacs30.
(package! yasnippet :recipe (:build (:not compile)))

;;; UI
(package! info-colors)
(package! authinfo-color-mode :recipe (:host github :repo "tecosaur/authinfo-color-mode"))
(package! goggles)
(package! rainbow-mode)
(package! topsy)
(package! anzu)
(package! isearch-mb)
(package! hercules :recipe (:host github :repo "Zetagon/hercules"))

;; cae-theme.el
(package! modus-themes)
(package! circadian)
(package! standard-themes)
(package! ef-themes)

;;; Tools
(package! nov)
(package! syslog-mode)
(package! w3m)
(package! ace-link)
(package! pdftotext :recipe (:host github :repo "tecosaur/pdftotext.el"))

;; These are used for their autoloads. They are not explicitly referenced in
;; this configuration.
(package! crontab-mode)
(package! ebuild-mode :built-in t)
(package! font-lock-studio)             ;I don't use this often, but it's useful
                                        ;for debugging font-lock issues.
(package! git-modes)
(package! free-keys)

;;; Editor
(package! pp+)
(package! avy :recipe (:repo "LemonBreezes/avy" :branch "master"))
(package! zop-to-char)
(package! aggressive-indent)
(package! hungry-delete)
(package! file-info)
(package! titlecase)
(package! macrursors :recipe (:host github :repo "corytertel/macrursors"))
(when (modulep! :editor multiple-cursors)
  (package! mc-extras))
(package! speedrect :recipe (:host github :repo "jdtsmith/speedrect"))
(package! avy-embark-collect)
(package! restore-point
  :recipe (:host github :repo "arthurcgusmao/restore-point"))
(package! symbol-overlay)
(package! sentex :recipe (:repo "https://codeberg.org/martianh/sentex.git"))
(package! edit-indirect)
(package! string-edit-at-point)

;; These are used for their autoloads. They are not explicitly referenced in
;; this configuration.
(package! ialign)

;; cae-multi.el
(package! git-auto-commit-mode)

;; cae-repeat.el
(package! define-repeat-map :recipe (:repo "https://tildegit.org/acdw/define-repeat-map.el.git"))

;; cae-keyboard.el
(package! auto-activating-snippets :recipe (:host github :repo "ymarco/auto-activating-snippets"))
(package! smart-semicolon)
(package! electric-spacing)

;; cae-vlf.el
(package! vlf)

;;; Autocompletion
(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
(package! isearch-dabbrev)
(package! consult-yasnippet)

;; cae-corfu.el
(package! cape-yasnippet :recipe (:host github :repo "elken/cape-yasnippet"))

;;; AI
(package! org-ai)
(package! gptel)
