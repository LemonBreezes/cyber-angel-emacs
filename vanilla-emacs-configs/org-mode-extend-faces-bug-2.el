;;; vanilla-emacs-configs/org-mode-extend-faces-bug-2.el -*- lexical-binding: t; -*-

(let ((default-directory (file-name-directory load-file-name)))
  (when (file-exists-p (expand-file-name "default.el" default-directory))
    (load-file (expand-file-name "default.el" default-directory))))

(straight-use-package 'org)

(require 'org)
(load-theme 'leuven t)

(dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5
                org-level-6 org-level-7 org-level-8))
  (set-face-attribute face nil :extend t))

(setq org-fontify-whole-heading-line t)
(scratch-buffer)
(org-mode)
(insert "* Description :unfold:
This module provides code completion, powered by [[doom-package:corfu]].

It is recommended to enable either this or [[doom-module::completion company]], in
case you desire pre-configured auto-completion. Corfu is much lighter weight and
focused, plus it's built on native Emacs functionality, whereas company is heavy
and highly non-native, but has some extra features and more maturity.

** Maintainers
- [[doom-user:][@LuigiPiucco]]

[[doom-contrib-maintainer:][Become a maintainer?]]

** Packages
- [[doom-package:corfu]]
- [[doom-package:cape]]
- [[doom-package:nerd-icons-corfu]] if [[doom-module::completion corfu +icons]]
- [[doom-package:orderless]] if [[doom-module::completion corfu +orderless]]
- [[doom-package:corfu-terminal]] if [[doom-module::os tty]]
- [[doom-package:yasnippet-capf]] if [[doom-module::editor snippets]]

** Hacks
/No hacks documented for this module./

** TODO Changelog
# This section will be machine generated. Don't edit it by hand.
/This module does not have a changelog yet./

* Installation
Enable this module in your ~doom!~ block.

This module has no direct requirements, but some languages may have their own
requirements to fulfill before you get code completion in them (and some
languages may lack code completion support altogether). Run ~$ doom doctor~ to
find out if you're missing any dependencies. Note that Corfu may have support
for completions in languages that have no development intelligence, since it
supports generic, context insensitive candidates such as file names or recurring
words. Snippets may also appear in the candidate list if available.

* Configuration
A few variables may be set to change behavior of this module:

- [[var:completion-at-point-functions]] ::
  This is not a module/package variable, but a builtin Emacs one. Even so, it's
  very important to how Corfu works, so we document it here. It contains a list
  of functions that are called in turn to generate completion candidates. The
  regular (non-lexical) value should contain few entries and they should
  generally be context aware, so as to predict what you need. Additional
  functions can be added as you get into more and more specific contexts. Also,
  there may be cases where you know beforehand the kind of candidate needed, and
  want to enable only that one. For this, the variable may be lexically bound to
  the correct value, or you may call the CAPF interactively if a single function
  is all you need.
- [[var:corfu-auto-delay]] ::
  Number of seconds till completion occurs automatically. Defaults to 0.1.
- [[var:corfu-auto-prefix]] ::
  Number of characters till auto-completion starts to happen. Defaults to 2.
- [[var:corfu-on-exact-match]] ::
  Configures behavior for exact matches. Its default is nil, and it's
  recommended to leave it at that. Otherwise, single matches on snippet keys
  expand immediately.
- [[var:+corfu-buffer-scanning-size-limit]]  ::
  Sets the maximum buffer size to be scanned by ~cape-dabbrev~. Defaults to 1
  MB. Set this if you are having performance problems using the CAPF.
- [[var:+corfu-want-C-x-bindings]]  ::
  Enables autocompletion backends to be bound under the ~C-x~ prefix. This
  overrides some built-in Emacs keybindings.

** Adding CAPFs to a mode
To add other CAPFs on a mode-per-mode basis, put either of the following in your
~config.el~:

#+begin_src emacs-lisp
(add-hook! some-mode (add-hook 'completion-at-point-functions #'some-capf depth t))
;; OR, but note the different call signature
(add-hook 'some-mode-hook (lambda () (add-hook 'completion-at-point-functions #'some-capf depth t)))
#+end_src

~DEPTH~ above is an integer between -100, 100, and defaults to 0 of omitted.  Also
see ~add-hook!~'s documentation for additional ways to call it. ~add-hook~ only
accepts the quoted arguments form above.

** Adding CAPFs to a key
To add other CAPFs to keys, adapt the snippet below into your ~config.el~:
#+begin_src emacs-lisp
;; For binding inside `corfu-mode-map'. Line 1 ensures the binding only exists
;; after some-mode-hook runs. Line 2 is needed only if the binding can't leak
;; into other Corfu buffers. When neither of the above make sense, the `map!'
;; call is enough.
(add-hook! some-mode ; Only needed if the binding is mode-specific
           (make-local-variable 'corfu-mode-map)
           (map! :map corfu-mode-map
                 :prefix \"C-x\" ; C-x is usually used as prefix, but it's not required
                 \"e\" #'cape-emoji)) ; Evil users probably want :i to avoid this in other states
#+end_src
")
