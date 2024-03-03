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

* TODO Usage
#+begin_quote
 🔨 /This module's usage documentation is incomplete./ [[doom-contrib-module:][Complete it?]]
#+end_quote

** Code completion
By default, completion gets triggered after typing 2 non-space consecutive
characters, or by means of the [[kbd:][C-SPC]] keybinding at any moment. While the popup
is visible, the following relevant keys are available:

** Searching with multiple keywords
If the [[doom-module::completion corfu +orderless]] flag is enabled, users can
perform code completion with multiple search keywords by use of space as the
separator. More information can be found [[https://github.com/oantolin/orderless#company][here]]. Pressing [[kdb:][C-SPC]] again while
completing inserts a space as separator. This allows searching with
space-separated terms; each piece will match individually and in any order, with
smart casing. Pressing just [[kbd:][SPC]] acts as normal and quits completion, so that
when typing sentences it doesn't try to complete the whole sentence instead of
just the word. Pressing [[kdb:][C-SPC]] with point after a separator escapes it with a
backslash, including the space in the search term, and pressing it with an
already escaped separator before point deletes it. Thus, you can cycle back if
you accidentaly press more than needed.

Additionally, for users of evil and regular corfu style, [[kdb:][C-SPC]] is smart
regarding your state. In normal-like states, enter insert then start corfu; in
visual-like states, perform [[help:evil-change][evil-change]] (which leaves you in insert state) then
start corfu; in insert-like states, start corfu immediatelly.

** Exporting to the minibuffer
The entries shown in the completion popup can be exported to a ~completing-read~
minibuffer, giving access to all the manipulations that suite allows. Using
Vertico for instance, one could use this to export with [[doom-package:embark]] via
[[kbd:][C-c C-l]] and get a buffer with all candidates.

** Manually call generic CAPFs
Completion at point functions have the property that, when called interactively
via their symbol, they work as a call to ~completion-at-point~ where
[[var:completion-at-point-functions]] is bound to that CAPF alone. This allows to
assign generic functions to a binding and call as needed, leaving the default
value used for most completion tasks much leaner (thus, faster and easier to
look through). This module provides some such bindings for Evil users (see the
table below), and you're free map your own of course. Emacs users have to map it
themselves for now, due to the author's lack of knowledge on ergonomic
equivalents to the Evil ones. If you have suggestions, though, we'd be happy to
know!

| Keybind | Description                     |
|---------+---------------------------------|
| [[kbd:][C-x]] [[kbd:][C-l]] | (insert-state) ~cape-line~      |
| [[kbd:][C-x]] [[kbd:][C-k]] | (insert-state) ~cape-keyword~   |
| [[kbd:][C-x]] [[kbd:][C-f]] | (insert-state) ~cape-file~      |
| [[kbd:][C-x]] [[kbd:][s]]   | (insert-state) ~cape-dict~      |
| [[kbd:][C-x]] [[kbd:][C-s]] | (insert-state) ~yasnippet-capf~ |
| [[kbd:][C-x]] [[kbd:][C-n]] | (insert-state) ~cape-dabbrev~   |
| [[kbd:][C-x]] [[kbd:][C-p]] | (insert-state) ~cape-history~   |
")
