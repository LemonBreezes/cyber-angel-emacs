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
(insert "#+title:    :completion corfu
#+subtitle: Complete with cap(f), cape and a flying feather
#+created:  September 9, 2022
#+since:    3.0.0 (#7002)

* Description :unfold:
This module provides code completion, powered by [[doom-package:corfu]].

It is recommended to enable either this or [[doom-module::completion company]], in
case you desire pre-configured auto-completion. Corfu is much lighter weight and
focused, plus it's built on native Emacs functionality, whereas company is heavy
and highly non-native, but has some extra features and more maturity.

** Maintainers
- [[doom-user:][@LuigiPiucco]]

[[doom-contrib-maintainer:][Become a maintainer?]]

** Module flags
- +icons ::
  Display icons beside completion suggestions.
- +tng ::
  Known as Tab'n'Go to Company users, changes behavior to invoke completion on
  [[kbd:][TAB]]. When Corfu is active, [[kbd:][TAB]] and [[kbd:][S-TAB]] will navigate the completion
  candidates. Arrow keys and evil-style movement are still supported.
- +orderless ::
  Pull in [[doom-package:orderless]] if necessary and apply multi-component
  completion (still needed if [[doom-module::completion vertico]] is active).
- +dabbrev ::
  Enable and configure [[doom-package:dabbrev]] as a close-to-universal CAPF
  fallback.
- +dict ::
  Enable and configure dictionary completion for text modes and related regions
  in programming modes.
- +emoji ::
  Enable and configure emoji completion via the emoji input method.

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

| Keybind  | Description                                               |
|----------+-----------------------------------------------------------|
| [[kbd:][<down>]]   | Go to next candidate                                      |
| [[kbd:][<up>]]     | Go to previous candidate                                  |
| [[kbd:][C-n]]      | Go to next candidate                                      |
| [[kbd:][C-p]]      | Go to previous candidate                                  |
| [[kbd:][C-j]]      | (evil) Go to next candidate                               |
| [[kbd:][C-k]]      | (evil) Go to previous candidate                           |
| [[kbd:][C-<down>]] | Go to next doc line                                       |
| [[kbd:][C-<up>]]   | Go to previous doc line                                   |
| [[kbd:][C-S-n]]    | Go to next doc line                                       |
| [[kbd:][C-S-p]]    | Go to previous doc line                                   |
| [[kbd:][C-S-j]]    | (evil) Go to next doc line                                |
| [[kbd:][C-S-k]]    | (evil) Go to previous doc line                            |
| [[kbd:][C-h]]      | Toggle documentation (if available)                       |
| [[kbd:][C-S-s]]    | Export to minibuffer (if [[doom-module::completion vertico]]) |
| [[kbd:][RET]]      | Insert candidate                                          |
| [[kbd:][SPC]]      | Quit autocompletion or pass-through after a wildcard      |
| [[kbd:][C-SPC]]    | Complete (unless [[doom-module::completion corfu +tng]])      |
| [[kbd:][C-SPC]]    | (when completing) Insert separator DWIM (see below)       |

If you prefer a [[kbd:][TAB]]-centric completion style, enable the [[doom-module::completion
corfu +tng]] flag so that, instead, you trigger completion with [[kbd:][TAB]], getting the
following additional binds:

| Keybind | Description                                   |
|---------+-----------------------------------------------|
| [[kbd:][TAB]]     | Complete                                      |
| [[kbd:][TAB]]     | (when completing) Go to next candidate        |
| [[kbd:][S-TAB]]   | (when completing) Go to previous candidate    |
| [[kbd:][DEL]]     | (when completing) Reset completion DWIM-style |

*** Completion in the minibuffer
In the minibuffer, sometimes autocompletion can interfere with your goal;
Imagine you're composing a search pattern incrementally, and you find what you
want early, with only half the word. You then press [[kbd:RET]]. If completion
kicked in as you typed, you may lose the match, since it will complete the
first candidate. On the other hand, if you were paying attention to the
suggestions and selecting one appropriate, that's desired behavior, and you may
even desire to modify the prompt further (if you were composing a command
instead, you may want to extend it after the candidate). To allow better
control, there are 3 confirm bindings when Corfu appears in the minibuffer:

| Keybind   | Description                                                        |
|-----------+--------------------------------------------------------------------|
| [[kbd:RET]]   | Accept the candidate only                                          |
| [[kbd:C-RET]] | Confirm the current prompt only                                    |
| [[kbd:S-RET]] | Accept the candidate then immediately confirm the completed prompt |

- Use [[kbd:RET]] when you want to continue composing after completing;
- Use [[kbd:C-RET]] when you already have the desired string, and completing would
  break it;
- Use [[kbd:S-RET]] when you know the composition will be finished after completion
  (thus avoiding the need to type [[kbd:RET]] twice);

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

* Troubleshooting
[[doom-report:][Report an issue?]]

If you have performance issues with ~cape-dabbrev~, the first thing I recommend
doing is to look at the list of buffers Dabbrev is scanning:

#+begin_src emacs-lisp
(dabbrev--select-buffers) ; => (#<buffer README.org> #<buffer config.el<3>> #<buffer cape.el> ...)
(length (dabbrev--select-buffers)) ; => 37
#+end_src

... and modify ~dabbrev-ignored-buffer-regexps~ or ~dabbrev-ignored-buffer-modes~
accordingly.

* Frequently asked questions
/This module has no FAQs yet./ [[doom-suggest-faq:][Ask one?]]

* TODO Appendix
#+begin_quote
 🔨 This module has no appendix yet. [[doom-contrib-module:][Write one?]]
#+end_quote
")
