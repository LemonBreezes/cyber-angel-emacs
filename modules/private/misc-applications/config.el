;;; private/misc-applications/config.el -*- lexical-binding: t; -*-

;;; Preamble

(defvar +misc-applications-music-dir "/mnt/hdd/music/"
  "The directory where your music library is located.")

(defvar +misc-applications-lisp-files nil)
(defvar +misc-applications-map (make-sparse-keymap))
(define-prefix-command '+misc-applications-map)
(defvar +misc-applications-prefix "a")

;; "a" is bound to `emabark-act', which I use `<f8>' for instead.
(keymap-unset doom-leader-map +misc-applications-prefix t)
(map! :leader :desc "misc-applications" +misc-applications-prefix #'+misc-applications-map)
(after! which-key
  (which-key-add-keymap-based-replacements 'doom-leader-map
    +misc-applications-prefix "misc-applications"))
(defvar application-types
  '(("games" "g")
    ("eyecandy" "e")
    ("system" "s")
    ("external-apps" "x")
    ("standalone-apps" "t")
    ("insert" "i")
    ("music" "m")
    ("helm" "h")))

(mapc (lambda (app-type)
        (let* ((name (car app-type))
               (prefix (cadr app-type))
               (var-prefix (concat "+misc-applications-" name "-prefix"))
               (var-map (concat "+misc-applications-" name "-map")))
          (set (intern var-prefix) prefix)
          (set (intern var-map) (make-sparse-keymap))
          (define-prefix-command (intern var-map))
          (define-key +misc-applications-map (kbd prefix) (intern var-map))))
      application-types)

(after! which-key
  (mapc (lambda (app-type)
          (let* ((name (car app-type))
                 (prefix (cadr app-type)))
            (which-key-add-keymap-based-replacements '+misc-applications-map
              prefix name)))
        application-types))

(map! :map +misc-applications-system-map
      "e" #'list-packages
      "E" #'list-processes
      "T" #'list-timers)
(after! which-key
  (which-key-add-keymap-based-replacements '+misc-applications-system-map
    "e" "List Emacs packages"
    "E" "List Emacs processes"
    "T" "List Emacs timers"))
(after! timer-list
  (map! :map timer-list-mode-map
        "<f6>" #'cae-cheatsheets-timer-list
        :n "<f6>" #'cae-cheatsheets-evil-timer-list
        :n "gr" #'revert-buffer))
(map! :map process-menu-mode-map
      "<f6>" #'cae-cheatsheets-process-menu
      :n "<f6>" #'cae-cheatsheets-evil-process-menu
      :n "gr" #'revert-buffer)
(after! package
  (map! :map package-menu-mode-map
        "<f6>" #'cae-cheatsheets-package-menu
        :n "<f6>" #'cae-cheatsheets-evil-package-menu
        :n "s /" #'package-menu-clear-filter
        :n "s N" #'package-menu-filter-by-name-or-description
        :n "s a" #'package-menu-filter-by-archive
        :n "s d" #'package-menu-filter-by-description
        :n "s k" #'package-menu-filter-by-keyword
        :n "s m" #'package-menu-filter-marked
        :n "s n" #'package-menu-filter-by-name
        :n "s s" #'package-menu-filter-by-status
        :n "s u" #'package-menu-filter-upgradable
        :n "s v" #'package-menu-filter-by-version))

(when (or (modulep! :private helm)
          (modulep! :completion helm))
  (map! :map +misc-applications-map
        "h" (cae-oneshot-keymap helm-command-map helm))
  (after! which-key
    (which-key-add-keymap-based-replacements '+misc-applications-map
      "h" "helm")))


;;; Standalone apps

(use-package! alarm-clock
  :defer t :init
  (map! :map +misc-applications-standalone-apps-map
        :prefix "a"
        "a" #'alarm-clock-set
        "A" #'alarm-clock-list-view)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-standalone-apps-map
      "a" "alarms"
      "a a" "Set alarm"
      "a A" "List alarms"))
  :config
  (map! :map +misc-applications-standalone-apps-map
        :prefix "a"
        "k" #'alarm-clock-kill)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-standalone-apps-map
      "a k" "Kill alarm"))
  (setq alarm-clock-cache-file
        (expand-file-name "alarm-clock.cache" doom-cache-dir))
  (alarm-clock--turn-autosave-on))

(use-package! elfeed
  :when (modulep! :app rss)
  :defer t :init
  (map! :map +misc-applications-standalone-apps-map
        "r" #'=rss)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-standalone-apps-map
      "r" "RSS"))
  :config
  (after! cae-cheatsheets
    (let ((custom-filters
           `(("R" ,(cmd! () (elfeed-search-set-filter "@6-months-ago +reddit")) "reddit" :column "Custom filters")
             ("E" ,(cmd! () (elfeed-search-set-filter "@6-months-ago +emacs")) "emacs" :column "Custom filters")
             ("Y" ,(cmd! () (elfeed-search-set-filter "@6-months-ago +tube")) "youtube" :column "Custom filters")
             ("*" ,(cmd! () (elfeed-search-set-filter "@6-months-ago +star")) "star" :column "Custom filters")
             ("a" ,(cmd! () (elfeed-search-set-filter "@6-months-ago")) "All" :column "Custom filters")
             ("T" ,(cmd! () (elfeed-search-set-filter "@1-day-ago")) "Today" :column "Custom filters"))))
      (dolist (hydra '(cae-cheatsheets-elfeed-hydra cae-cheatsheets-evil-elfeed-hydra))
        (eval `(defhydra+ ,hydra () ,@custom-filters)))
      (cl-loop for (key . filter) in custom-filters do
               (map! :ng key filter))))
  (after! recentf
    (push elfeed-db-directory recentf-exclude))
  (map! :map elfeed-show-mode-map
        "?" #'describe-mode
        :map elfeed-search-mode-map
        "?" #'describe-mode
        :ng "q" #'+elfeed-quit
        :n "b" #'elfeed-search-browse-url
        :n "m" #'elfeed-toggle-star
        :n "F" #'elfeed-tube-fetch
        :n "l" #'+elfeed-toggle-log-buffer
        :ng "t" #'mark-whole-buffer)
  (map! :map elfeed-search-mode-map
        "<f6>" #'cae-cheatsheets-elfeed
        :n "<f6>" #'cae-cheatsheets-evil-elfeed)

  (use-package! elfeed-tube
    :config
    (elfeed-tube-setup)
    (map! :map elfeed-show-mode-map
          "F" #'elfeed-tube-fetch
          [remap save-buffer] #'elfeed-tube-save
          :map elfeed-search-mode-map
          "F" #'elfeed-tube-fetch
          [remap save-buffer] #'elfeed-tube-save)))


;;; External apps

(use-package! leetcode
  :defer t :init
  (defvar +leetcode-workspace-name "*leetcode*"
    "The name of the workspace to use for leetcode.")
  (when (memq system-type '(cygwin windows-nt ms-dos))
    (advice-add #'leetcode--install-my-cookie :override #'ignore))
  (map! :map +misc-applications-external-apps-map
        "l" #'+leetcode)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-external-apps-map
      "l" "LeetCode"))
  :config
  (map! :map leetcode--problems-mode-map
        "q" #'+leetcode-soft-quit
        "Q" #'+leetcode-quit
        "<f6>" #'cae-cheatsheets-leetcode-problems
        :n "<f6>" #'cae-cheatsheets-evil-leetcode-problems
        :map leetcode--problem-detail-mode-map
        "o" #'link-hint-open-link)
  (add-hook 'leetcode-solution-mode-hook
            (lambda ()
              ;; Flycheck will emit errors because the code does not have any
              ;; import/include/etc statements. Besides, we can't use Flycheck
              ;; in an interview anyway.
              (when (bound-and-true-p flycheck-mode)
                (flycheck-mode -1))
              ;; Copilot is basically cheating, so disable it too.
              (when (bound-and-true-p copilot-mode)
                (copilot-mode -1))))
  (setq leetcode-save-solutions t)
  (setq leetcode-directory "~/src/leetcode"))

(use-package! consult-gh
  :defer t :when (modulep! :completion vertico)
  :commands (consult-gh-orgs
             consult-gh-repo-clone
             consult-gh-search-repos
             consult-gh-search-issues
             consult-gh-find-file
             consult-gh-repo-fork)
  :init
  (defvar consult-preview-key 'any)
  (setq consult-gh-prioritize-local-folder 'suggest
        consult-gh-confirm-before-clone nil)
  (map! :map +misc-applications-external-apps-map
        "go" #'consult-gh-orgs
        "gc" #'consult-gh-repo-clone
        "gs" #'consult-gh-search-repos
        "gi" #'consult-gh-search-issues
        "gf" #'consult-gh-find-file
        "gk" #'consult-gh-repo-fork)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-external-apps-map
      "g" "GitHub CLI"
      "go" "Organizations"
      "gc" "Clone repo"
      "gs" "Search repos"
      "gi" "Search issues"
      "gf" "Find file"
      "gk" "Fork repo"))
  :config
  (setq consult-gh-default-clone-directory "~/src/"
        consult-gh-show-preview t
        consult-gh-issue-action #'consult-gh--issue-browse-url-action
        consult-gh-repo-action #'consult-gh--repo-browse-files-action
        consult-gh-file-action #'consult-gh--files-view-action
        consult-gh-preview-buffer-mode #'org-mode
        consult-gh-default-orgs-list '("oantolin" "minad" "alphapapa"
                                       "LemonBreezes" "protesilaos"
                                       "emacs-mirror" "doomemacs" "tecosaur"
                                       "systemcrafters")))
(use-package! consult-gh-embark
  :after (consult-gh embark))


;;; System

(use-package! daemons
  :when (eq system-type 'gnu/linux)
  :defer t :init
  (map! :map +misc-applications-system-map
        "u" #'daemons)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-system-map
      "u" "daemons"))
  :config
  (map! :map daemons-mode-map
        "<f6>" #'cae-cheatsheets-daemons
        :n "<f6>" #'cae-cheatsheets-evil-daemons)
  (setq daemons-always-sudo t
        daemons-show-output-in-minibuffer t))

(use-package! disk-usage
  :defer t :init
  (map! :map +misc-applications-system-map
        "d" #'disk-usage)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-system-map
      "d" "disk usage"))
  :config
  (map! :map disk-usage-mode-map
        "<f6>" #'cae-cheatsheets-disk-usage
        :n "<f6>" #'cae-cheatsheets-evil-disk-usage))

(use-package! helm-linux-disks
  :when (and (eq system-type 'gnu/linux)
             (or (modulep! :private helm)
                 (modulep! :completion helm)))
  :defer t :init
  (map! :map +misc-applications-system-map
        "D" #'helm-linux-disks)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-system-map
      "D" "Linux disks")))

(use-package! helm-system-packages
  :when (and (not (memq system-type '(cygwin windows-nt ms-dos)))
             (or (modulep! :private helm)
                 (modulep! :completion helm)))
  :defer t :init
  (map! :map +misc-applications-system-map
        "s" #'helm-system-packages)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-system-map
      "s" "system packages")))

(use-package! pulseaudio-control
  :when (and (eq system-type 'gnu/linux)
             (executable-find "pactl"(and (eq system-type 'gnu/linux)
                                          (executable-find "pactl"))))
  :defer t :init
  (map! :map ctl-x-map
        "/" (cae-oneshot-keymap pulseaudio-control-map
                                pulseaudio-control))
  :config
  (pulseaudio-control-default-keybindings)
  (after! which-key
    (push '((nil . "pulseaudio-control-\\(.*\\)") . (nil . "\\1"))
          which-key-replacement-alist))
  (setq pulseaudio-control-use-default-sink t)
  (pulseaudio-control-default-keybindings))

(use-package! trashed
  :defer t :init
  (map! :map +misc-applications-system-map
        "t" #'trashed)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-system-map
      "t" "trash files"))
  (advice-add #'trashed :around #'+trashed-revert-buffer-a)
  (add-hook 'trashed-mode-hook #'+misc-applications-hide-cursor-h)
  (add-hook 'trashed-mode-hook #'doom-mark-buffer-as-real-h)
  :config
  (map! :map trashed-mode-map
        "<f6>" #'cae-cheatsheets-trashed
        :n "<f6>" #'cae-cheatsheets-evil-trashed))

(use-package! proced
  :defer t :init
  (map! :map +misc-applications-system-map
        "p" #'proced)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-system-map
      "p" "List system processes"))
  (when (modulep! :editor evil)
    (after! evil-snipe
      (cl-pushnew #'proced-mode evil-snipe-disabled-modes)))
  :config
  (setq proced-enable-color-flag t)
  (add-hook 'proced-mode-hook #'+misc-applications-hide-cursor-h)
  (map! :map proced-mode-map
        "<f6>" #'cae-cheatsheets-proced
        :n "<f6>" #'cae-cheatsheets-evil-proced
        "c" #'proced-mark-children
        :n "gr" #'revert-buffer))

(use-package! neato-graph-bar
  :defer t :init
  (map! :map +misc-applications-system-map
        "g" #'neato-graph-bar)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-system-map
      "g" "Resource usage graph"))
  :config
  (map! :map neato-graph-bar-mode-map
        :ng "q" #'quit-window))


;;; Insert

(use-package! helm-rage
  :when (or (modulep! :private helm)
            (modulep! :completion helm))
  :defer t :init
  (map! :map +misc-applications-insert-map
        "r" #'helm-rage)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-insert-map
      "r" "Insert ASCII memes")))

(use-package! lorem-ipsum
  :defer t :init
  (map! :map +misc-applications-insert-map
        (:prefix "l"
         "l" #'lorem-ipsum-insert-list
         "p" #'lorem-ipsum-insert-paragraphs
         "s" #'lorem-ipsum-insert-sentences))
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-insert-map
      "l" "lorem-ipsum"
      "l l" "Insert junk list"
      "l p" "Insert junk paragraph"
      "l s" "Insert junk sentences")))

(use-package! password-generator
  :defer t :init
  (map! :map +misc-applications-insert-map
        (:prefix "p"
         "c" #'password-generator-custom
         "s" #'password-generator-simple
         "t" #'password-generator-strong
         "n" #'password-generator-numeric
         "p" #'password-generator-paranoid
         "h" #'password-generator-phonetic))
  (after! which-key
    (which-key-add-keymap-based-replacements
      +misc-applications-insert-map
      "p" "password-generator"
      "pc" "Custom alphabet"
      "ps" "Simple"
      "pt" "Strong"
      "pn" "Numeric"
      "pp" "Paranoid"
      "ph" "Phonetic")))

(use-package! uuidgen
  :defer t :init
  (map! :map +misc-applications-insert-map
        "u" #'uuidgen)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-insert-map
      "u" "Insert UUID")))


;;; Games

(use-package! bubbles
  :defer t :init
  (defvar +bubbles-workspace-name "*bubbles*")
  (defvar +bubbles--old-wconf nil)
  (map! :map +misc-applications-games-map
        "b" #'+bubbles)
  :config
  (when (modulep! :editor evil)
    (after! evil-snipe
      (cl-pushnew #'bubbles-mode evil-snipe-disabled-modes)))
  (map! :map bubbles-mode-map
        :ng "q" #'+bubbles-quit
        :n "RET" #'bubbles-plop
        "<f6>" #'cae-cheatsheets-bubbles
        :ng "u" #'bubbles-undo
        :ng "ta" #'bubbles-set-graphics-theme-ascii
        :ng "tb" #'bubbles-set-graphics-theme-balls
        :ng "te" #'bubbles-set-graphics-theme-emacs
        :ng "tc" #'bubbles-set-graphics-theme-circles
        :ng "ts" #'bubbles-set-graphics-theme-squares
        :ng "td" #'bubbles-set-graphics-theme-diamonds
        :ng "dh" #'bubbles-set-game-hard
        :ng "de" #'bubbles-set-game-easy
        :ng "dm" #'bubbles-set-game-medium
        :ng "dd" #'bubbles-set-game-difficult
        :ng "du" #'bubbles-set-game-userdefined
        :ng "S" #'bubbles-save-settings))

(use-package! dunnet
  :defer t :init
  (defvar +dunnet-workspace-name "*dunnet*")
  (defvar +dunnet--old-wconf nil)
  (map! :map +misc-applications-games-map
        "d" #'+dunnet)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-games-map
      "d" "Dunnet"))
  :config
  (when (modulep! :editor evil)
    (after! evil
      (evil-set-initial-state #'dun-mode 'insert)))
  (map! :map dun-mode-map
        "C-c C-k" #'+dunnet-quit
        :n "q" #'+dunnet-quit))

(use-package! speed-type
  :defer t :init
  (defvar +speed-type-workspace-name "*speed-type*")
  (defvar +speed-type--old-wconf nil)
  (map! :map +misc-applications-games-map
        "T" #'+speed-type-text)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-games-map
      "T" "Speed Type"))
  (add-hook 'speed-type-mode-hook #'visual-line-mode)
  :config
  (when (modulep! :private corfu)
    (add-to-list 'corfu-excluded-modes #'speed-type-mode))
  (when (modulep! :editor evil)
    (after! evil
      (evil-set-initial-state #'speed-type-mode 'insert)))
  (map! :map speed-type--completed-keymap
        "q" #'+speed-type-quit
        "r" #'speed-type--replay
        "n" #'speed-type--play-next
        :map speed-type-mode-map
        "<f6>" #'cae-cheatsheets-speed-type
        :n "q" #'+speed-type-quit))

(use-package! snake
  :defer t :init
  (defvar +snake-workspace-name "*snake*")
  (defvar +snake--old-wconf nil)
  (map! :map +misc-applications-games-map
        "s" #'+snake)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-games-map
      "s" "Snake"))
  :config
  (map! :map snake-mode-map
        "<f6>" #'cae-cheatsheets-snake
        :ng "q" #'+snake-quit
        :map snake-null-map
        :ng "q" #'+snake-quit))

(use-package! tetris
  :defer t :init
  (defvar +tetris-workspace-name "*tetris*")
  (defvar +tetris--old-wconf nil)
  (map! (:map +misc-applications-games-map
         "t" #'+tetris))
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-games-map
      "t" "Tetris"))
  :config
  (map! :map tetris-mode-map
        "<f6>" #'cae-cheatsheets-tetris
        :n "<f6>" #'cae-cheatsheets-evil-tetris
        :ng "a" #'autotetris-mode
        :ng "q" #'+tetris-quit)
  (map! :map autotetris-mode-map
        "a" nil))                       ;Not sure what `autotetris-move' even
                                        ;does to be honest.


;;; Eyecandy

(use-package! fireplace
  :defer t :init
  (defvar +fireplace-workspace-name "*fireplace*")
  (defvar +fireplace--old-wconf nil)
  (map! :map +misc-applications-eyecandy-map
        "f" #'+fireplace)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-eyecandy-map
      "f" "Fireplace"))
  :config
  (map! :map fireplace-mode-map
        "<f6>" #'cae-cheatsheets-fireplace
        :ng "q" #'+fireplace-quit))

(use-package! flames-of-freedom
  :defer t :init
  (defvar +flames-of-freedom-workspace-name "*flames-of-freedom*")
  (defvar +flames-of-freedom--old-wconf nil)
  (map! :map +misc-applications-eyecandy-map
        "F" #'+flames-of-freedom)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-eyecandy-map
      "F" "Flames of Freedom")))

(use-package! snow
  :defer t :init
  (defvar +snow-workspace-name "*snow*")
  (defvar +snow--old-wconf nil)
  (map! :map +misc-applications-eyecandy-map
        "s" #'+snow)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-eyecandy-map
      "s" "Snow")))

(use-package! zone
  :defer t :defer-incrementally t :init
  (map! :map +misc-applications-eyecandy-map
        "z" #'zone-choose)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-eyecandy-map
      "z" "Zone"))
  ;; For `zone-matrix'.
  (defvar tabbar-mode nil)
  (autoload 'zone-matrix "zone-matrix")
  (advice-add #'zone-matrix :before
              (cae-defun +zone-matrix-setup-buffer-appearance ()
                (setq-local nobreak-char-display nil)
                (+misc-applications-hide-cursor-h)
                (face-remap-add-relative 'default :background "black")))

  ;; Do not zone in a popup window. Also, do not show other windows when zoning.
  ;; Quit out of the minibuffer if necessary before zoning.q
  (defadvice! +zone-switch-to-root-window-a (oldfun &rest args)
    :around #'zone
    (let ((zone-fn (lambda ()
                     (let ((wconf (current-window-configuration))
                           (tabbar-state (frame-parameter nil 'tab-bar-lines)))
                       (select-window (car (doom-visible-windows)))
                       (let ((ignore-window-parameters t))
                         (delete-other-windows))
                       (set-frame-parameter nil 'tab-bar-lines 0)
                       (apply oldfun args)
                       (set-frame-parameter nil 'tab-bar-lines tabbar-state)
                       (set-window-configuration wconf)))))
      (run-at-time (+ (* 0.01 (minibuffer-depth)) 0.01) nil zone-fn))
    (dotimes (i (minibuffer-depth))
      (run-at-time (* 0.01 i) nil #'minibuffer-keyboard-quit)))
  :config
  ;; remove not interesting programs
  (setq zone-programs [zone-nyan
                       zone-rainbow
                       zone-matrix
                       zone-tmux-clock
                       zone-pgm-md5
                       zone-pgm-sl
                       zone-pgm-jitter
                       zone-pgm-putz-with-case
                       zone-pgm-dissolve
                       ;; zone-pgm-explode
                       zone-pgm-whack-chars
                       zone-pgm-rotate
                       zone-pgm-rotate-LR-lockstep
                       zone-pgm-rotate-RL-lockstep
                       zone-pgm-rotate-LR-variable
                       zone-pgm-rotate-RL-variable
                       zone-pgm-drip
                       ;; zone-pgm-drip-fretfully
                       ;; zone-pgm-five-oclock-swan-dive
                       ;; zone-pgm-martini-swan-dive
                       zone-pgm-rat-race
                       zone-pgm-paragraph-spaz])

  (after! zone-matrix
    (setq zmx-unicode-mode t))

  (unless (bound-and-true-p exwm--connection)
    (zone-when-idle (* 5 60))))

;; Here's another Zone that says positive words together with their definitions.
;; But it requires `wordnet' to be installed and also an internet connection.
;; https://xenodium.com/emacs-zones-to-lift-you-up/

;; This is another Zone referencing the Matrix movie but it's kind of boring. It
;; just says some green text slowly one line at a time.
;; https://github.com/vreeze/zone-matrix-wake-up


;;; Music

(use-package! somafm
  :defer t :init
  (map! :map +misc-applications-music-map
        "s" #'+somafm)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-music-map
      "s" "SomaFM"))
  (add-hook 'somafm-mode-hook #'+misc-applications-hide-cursor-h)
  (when (modulep! :editor evil)
    (after! evil-snipe
      (cl-pushnew #'somafm-mode evil-snipe-disabled-modes)))
  :config
  (map! :map somafm-mode-map
        "<f6>" #'cae-cheatsheets-somafm
        :n "<f6>" #'cae-cheatsheets-evil-somafm
        :ng "q" #'quit-window
        :n "l" #'somafm--sort
        :n "s" #'somafm--stop))

(use-package! mpc
  :defer t :init
  (defvar +mpc-workspace-name "*mpc*")
  (defvar +mpc--old-wconf nil)
  (when (modulep! :editor evil)
    (after! evil-snipe
      (cl-pushnew #'mpc-mode evil-snipe-disabled-modes)))
  (map! :map +misc-applications-music-map
        "c" #'+mpc)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-music-map
      "c" "MPC"))
  :config
  (setq mpc-host "localhost"
        mpc-mpd-music-directory "/mnt/unindexed-music/")
  (map! :map mpc-mode-map
        :ng "q" #'+mpc-quit
        "<f6>" #'cae-cheatsheets-mpc
        :n "<f6>" #'cae-cheatsheets-evil-mpc
        :n "C-j" #'evil-collection-mpc-move-down
        :n "C-k" #'evil-collection-mpc-move-up
        :n "e" #'+mpc-other-window
        :n "w" #'+mpc-other-window-previous
        :n "RET" #'mpc-play-at-point
        :n "o" #'mpc-goto-playing-song
        :n "C" #'mpc-stop
        :n "i" #'evilem-motion-next-line
        :n "u" #'evilem-motion-previous-line))

(use-package! empv
  :defer t :init
  (map! :map +misc-applications-music-map
        "m" (cae-oneshot-keymap empv-map empv))
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-music-map
      "m" "MPV"))
  :config
  (map! :map +misc-applications-music-map
        "m" empv-map)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-music-map
      "m" "MPV"))
  (map! :map empv-map
        "P" #'empv-youtube-playlist
        "T" #'empv-youtube-tabulated
        "'" #'empv-youtube-tabulated-last-results)
  (setq empv-youtube-use-tabulated-results nil)
  (add-hook 'empv-youtube-results-mode-hook
            (cae-defun +empv-youtube-results-h ()
              (setq tabulated-list-padding 0)
              (setq-local tabulated-list-format
                          `[("Thumbnail" 20 nil)
                            ("Title" ,(- (window-width) 20 10 10 (* tabulated-list-padding 2)) t)
                            ("Length"  10 t)
                            ("Views" 10 t)])))
  (require 'elfeed-tube)
  (add-to-list 'empv-mpv-args "--ytdl-format=best")
  (add-to-list 'empv-mpv-args "--save-position-on-quit")
  (setq empv-reset-playback-speed-on-quit t
        empv-base-directory "/mnt/hdd/music"
        empv-audio-dir "/mnt/hdd/music"
        empv-video-dir "/mnt/hdd/videos")
  (setq empv-invidious-instance
        (concat "https://"
                (aio-wait-for (elfeed-tube--get-invidious-url))
                "/api/v1"))
  (after! embark
    (empv-embark-initialize-extra-actions)))

(use-package! emms
  :defer t :init
  (defvar +emms-music-dir +misc-applications-music-dir
    "The directory where your music library is located.")
  (add-to-list 'safe-local-variable-directories +emms-music-dir)
  (defvar +emms-workspace-name "*emms*")
  (defvar +emms--old-wconf nil)
  (setq emms-directory (concat doom-data-dir "emms/")
        emms-cache-file (concat emms-directory "cache"))
  (after! dired
    (map! :map dired-mode-map
          :ng "E" #'emms-play-dired))
  (map! :map +misc-applications-music-map
        "e" #'+emms)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-music-map
      "e" "EMMS"))
  :config
  (map! :map emms-browser-mode-map
        :ng "q" #'+emms-quit
        :ng "a" #'+emms-quick-access)
  (emms-all)
  (emms-default-players)
  (setq emms-repeat-playlist t
        emms-repeat-track t
        emms-random-playlist t
        emms-later-do-interval 0.5
        emms-later-do-batch 10
        emms-source-file-directory-tree-function #'emms-source-file-directory-tree-find
        emms-source-file-default-directory (expand-file-name "Artists" +emms-music-dir)
        emms-player-mpd-music-directory (expand-file-name "Artists" +emms-music-dir)
        emms-info-native--max-num-vorbis-comments 48000
        emms-browser-covers #'emms-browser-cache-thumbnail-async
        emms-info-functions '(emms-info-exiftool)
        emms-browser-switch-to-playlist-on-add t)
  (map! :map emms-playlist-mode-map
        :localleader
        "l" #'emms-toggle-repeat-playlist
        "p" #'emms-insert-playlist
        "i" #'emms-insert-file
        "t" #'emms-toggle-repeat-track
        "s" #'emms-playlist-save
        "m" #'emms-shuffle)
  (add-to-list 'emms-track-initialize-functions 'emms-info-initialize-track)
  (add-hook 'emms-browser-mode-hook #'doom-mark-buffer-as-real-h)
  (add-hook 'emms-playlist-mode-hook #'doom-mark-buffer-as-real-h)

  (setq emms-track-description-function 'cae-emms-track-description))

(use-package! helm-emms
  :when (or (modulep! :private helm)
            (modulep! :completion helm))
  :defer t :init
  (map! :map +misc-applications-music-map
        "h" #'helm-emms)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-music-map
      "h" "Helm EMMS"))
  :config
  (setq helm-emms-dired-directories (list (expand-file-name +emms-music-dir))
        helm-emms-use-track-description-function t
        helm-emms-directory-files-recursive-fn #'helm-emms-walk-directory-with-find
        helm-emms-default-sources '(helm-source-emms-files
                                    helm-source-emms-streams
                                    helm-source-emms-dired)))
