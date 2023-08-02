;;; private/misc-applications/config.el -*- lexical-binding: t; -*-

;;; Preamble

(defvar +misc-applications-lisp-files nil)
(defvar +misc-applications-map (make-sparse-keymap))
(define-prefix-command '+misc-applications-map)
(defvar +misc-applications-prefix "a")

;; "a" is bound to `emabark-act', which I use `<f8>' for instead.
(keymap-unset doom-leader-map +misc-applications-prefix t)

(map! :leader :desc "misc-applications" +misc-applications-prefix #'+misc-applications-map)
(defvar application-types
  '(("lookup" "l")
    ("games" "g")
    ("eyecandy" "e")
    ("system" "s")
    ("external-apps" "x")
    ("standalone-apps" "t")
    ("insert" "i")
    ("emacs-os" "o")))

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
      "p" #'list-packages
      "E" #'list-processes
      "T" #'list-timers)
(after! which-key
  (which-key-add-keymap-based-replacements '+misc-applications-system-map
    "p" "packages"
    "E" "emacs processes"
    "T" "timers"))

(after! timer-list
  (map! :map timer-list-mode-map
        "<f6>" #'+timer-list-hydra/body))
(map! :map process-menu-mode-map
      "<f6>" #'+list-processes-hydra/body)


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
      "a a" "set alarm"
      "a A" "list alarms"))
  :config
  (map! :leader
        :prefix +misc-applications-prefix
        (:prefix ("a" . "alarms")
         "k" #'alarm-clock-kill))
  (setq alarm-clock-cache-file
        (expand-file-name "alarm-clock.cache" doom-cache-dir))
  (alarm-clock--turn-autosave-on))

(use-package! elfeed
  :when (modulep! :app rss)
  :defer t :init
  (map! :map +misc-applications-standalone-apps-map
        "r" #'=rss)
  :config
  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'star))
  (push elfeed-db-directory recentf-exclude)
  (map! :map elfeed-show-mode-map
        "?" #'describe-mode
        :map elfeed-search-mode-map
        "?" #'describe-mode
        "q" #'+elfeed-quit)
  (when (modulep! :ui hydra)
    (map! :map elfeed-search-mode-map
          "<f6>" #'cae-elfeed-hydra/body
          ;; Elfeed maps `h' to `describe-mode', which is not as good.
          "h" #'cae-elfeed-hydra/body))

  (use-package! elfeed-tube
    :config
    (elfeed-tube-setup)
    (map! :map elfeed-show-mode-map
          "F" #'elfeed-tube-fetch
          [remap save-buffer] #'elfeed-tube-save
          :map elfeed-search-mode-map
          "F" #'elfeed-tube-fetch
          [remap save-buffer] #'elfeed-tube-save)))

(use-package! my-repo-pins
  :defer t :init
  (map! :map +misc-applications-standalone-apps-map
        "j" #'my-repo-pins)
  :config
  (make-directory "~/code-root" t)
  (setq my-repo-pins-code-root "~/code-root"))


;;; External apps

(use-package! elcord
  :when (and (cae-display-graphic-p)
             (not (or (memq system-type '(cygwin windows-nt ms-dos))
                      (getenv "WSL_DISTRO_NAME")))
             ;; I only use this on my desktop machine when EXWM is running.
             (modulep! :private exwm))
  :defer t :hook (doom-first-file . elcord-mode)
  :init
  (defadvice! +elcord--buffer-boring-p-a (buffer-name)
    :before-until #'elcord--buffer-boring-p
    (or (string-match-p "^\\*\\(doom\\|Messages\\|scratch\\|dashboard\\|elfeed\\|vterm\\|eshell\\|terminal\\|magit\\|help\\|Compile-Log\\|lsp\\|treemacs\\|\\*\\)" buffer-name)
        (parent-mode-is-derived-p (buffer-local-value 'major-mode (get-buffer buffer-name))
                                  'exwm-mode)
        (parent-mode-is-derived-p (buffer-local-value 'major-mode (get-buffer buffer-name))
                                  'dired-mode)))
  :config
  (setq elcord-quiet t
        elcord-use-major-mode-as-main-icon t
        elcord-refresh-rate 10
        elcord-idle-timer 300
        elcord-idle-message "Going for a walk...")
  (autoload 'parent-mode-is-derived-p "parent-mode"))

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
        "<f6>" #'+leetcode-problems-hydra/body
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

(use-package! somafm
  :defer t :init
  (map! :map +misc-applications-external-apps-map
        "s" #'+somafm)
  :config
  (map! :map somafm-mode-map
        "<f6>" #'+somafm-hydra/body))

(use-package! wttrin
  :defer t :init
  (map! :map +misc-applications-external-apps-map
        "w" #'wttrin)
  (advice-add #'wttrin-query :after
              (cae-defun +wttrin-setup-h (&rest _)
                (face-remap-add-relative 'default :family "Iosevka" :height 1.0))))


;;; System

(use-package! daemons
  :when (eq system-type 'gnu/linux)
  :defer t :init
  (map! :map +misc-applications-system-map
        "u" #'daemons)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-system-map
      "u" "services"))
  :config
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
        "<f6>" #'+disk-usage-hydra/body))

(use-package! helm-linux-disks
  :when (and (eq system-type 'gnu/linux)
             (or (modulep! :private helm)
                 (modulep! :completion helm)))
  :defer t :init
  (map! :map +misc-applications-system-map
        "D" #'helm-linux-disks)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-system-map
      "D" "disks")))

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

(use-package! paradox
  :defer t :init
  (map! :map +misc-applications-system-map
        "e" #'paradox-list-packages)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-system-map
      "e" "emacs packages"))
  :config
  (paradox-enable)
  (map! :map paradox-menu-mode-map
        "<f6>" #'cae-paradox-menu-quick-help))

(use-package! pulseaudio-control
  :when (and (eq system-type 'gnu/linux)
             (executable-find "pactl"(and (eq system-type 'gnu/linux)
                                          (executable-find "pactl"))))
  :defer t :init
  (map! :map ctl-x-map
        "/" (cmd!
             ;; Lazy load `pulseaudio-control'.
             (pulseaudio-control-default-keybindings)
             (setq unread-command-events (list ?\C-x ?/))
             (cae-which-key-show-map 'pulseaudio-control-map)))
  :config
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
  (add-hook 'trashed-mode-hook #'+trashed-hide-cursor-h)
  (add-hook 'trashed-mode-hook #'doom-mark-buffer-as-real-h)
  :config
  (map! :map trashed-mode-map
        "<f6>" #'+trashed-hydra/body))


;;; Insert

(use-package! helm-rage
  :when (or (modulep! :private helm)
            (modulep! :completion helm))
  :defer t :init
  (map! :map +misc-applications-insert-map
        "r" #'helm-rage))

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
      "l l" "lorem-ipsum-insert-list"
      "l p" "lorem-ipsum-insert-paragraphs"
      "l s" "lorem-ipsum-insert-sentences")))

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
      "pc" "custom"
      "ps" "simple"
      "pt" "strong"
      "pn" "numeric"
      "pp" "paranoid"
      "ph" "phonetic")))

(use-package! uuidgen
  :defer t :init
  (map! :map +misc-applications-insert-map
        "u" #'uuidgen))

(use-package! decide
  :defer t :init
  (map! :map +misc-applications-insert-map
        (:prefix "d"
         "?" #'decide-dwim-insert
         "+" #'decide-for-me-likely
         "-" #'decide-for-me-unlikely
         "d" #'decide-roll-dice
         "D" #'decide-roll-2d6
         "3" #'decide-roll-1d3
         "4" #'decide-roll-1d4
         "5" #'decide-roll-1d5
         "6" #'decide-roll-1d6
         "7" #'decide-roll-1d7
         "8" #'decide-roll-1d8
         "9" #'decide-roll-1d9
         "1 0" #'decide-roll-1d10
         "1 2" #'decide-roll-1d12
         "2 0" #'decide-roll-1d20
         "%" #'decide-roll-1d100
         "f" #'decide-roll-fate
         "a" #'decide-roll-1dA
         "A" #'decide-roll-2dA
         "r" #'decide-random-range
         "c" #'decide-random-choice
         "t" #'decide-from-table
         (:prefix "w"
          "4" #'decide-whereto-compass-4
          "6" #'decide-whereto-compass-6
          "8" #'decide-whereto-compass-8
          "1 0" #'decide-whereto-compass-10)
         (:prefix "W"
          "2" #'decide-whereto-relative-2
          "3" #'decide-whereto-relative-3
          "4" #'decide-whereto-relative-4
          "6" 'decide-whereto-relative-6)
         "RET" #'decide-question-return
         "SPC" #'decide-question-space))
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-insert-map
      "d" "decide"
      "dw" "whereto compass"
      "dW" "whereto relative"))
  (dolist (f '(decide-dwim-insert decide-for-me-likely
               decide-for-me-unlikely decide-roll-dice decide-roll-2d6
               decide-roll-1d3 decide-roll-1d4 decide-roll-1d5 decide-roll-1d6
               decide-roll-1d7 decide-roll-1d8 decide-roll-1d9 decide-roll-1d10
               decide-roll-1d12 decide-roll-1d20 decide-roll-1d100
               decide-roll-fate decide-roll-1dA decide-roll-2dA
               decide-random-range decide-random-choice decide-from-table
               decide-whereto-compass-4 decide-whereto-compass-6
               decide-whereto-compass-8 decide-whereto-compass-10
               decide-whereto-relative-2 decide-whereto-relative-3
               decide-whereto-relative-4 decide-whereto-relative-6
               decide-question-return decide-question-space))
    (autoload f "decide" nil t))
  :config
  (map! :prefix +misc-applications-insert-prefix
        "d" #'decide-prefix-map)
  (after! which-key
    (which-key-add-keymap-based-replacements decide-prefix-map
      "w" "whereto compass"
      "W" "whereto relative")))


;;; Games

(use-package! bubbles
  :defer t :init
  (map! :map +misc-applications-games-map
        "b" #'bubbles)
  :config
  (map! :map bubbles-mode-map
        :n "RET" #'bubbles-plop
        "<f6>" #'+bubbles-hydra/body
        "ta" #'bubbles-set-graphics-theme-ascii
        "tb" #'bubbles-set-graphics-theme-balls
        "te" #'bubbles-set-graphics-theme-emacs
        "tc" #'bubbles-set-graphics-theme-circles
        "ts" #'bubbles-set-graphics-theme-squares
        "td" #'bubbles-set-graphics-theme-diamonds
        "dh" #'bubbles-set-game-hard
        "de" #'bubbles-set-game-easy
        "dm" #'bubbles-set-game-medium
        "dd" #'bubbles-set-game-difficult
        "du" #'bubbles-set-game-userdefined
        "S" #'bubbles-save-settings))

(use-package! doctor
  :defer t :init
  (map! :map +misc-applications-games-map
        "D" #'doctor))

(use-package! dunnet
  :defer t :init
  (map! :map +misc-applications-games-map
        "d" #'dunnet))

(use-package! speed-type
  :defer t :init
  (map! :map +misc-applications-games-map
        "T" #'speed-type-text)
  (add-hook 'speed-type-mode-hook #'visual-line-mode)
  :config
  (when (modulep! :private corfu)
    (add-to-list 'corfu-excluded-modes #'speed-type-mode))
  (map! :map speed-type--completed-keymap
        "q" #'kill-this-buffer
        "r" #'speed-type--replay
        "n" #'speed-type--play-next
        :map speed-type-mode-map
        "<f6>" #'+speed-type-hydra/body))

(use-package! snake
  :defer t :init
  (map! :map +misc-applications-games-map
        "s" #'snake)
  :config
  (map! :map snake-mode-map
        "<f6>" #'+snake-hydra/body))

(use-package! tetris
  :defer t :init
  (map! (:map +misc-applications-games-map
         "t" #'tetris)
        (:map +misc-applications-eyecandy-map
         "t" #'autotetris))
  :config
  (map! :map tetris-mode-map
        "<f6>" #'+tetris-hydra/body
        "a" #'autotetris-mode)
  (map! :map autotetris-mode-map
        "a" nil))                       ;Not sure what `autotetris-move' even
                                        ;does to be honest.


;;; Eyecandy

(use-package! fireplace
  :defer t :init
  (map! :map +misc-applications-eyecandy-map
        "f" #'fireplace)
  :config
  (map! :map fireplace-mode-map
        "<f6>" #'+fireplace-hydra/body))

(use-package! flames-of-freedom
  :defer t :init
  (map! :map +misc-applications-eyecandy-map
        "F" #'flames-of-freedom-default))

(use-package! snow
  :defer t :init
  (map! :map +misc-applications-eyecandy-map
        "s" #'snow))

(use-package! zone
  :defer t :defer-incrementally t :init
  (map! :map +misc-applications-eyecandy-map
        "z" #'zone-choose)
  ;; For `zone-matrix'.
  (defvar tabbar-mode nil)
  (autoload 'zone-matrix "zone-matrix")
  (advice-add #'zone-matrix :before
              (cae-defun +zone-matrix-setup-buffer-appearance ()
                (setq-local nobreak-char-display nil)
                (face-remap-add-relative 'default :background "black")))

  ;; Do not zone in a popup window. Also, do not show other windows when zoning.
  ;; Quit out of the minibuffer if necessary before zoning.q
  (defadvice! +zone-switch-to-root-window-a (oldfun &rest args)
    :around #'zone
    (let ((zone-fn (lambda ()
                     (let ((wconf (current-window-configuration))
                           (tabbar-state (frame-parameter nil 'tab-bar-lines)))
                       (select-window (car (doom-visible-windows)))
                       (delete-other-windows)
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


;;; Emacs OS

(use-package! ednc
  :when (cae-display-graphic-p)
  :defer t :init
  (defun +ednc-load-h ()
    (and (require 'dbus nil t)
         (ednc-mode +1)))
  (run-with-idle-timer 1.5 nil #'+ednc-load-h)
  (map! :map +misc-applications-emacs-os-map
        "ns" #'+ednc-show-notifications
        "nd" #'+ednc-dismiss-all-notifications)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-emacs-os-map
      "n" "notifications"
      "ns" "show notifications"
      "nd" "dismiss all notifications"))
  (add-hook 'ednc-notification-presentation-functions #'+ednc-show-notification-in-buffer)
  (defun +ednc-stack-notifications (&optional hide)
    (mapconcat (lambda (notification)
                 (let ((app-name (ednc-notification-app-name notification)))
                   (unless (member app-name hide)
                     (push app-name hide)
                     (ednc-format-notification notification))))
               (ednc-notifications) ""))
  (add-hook 'ednc-notification-presentation-functions
            (lambda (&rest _) (force-mode-line-update t)))
  :config
  (add-to-list 'global-mode-string
               '((:eval (+ednc-stack-notifications))))
  (map! :map ednc-view-mode-map
        "n" #'next-line
        "p" #'previous-line))

(use-package! proced
  :defer t :init
  (map! :map +misc-applications-system-map
        "p" #'proced)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-system-map
      "p" "processes"))
  :config
  (setq proced-enable-color-flag t)
  (map! :map proced-mode-map
        "<f6>" #'+proced-hydra/body))
