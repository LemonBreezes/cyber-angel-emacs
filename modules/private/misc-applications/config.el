;;; private/misc-applications/config.el -*- lexical-binding: t; -*-

(defvar +misc-applications-lisp-files nil)
(defvar +misc-applications-map (make-sparse-keymap))
(defvar +misc-applications-prefix "a")
(defvar +misc-applications-lookup-prefix (concat +misc-applications-prefix "L"))
(defvar +misc-applications-games-prefix (concat +misc-applications-prefix "g"))
(defvar +misc-applications-eyecandy-prefix (concat +misc-applications-prefix "e"))
(defvar +misc-applications-lists-prefix (concat +misc-applications-prefix "l"))
(defvar doom-picture-dir "~/Pictures/")
(map! :leader :prefix (+misc-applications-prefix . "misc-applications"))
(map! :leader :prefix (+misc-applications-lookup-prefix . "lookup"))
(map! :leader :prefix (+misc-applications-lists-prefix . "lists"))
(map! :leader :prefix (+misc-applications-games-prefix . "games"))
(map! :leader :prefix (+misc-applications-eyecandy-prefix . "eyecandy"))

;; "a" is bound to `emabark-act', which I use `<f8>' for instead.
(keymap-unset doom-leader-map +misc-applications-prefix t)
(map! :leader
      :prefix +misc-applications-prefix
      "1" #'mpc)
(map! :leader
      :prefix +misc-applications-lists-prefix
      :desc "emacs packages" "p" #'list-packages
      :desc "emacs processes" "e" #'list-processes
      :desc "emacs timers" "T" #'list-timers)

(after! timer-list
  (map! :map timer-list-mode-map
        "<f6>" #'+timer-list-hydra/body))
(map! :map process-menu-mode-map
      "<f6>" #'+list-processes-hydra/body)

;; TODO Turn these into module flags and separate the binding of their prefixes
;; out into `doom-after-init-hook' so that the user can modify them.
(setq +misc-applications-lisp-files
      '("+alarm-clock"
        ;;"+aurel"
        "+disk-usage"
        ;;"+elcord"
        "+elfeed"
        ;;"+enime"
        ;;"+exercism"
        ;;"+frameshot"
        "+leetcode"
        ;;"+mentor"
        "+my-repo-pins"
        ;;"+picpocket"
        "+pulseaudio-control"
        "+somafm"
        "+try"
        ;;"+vuiet"
        ;;"+webpaste" ; I'm using 0x0.st instead.
        "+wttrin"

        ;; Lists
        "+daemons"
        ;;"+helm-linux-disks"
        ;;"+helm-system-packages"
        "+paradox"
        "+trashed"

        ;; Lookup
        "+devdocs"
        "+know-your-http-well"
        "+shortdoc"
        "+tldr"
        "+x86-lookup"

        ;; Games
        "+bubbles"
        "+doctor"
        "+dunnet"
        "+speed-type"
        "+snake"
        "+tetris"

        "+fireplace"
        "+flames-of-freedom"
        "+snow"
        "+zone"

        ;; Emacs OS
        "+ednc"                         ;I should write a Hydra for this package
                                        ;once I start using it.
        "+proced"))

(dolist (file +misc-applications-lisp-files)
  (load! file))
