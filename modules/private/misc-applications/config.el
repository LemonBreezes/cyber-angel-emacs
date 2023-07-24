;;; private/misc-applications/config.el -*- lexical-binding: t; -*-

(defvar +misc-applications-lisp-files nil)
(defvar +misc-applications-map (make-sparse-keymap))
(defvar +misc-applications-prefix "a")
(defvar +misc-applications-lookup-prefix (concat +misc-applications-prefix "l"))
(defvar +misc-applications-games-prefix (concat +misc-applications-prefix "g"))
(defvar +misc-applications-eyecandy-prefix (concat +misc-applications-prefix "e"))
(defvar +misc-applications-system-prefix (concat +misc-applications-prefix "s"))
(defvar +misc-applications-external-prefix (concat +misc-applications-prefix "x"))
(defvar +misc-applications-standalone-prefix (concat +misc-applications-prefix "t"))
(defvar doom-picture-dir "~/Pictures/")
(map! :leader :prefix (+misc-applications-prefix . "misc-applications"))
(map! :leader :prefix (+misc-applications-lookup-prefix . "lookup"))
(map! :leader :prefix (+misc-applications-system-prefix . "system"))
(map! :leader :prefix (+misc-applications-games-prefix . "games"))
(map! :leader :prefix (+misc-applications-eyecandy-prefix . "eyecandy"))
(map! :leader :prefix (+misc-applications-external-prefix . "external"))
(map! :leader :prefix (+misc-applications-standalone-prefix . "standalone"))

;; "a" is bound to `emabark-act', which I use `<f8>' for instead.
(keymap-unset doom-leader-map +misc-applications-prefix t)
(map! :leader
      :prefix +misc-applications-prefix
      "1" #'mpc)
(map! :leader
      :prefix  +misc-applications-system-prefix
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
      '(;; Standalone apps
        "+alarm-clock"
        "+elfeed"
        "+ement"                        ; TODO
        "+my-repo-pins"

        ;; Use external APIs or apps
        "+elcord"
        "+leetcode"
        "+somafm"
        "+wttrin"

        ;; System
        "+daemons"
        "+disk-usage"
        "+helm-linux-disks"
        "+helm-system-packages"
        "+paradox"
        "+pulseaudio-control"
        "+trashed"

        ;; Insert text
        ;; TODO Add `helm-rage' and `lorem-ipsum' to this list.

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
        ;; TODO Add `2048', `klondike', and `chess' to this list.

        ;; Eye candy
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
