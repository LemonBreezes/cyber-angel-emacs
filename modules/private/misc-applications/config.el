;;; private/misc-applications/config.el -*- lexical-binding: t; -*-

(defvar +misc-applications-lisp-files nil)
(defvar +misc-applications-map (make-sparse-keymap))
(defvar +misc-applications-prefix "a")
(defvar +misc-applications-lookup-prefix (concat +misc-applications-prefix "l"))
(defvar +misc-applications-games-prefix (concat +misc-applications-prefix "g"))
(defvar +misc-applications-eyecandy-prefix (concat +misc-applications-prefix "e"))
(defvar +misc-applications-system-prefix (concat +misc-applications-prefix "s"))
(defvar +misc-applications-external-apps-prefix (concat +misc-applications-prefix "x"))
(defvar +misc-applications-standalone-apps-prefix (concat +misc-applications-prefix "t"))
(defvar +misc-applications-insert-prefix (concat +misc-applications-prefix "i"))
(defvar doom-picture-dir "~/Pictures/")
(map! :leader :prefix (+misc-applications-prefix . "misc-applications"))
(map! :leader :prefix (+misc-applications-lookup-prefix . "lookup"))
(map! :leader :prefix (+misc-applications-system-prefix . "system"))
(map! :leader :prefix (+misc-applications-games-prefix . "games"))
(map! :leader :prefix (+misc-applications-eyecandy-prefix . "eyecandy"))
(map! :leader :prefix (+misc-applications-external-apps-prefix . "external apps"))
(map! :leader :prefix (+misc-applications-standalone-apps-prefix . "standalone apps"))
(map! :leader :prefix (+misc-applications-insert-prefix . "insert"))

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

(setq +misc-applications-lisp-files
      '(;; Standalone apps
        "+alarm-clock"
        "+elfeed"
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

        ;; Insert
        "+helm-rage"
        "+lorem-ipsum"
        "+password-generator"

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
