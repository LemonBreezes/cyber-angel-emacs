;;; private/misc-applications/config.el -*- lexical-binding: t; -*-

(defvar +misc-applications-map (make-sparse-keymap))
(defvar +misc-applications-prefix "j")
(defvar +misc-applications-lookup-prefix "j l")
(defvar +misc-applications-games-prefix "j g")
(defvar +misc-applications-eyecandy-prefix "j e")
(defvar doom-picture-dir "~/Pictures/")
(map! :leader :prefix (+misc-applications-prefix . "misc-applications"))
(map! :leader :prefix (+misc-applications-lookup-prefix . "lookup"))
(map! :leader :prefix (+misc-applications-games-prefix . "games"))
(map! :leader :prefix (+misc-applications-eyecandy-prefix . "eyecandy"))

(map! :leader
      :prefix +misc-applications-prefix
      "1" #'mpc)

(defvar +misc-applications--lisp-files
  '("+alarm-clock"
    "+aurel"
    "+daemons"
    "+disk-usage"
    ;; "+elcord"
    "+enime"
    "+exercism"
    "+forecast"
    ;; "+frameshot"
    "+helm-linux-disks"
    "+helm-system-packages"
    ;; "+mentor"
    "+my-repo-pins"
    ;; "+picpocket"
    "+pulseaudio-control"
    "+somafm"
    "+trashed"
    "+try"
    "+vuiet"
    "+ytel"
    "+webpaste"
    "+elfeed"

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

    ;; TODO Eyecandy
    ;; zone
    ;; nyan zone
    ;; also encryption zone
    "+fireplace"
    "+flames-of-freedom"
    "+snow"
    "+zone"
    ))

(dolist (file +misc-applications--lisp-files)
  (load! file))
