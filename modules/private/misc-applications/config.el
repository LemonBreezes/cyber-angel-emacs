;;; private/misc-applications/config.el -*- lexical-binding: t; -*-

(defvar +misc-applications-map (make-sparse-keymap))
(defvar +misc-applications-prefix "j")
(defvar +misc-applications-lookup-prefix "j l")
(defvar +misc-applications-games-prefix "j g")
(defvar doom-picture-dir "~/Pictures/")
(map! :leader :prefix (+misc-applications-prefix . "misc-applications"))
(map! :leader :prefix (+misc-applications-lookup-prefix . "lookup"))
(map! :leader :prefix (+misc-applications-games-prefix . "games"))

(autoload 'emms "emms")
(when (modulep! :private misc-applications)
  (map! :leader
        :prefix +misc-applications-prefix
        "1" #'mpc
        "2" #'emms))

(defvar +misc-applications--lisp-files
  '("+alarm-clock"
    "+aurel"
    "+daemons"
    "+disk-usage"
    "+elcord"
    "+enime"
    "+exercism"
    "+forecast"
    "+frameshot"
    "+helm-linux-disks"
    "+helm-system-packages"
    "+mentor"
    "+my-repo-pins"
    "+picpocket"
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
    "+tetris"))

(dolist (file +misc-applications--lisp-files)
  (load! file))
