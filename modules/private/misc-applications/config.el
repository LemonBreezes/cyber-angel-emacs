;;; private/misc-applications/config.el -*- lexical-binding: t; -*-

(defvar +misc-applications-lisp-files nil)
(defvar +misc-applications-map (make-sparse-keymap))
(define-prefix-command '+misc-applications-map)
(defvar +misc-applications-prefix "a")
(map! :leader
      :desc "+misc-applications" +misc-applications-prefix
      (defun +misc-applications-prefix ()
        (interactive)
        ;; "a" is bound to `emabark-act', which I use `<f8>' for instead.
        (keymap-unset doom-leader-map +misc-applications-prefix t)
        (defvar +misc-applications-lookup-prefix "l")
        (defvar +misc-applications-games-prefix "g")
        (defvar +misc-applications-eyecandy-prefix "e")
        (defvar +misc-applications-system-prefix "s")
        (defvar +misc-applications-external-apps-prefix "x")
        (defvar +misc-applications-standalone-apps-prefix "t")
        (defvar +misc-applications-insert-prefix "i")
        (defvar +misc-applications-lookup-map (make-sparse-keymap))
        (defvar +misc-applications-games-map (make-sparse-keymap))
        (defvar +misc-applications-eyecandy-map (make-sparse-keymap))
        (defvar +misc-applications-system-map (make-sparse-keymap))
        (defvar +misc-applications-external-apps-map (make-sparse-keymap))
        (defvar +misc-applications-standalone-apps-map (make-sparse-keymap))
        (defvar +misc-applications-insert-map (make-sparse-keymap))
        (define-prefix-command '+misc-applications-lookup-map)
        (define-prefix-command '+misc-applications-games-map)
        (define-prefix-command '+misc-applications-eyecandy-map)
        (define-prefix-command '+misc-applications-system-map)
        (define-prefix-command '+misc-applications-external-apps-map)
        (define-prefix-command '+misc-applications-standalone-apps-map)
        (define-prefix-command '+misc-applications-insert-map)
        (map! :leader :desc "misc-applications" +misc-applications-prefix #'+misc-applications-map)
        (define-key +misc-applications-map (kbd +misc-applications-lookup-prefix)
          '+misc-applications-lookup-map)
        (define-key +misc-applications-map (kbd +misc-applications-games-prefix)
          '+misc-applications-games-map)
        (define-key +misc-applications-map (kbd +misc-applications-eyecandy-prefix)
          '+misc-applications-eyecandy-map)
        (define-key +misc-applications-map (kbd +misc-applications-system-prefix)
          '+misc-applications-system-map)
        (define-key +misc-applications-map (kbd +misc-applications-external-apps-prefix)
          '+misc-applications-external-apps-map)
        (define-key +misc-applications-map (kbd +misc-applications-standalone-apps-prefix)
          '+misc-applications-standalone-apps-map)
        (define-key +misc-applications-map (kbd +misc-applications-insert-prefix)
          '+misc-applications-insert-map)
        (after! which-key
          (which-key-add-keymap-based-replacements '+misc-applications-map
            "l" "lookup"
            "s" "system"
            "g" "games"
            "e" "eyecandy"
            "x" "external apps"
            "t" "standalone apps"
            "i" "insert"))

        (map! :map +misc-applications-system-map
              "p" #'list-packages
              "e" #'list-processes
              "T" #'list-timers)
        (after! which-key
          (which-key-add-keymap-based-replacements '+misc-applications-system-map
            "p" "packages"
            "e" "processes"
            "T" "timers"))

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

                ;; TODO Music
                ;; mpc
                ;; somafm

                ;; Insert
                "+helm-rage"
                "+lorem-ipsum"
                "+password-generator"
                "+uuidgen"
                "+decide"

                ;; TODO Quotes

                ;; Lookup
                ;;"+devdocs"
                ;;"+know-your-http-well"
                ;;"+shortdoc"
                ;;"+tldr"
                ;;"+x86-lookup"

                ;; Games
                ;;"+bubbles"
                ;;"+doctor"
                ;;"+dunnet"
                ;;"+speed-type"
                ;;"+snake"
                ;;"+tetris"
                ;; TODO Add `2048', `klondike', and `chess' to this list.

                ;; Eye candy
                ;;"+fireplace"
                ;;"+flames-of-freedom"
                ;;"+snow"
                ;;"+zone"

                ;; Emacs OS
                ;;"+ednc"                 ;I should write a Hydra for this package
                ;;                        ;once I start using it.
                ;;"+proced"
                ))

        (dolist (file +misc-applications-lisp-files)
          (load! file))
        (map! "<f13>" #'+misc-applications-map)
        (setq unread-command-events
              (list 'f13))))
