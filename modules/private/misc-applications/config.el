;;; private/misc-applications/config.el -*- lexical-binding: t; -*-

(defvar +misc-applications-lisp-files nil)
(defvar +misc-applications-map (make-sparse-keymap))
(define-prefix-command '+misc-applications-map)
(defvar +misc-applications-prefix "a")
;;map! :leader
;;:desc "+misc-applications" +misc-applications-prefix
;;defun +misc-applications-prefix ()
;;(interactive)

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
        "+ednc"
        "+proced"))

;;(dolist (file +misc-applications-lisp-files)
;;  (load! file))
;;(map! "<f13>" #'+misc-applications-map)
;;(setq unread-command-events
;;      (list 'f13))
