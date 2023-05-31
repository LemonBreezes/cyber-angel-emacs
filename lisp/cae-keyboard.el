;;; ~/.doom.d/lisp/cae-keyboard.el -*- lexical-binding: t; -*-

;;; Remap keys

;; These are the orbits of the alphabet under the permutation of the keys
;; created by the keyboard layout. Because my keyboard layout is so weird, some
;; of these remappings involve non-printable characters. I have replaced those
;; in the list below with a null byte (0x00) and terminated the list.
(defvar cae-keyboard-orbits
  '((?w ?b ?j ?o ?y ?v ?k ?t ?g ?s ?a ?r ?f ?n ?m ?c ?u)
    (?W ?B ?J ?O ?Y ?V ?K ?T ?G ?S ?A ?R ?F ?N ?M ?C ?U)
    (?# ?3 ?\; ?h ?d ?e ?p ?\' ?_ ?\} ?\0)
    (?: ?H ?D ?E ?P ?% ?^ ?6 ?$ ?4 ?= ?\0)
    (?i ?l) (?I ?L)
    (?x ?,) (?X ?<)
    (?0 ?\))
    (?1 ?!)
    (?2 ?@)
    (?5 ?+ ?\0)
    (?7 ?&)
    (?8 ?*)
    (?9 ?\()
    (?z) (?Z) (?-) (?.) (?>) (?q) (?Q)
    ))


(define-key key-translation-map (cae-keyboard-kbd "C-x t" "0") (kbd "C-x t 0"))
(define-key key-translation-map (cae-keyboard-kbd "C-x t" "1") (kbd "C-x t 1"))
(define-key key-translation-map (cae-keyboard-kbd "C-x t" "2") (kbd "C-x t 2"))
(define-key key-translation-map (cae-keyboard-kbd "C-c w" "0") (kbd "C-c w 0"))
(define-key key-translation-map (cae-keyboard-kbd "C-c w" "1") (kbd "C-c w 1"))
(define-key key-translation-map (cae-keyboard-kbd "C-c w" "2") (kbd "C-c w 2"))
(define-key key-translation-map (cae-keyboard-kbd "C-c w" "3") (kbd "C-c w 3"))
(define-key key-translation-map (cae-keyboard-kbd "C-c w" "4") (kbd "C-c w 4"))
(define-key key-translation-map (cae-keyboard-kbd "C-c w" "5") (kbd "C-c w 5"))
(define-key key-translation-map (cae-keyboard-kbd "C-c w" "6") (kbd "C-c w 6"))
(define-key key-translation-map (cae-keyboard-kbd "C-c w" "7") (kbd "C-c w 7"))
(define-key key-translation-map (cae-keyboard-kbd "C-c w" "8") (kbd "C-c w 8"))
(define-key key-translation-map (cae-keyboard-kbd "C-c w" "9") (kbd "C-c w 9"))

;; other window prefix
(define-key key-translation-map (kbd "C-x 4 C-x 4") (kbd "C-x 4 4"))
(define-key key-translation-map (kbd "C-x 4 C-x 1") (kbd "C-x 4 1"))
(define-key key-translation-map (kbd "C-x 4 C-x 0") (kbd "C-x 4 0"))
(define-key key-translation-map (kbd "C-x 4 C-x C-b") (kbd "C-x 4 b"))
(define-key key-translation-map (kbd "C-x 4 C-x C-f") (kbd "C-x 4 f"))
(define-key key-translation-map (kbd "C-x 4 C-x C-.") (kbd "C-x 4 ."))
(define-key key-translation-map (kbd "C-x 4 C-x C-i") (kbd "C-x 4 i"))
(define-key key-translation-map (kbd "C-x 4 C-x C-a") (kbd "C-x 4 a"))
(define-key key-translation-map (kbd "C-x 4 C-x C-c") (kbd "C-x 4 c"))
(define-key key-translation-map (kbd "C-x 4 C-x C-d") (kbd "C-x 4 d"))
(define-key key-translation-map (kbd "C-x 4 C-x C-m") (kbd "C-x 4 m"))
(define-key key-translation-map (kbd "C-x 4 C-x C-p") (kbd "C-x 4 p"))
(define-key key-translation-map (kbd "C-x 4 C-x C-r") (kbd "C-x 4 r"))
(define-key key-translation-map (kbd "C-x 4 C-x C-o") (kbd "C-x 4 j"))

;; other frame prefix
(define-key key-translation-map (kbd "C-x 5 C-x 5") (kbd "C-x 5 5"))

;;; Universal argument

(defconst home-row-numbers-qwerty
  (cae-keyboard-remap '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;)))

(home-row-numbers)

(map! :map universal-argument-map
      "l" #'cae-keyboard-insert-current-prefix)

;;; Distinguishing dual-purpose keycodes

;; Look up the "TAB" key in the current major mode. If it is bound and "<tab>"
;; is not bound, then bind it also to <tab>. This way, our global <tab> key
;; does not bind "C-i" everywhere.
(when (display-graphic-p)
  (defun cae-keyboard-conditionally-remap-C-i ()
    (run-at-time
     0.0 nil
     (lambda ()
       (let ((tab-command (lookup-key (current-local-map) (kbd "<tab>")))
             (C-i-command (lookup-key (current-local-map) (kbd "C-i"))))
         (when (and C-i-command
                    (or (not tab-command)
                        (eq tab-command 'self-insert-command))
                    (not (eq tab-command C-i-command)))
           (define-key (current-local-map) (kbd "<tab>") C-i-command))))))

  (add-hook 'after-change-major-mode-hook #'cae-keyboard-conditionally-remap-C-i)

  (map! "<tab>" #'indent-for-tab-command
        "C-i" #'doom/dumb-indent
        "C-S-i" #'doom/dumb-dedent))
