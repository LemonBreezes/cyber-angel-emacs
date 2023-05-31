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

;;; Universal argument

(defconst home-row-numbers-qwerty
  (cae-keyboard-remap '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;)))

(home-row-numbers)

(map! :map universal-argument-map
      "l" #'cae-keyboard-insert-current-prefix)

;;; Distinguishing dual-purpose keycodes
