;;; ~/.doom.d/lisp/cae-keyboard.el -*- lexical-binding: t; -*-

;;; Remap keys

;; TODO Use key translation map instead of remapping keys
;; Do this for the C-x t and C-c w prefix keys
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Translation-Map.html

(define-key key-translation-map (kbd "C-x t )") (kbd "C-x t 0"))
(define-key key-translation-map (kbd "C-x t !") (kbd "C-x t !"))
(define-key key-translation-map (kbd "C-x t @") (kbd "C-x t 2"))
(define-key key-translation-map (kbd "C-c w !") (kbd "C-x t 1"))
(define-key key-translation-map (kbd "C-c w @") (kbd "C-x t 2"))
(define-key key-translation-map (kbd "C-c w ;") (kbd "C-x t 3"))
(define-key key-translation-map (kbd "C-c w =") (kbd "C-x t 4"))
(define-key key-translation-map (kbd "C-c w +") (kbd "C-x t 5"))
(define-key key-translation-map (kbd "C-c w $") (kbd "C-x t 6"))
(define-key key-translation-map (kbd "C-c w &") (kbd "C-x t 7"))
(define-key key-translation-map (kbd "C-c w *") (kbd "C-x t 8"))
(define-key key-translation-map (kbd "C-c w (") (kbd "C-x t 9"))
(define-key key-translation-map (kbd "C-c w )") (kbd "C-x t 0"))

;;; Universal argument

(defvar home-row-numbers-qwerty
  '(?r ?a ?e ?n ?s ?d ?o ?t ?i ?h)
  "list of the qwerty home row keys")

(defvar home-row-numbers-qwerty-numpad
  '(?c ?x ?\. ?o ?t ?i ?w ?l ?y ?\ )
  "keys forming a numpad under the right hand in qwerty")

(defvar home-row-numbers-norm
  '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)
  "list of the numbers on the keyboard in normal order")

(defvar home-row-numbers nil)

(home-row-numbers)

(map! :map universal-argument-map
      "l" #'cae-keyboard-insert-current-prefix)
