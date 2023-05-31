;;; ~/.doom.d/lisp/cae-keyboard.el -*- lexical-binding: t; -*-

;;; Remap keys

;; These are the orbits of the alphabet under the pseudo-permutation of the keys
;; created by the keyboard layout.
(defvar cae-keyboard-orbits
  '((?w ?b ?j ?o ?y ?v ?k ?t ?g ?s ?a ?r ?f ?n ?m ?c ?u) (?W ?B ?J ?O ?Y ?V ?K ?T ?G ?S ?A ?R ?F ?N ?M ?C ?U)
    (?\; ?h ?d ?e ?p ?\' ?_ ?\} ?\0) (?: ?H ?D ?E ?P ?% ?^ ?6 ?$ ?4 ?= ?\0)
    (?i ?l) (?I ?L)
    (?x ?,) (?X ?<)
    (?z) (?Z)
    (?-)
    ))

(cl-defun cae-keyboard-remap (arg)
  (declare (pure t) (side-effect-free t))
  (when (characterp arg)
    (let ((orbit (cl-find arg cae-keyboard-orbits :test #'memq)))
      (when orbit
        (cl-return-from cae-keyboard-remap
          (nth (mod (1+ (cl-position arg orbit)) (length orbit)) orbit)))))
  (when (json-alist-p arg)
    (mapcar (lambda (x)
              (cons (cae-keyboard-remap (car x)) (cdr x)))
            arg))
  (cl-return-from cae-keyboard-remap
    (cl-mapcar #'cae-keyboard-remap arg)))

(defun cae-keyboard-remap-to-strings (arg)
  (declare (pure t) (side-effect-free t))
  (when (characterp arg)
    (cl-return-from cae-keyboard-remap-to-strings
      (char-to-string arg)))
  (when (json-alist-p arg)
    (mapcar (lambda (x)
              (cons (cae-keyboard-remap-to-strings (car x)) (cdr x)))
            arg))
  (cl-mapcar #'char-to-string (cae-keyboard-remap arg)))

(defun cae-keyboard-strings (arg)
  (declare (pure t) (side-effect-free t))
  (cl-mapcar #'char-to-string arg))

(cl-assert (eq (cae-keyboard-remap ?w) ?b))
(cl-assert (eq (cae-keyboard-remap ?b) ?j))
(cl-assert (eq (cae-keyboard-remap ?j) ?o))

(define-key key-translation-map (kbd "C-x t )") (kbd "C-x t 0"))
(define-key key-translation-map (kbd "C-x t !") (kbd "C-x t !"))
(define-key key-translation-map (kbd "C-x t @") (kbd "C-x t 2"))
(define-key key-translation-map (kbd "C-c w !") (kbd "C-c w 1"))
(define-key key-translation-map (kbd "C-c w @") (kbd "C-c w 2"))
(define-key key-translation-map (kbd "C-c w ;") (kbd "C-c w 3"))
(define-key key-translation-map (kbd "C-c w =") (kbd "C-c w 4"))
(define-key key-translation-map (kbd "C-c w +") (kbd "C-c w 5"))
(define-key key-translation-map (kbd "C-c w $") (kbd "C-c w 6"))
(define-key key-translation-map (kbd "C-c w &") (kbd "C-c w 7"))
(define-key key-translation-map (kbd "C-c w *") (kbd "C-c w 8"))
(define-key key-translation-map (kbd "C-c w (") (kbd "C-c w 9"))
(define-key key-translation-map (kbd "C-c w )") (kbd "C-c w 0"))

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
