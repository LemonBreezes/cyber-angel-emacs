;;; autoload/cae-keyboard.el -*- lexical-binding: t; -*-

(unless (featurep 'json)
  (defun json-alist-p (list)
    "Non-nil if and only if LIST is an alist with simple keys."
    (declare (pure t) (side-effect-free error-free))
    (while (and (consp (car-safe list))
                (atom (caar list))
                (setq list (cdr list))))
    (null list)))

;; These are the orbits of the alphabet under the permutation of the keys
;; created by the keyboard layout. Because my keyboard layout is so weird, some
;; of these remappings involve non-printable characters. I have replaced those
;; in the list below with a null byte (0x00) and terminated the vector.
(defvar cae-keyboard-orbits
  [[?w ?b ?j ?o ?y ?v ?k ?t ?g ?s ?a ?r ?f ?n ?m ?c ?u]
   [?W ?B ?J ?O ?Y ?V ?K ?T ?G ?S ?A ?R ?F ?N ?M ?C ?U]
   [?# ?3 ?\; ?h ?d ?e ?p ?\' ?_ ?~ ?` ?\" ?% ?5 ?+ ?\0]
   [?: ?H ?D ?E ?P ?% ?^ ?6 ?$ ?4 ?= ?\0]
   [?i ?l] [?I ?L]
   [?x ?,] [?X ?<]
   [?0 ?\)]
   [?1 ?!]
   [?2 ?@]
   [?5 ?+ ?\0]
   [?7 ?&]
   [?8 ?*]
   [?9 ?\(]
   [?z] [?Z] [?-] [?.] [?>] [?q] [?Q] [?\[] [?\]]])

;;;###autoload
(defun cae-keyboard-insert-current-prefix (arg)
"Insert the current prefix argument."
(interactive "P")
(insert (format "%s" arg)))

(cl-defun cae-keyboard-apply-recursively (fn arg)
  (declare (pure t) (side-effect-free t))
  (when (characterp arg)
    (cl-return-from cae-keyboard-apply-recursively
      (funcall fn arg)))
  (when (json-alist-p arg)
    (cl-return-from cae-keyboard-apply-recursively
      (mapcar (lambda (x)
                (cons (cae-keyboard-apply-recursively fn (car x)) (cdr x)))
              arg)))
  (when (stringp arg)
    (cl-return-from cae-keyboard-apply-recursively
      (cl-mapcar (lambda (x) (cae-keyboard-apply-recursively fn x)) (kbd arg))))
  (cl-return-from cae-keyboard-apply-recursively
    (cl-mapcar (lambda (x) (cae-keyboard-apply-recursively fn x)) arg)))

(defun cae-keyboard-remap-char (arg)
  (declare (pure t) (side-effect-free t))
  (let ((orbit (cl-find arg cae-keyboard-orbits :test #'cl-find)))
    (if orbit
        (aref orbit (mod (1+ (cl-position arg orbit)) (length orbit)))
      arg)))

(defun cae-keyboard-remap-char-reverse (arg)
  (declare (pure t) (side-effect-free t))
  (let ((orbit (cl-find arg cae-keyboard-orbits :test #'cl-find)))
    (if orbit
        (aref orbit (mod (1- (cl-position arg orbit)) (length orbit)))
      arg)))

;;;###autoload
(defun cae-keyboard-strings (arg)
  (declare (pure t) (side-effect-free t))
  (cae-keyboard-apply-recursively #'char-to-string arg))

;;;###autoload
(defun cae-keyboard-remap (arg)
  (declare (pure t) (side-effect-free t))
  (cae-keyboard-apply-recursively #'cae-keyboard-remap-char arg))

;;;###autoload
(defun cae-keyboard-remap-reverse (arg)
  (declare (pure t) (side-effect-free t))
  (cae-keyboard-apply-recursively #'cae-keyboard-remap-char-reverse arg))

;;;###autoload
(defun cae-keyboard-remap-to-strings (arg)
  (declare (pure t) (side-effect-free t))
  (cae-keyboard-apply-recursively
   (-compose #'char-to-string #'cae-keyboard-remap-char)
   arg))

;;;###autoload
(defun cae-keyboard-kbd (&rest args)
  (declare (pure t) (side-effect-free t))
  (pcase (length args)
    (0 (kbd ""))
    (1 (apply #'string (cae-keyboard-remap (kbd (string-join args " ")))))
    (2 (mapconcat #'kbd
                  (append (butlast args)
                          (list (cae-keyboard-kbd (car (last args)))))))
    (_ (apply #'string (cae-keyboard-remap (kbd (string-join args " ")))) )))

;;;###autoload
(defun cae-keyboard-kbd-reverse (&rest args)
  (declare (pure t) (side-effect-free t))
  (pcase (length args)
    (0 "")
    (1 (apply #'string (cae-keyboard-remap-reverse (kbd (string-join args " ")))))
    (2 (mapconcat #'kbd
                  (append (butlast args)
                          (list (cae-keyboard-kbd (car (last args)))))))
    (_ (apply #'string (cae-keyboard-remap-reverse (kbd (string-join args " ")))) )))

;;;###autoload
(defun cae-keyboard-digit-argument ()
  (interactive)
  (setq last-command-event
        (cae-keyboard-remap-reverse last-command-event))
  (call-interactively #'digit-argument))

;;;###autoload
(defun cae-keyboard-electric-spacing-\(()
    "The same as `electric-spacing-\(' but does not insert the closing paren."
    (if (looking-back (regexp-opt '("if" "else" "for" "while" "switch")))
        (insert " (")
      (insert "(")))
