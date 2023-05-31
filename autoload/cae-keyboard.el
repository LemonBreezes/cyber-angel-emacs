;;; autoload/cae-keyboard.el -*- lexical-binding: t; -*-

;; TODO Rewrite this file so that orbits in `cae-keyboard-orbits' always loop
;; around. That way, I can improve my handling of the Lispy keybindings.
;; (read-char) "<prior>" => (error "Non-character input-event")
;; This is quite problematic!

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
   ;; These keys are in the same place for me as in QWERTY.
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
                          (list (cae-keyboard-kbd (car (last args)))))
                  ""))
    (_ (apply #'string (cae-keyboard-remap (kbd (string-join args " ")))) )))

;;;###autoload
(defun cae-keyboard-kbd-reverse (&rest args)
  (declare (pure t) (side-effect-free t))
  (pcase (length args)
    (0 "")
    (1 (apply #'string (cae-keyboard-remap-reverse (kbd (string-join args " ")))))
    (2 (mapconcat #'kbd
                  (append (butlast args)
                          (list (cae-keyboard-kbd (car (last args)))))
                  ""))
    (_ (apply #'string (cae-keyboard-remap-reverse (kbd (string-join args " ")))) )))

;;;###autoload
(defun cae-keyboard-digit-argument ()
  (interactive)
  (setq last-command-event
        (cae-keyboard-remap-reverse last-command-event))
  (call-interactively #'digit-argument))

;;;###autoload
(defun cae-keyboard-remap-hydra-hint (s)
  (declare (pure t) (side-effect-free t))
  (with-output-to-string
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((ch (following-char)))
          (if (and (char-equal ch ?_)
                   (eq (char-after (+ 2 (point)))
                               ?_))
              (progn
                (forward-char 1)        ; discard _
                (let ((char (following-char)))
                  (princ
                   (string
                    ?_
                    (cae-keyboard-remap-char char)))
                  (forward-char 1)      ; discard _
                  (cond ((eq (char-after (1+ (point))) ?:)
                         (forward-char 1) ; discard :
                         (princ (string ?_ ?:)))
                        ((or (and (<= (char-after (1+ (point))) ?z)
                                  (>= (char-after (1+ (point))) ?a))
                             (and (<= (char-after (1+ (point))) ?Z)
                                  (>= (char-after (1+ (point))) ?A)))
                         (princ (string ?_ ?: ?\s char)))
                        (t (princ (char-to-string ch))))))
            (princ (char-to-string ch))))
        (forward-char)))))
