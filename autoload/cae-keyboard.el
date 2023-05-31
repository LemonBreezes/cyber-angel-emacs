;;; autoload/cae-keyboard.el -*- lexical-binding: t; -*-

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
  (let ((orbit (cl-find arg cae-keyboard-orbits :test #'memq)))
    (if orbit
        (nth (mod (1+ (cl-position arg orbit)) (length orbit)) orbit)
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
(defun cae-keyboard-remap-to-strings (arg)
  (declare (pure t) (side-effect-free t))
  (cae-keyboard-apply-recursively
   (-compose #'char-to-string #'cae-keyboard-remap-char)
    arg))

(defun cae-keyboard--kbd (&rest args)
  (declare (pure t) (side-effect-free t))
  (apply #'string (cae-keyboard-remap (kbd (string-join args " ")))))

;; only apply cae-keyboard-kdb to the first argument
(defun cae-keyboard--kbd1 (&rest args)
  (declare (pure t) (side-effect-free t))
  (let ((kbd (cae-keyboard--kbd (car args))))
    (cons kbd (cdr args))))

;; only apply cae-keyboard-kdb to the last argument
(defun cae-keyboard--kbd2 (&rest args)
  (declare (pure t) (side-effect-free t))
  (mapconcat #'kbd
             (append (butlast args)
                     (list (cae-keyboard--kbd (car (last args)))))))

(defun cae-keyboard-kbd (&rest args)
  (declare (pure t) (side-effect-free t))
  (pcase (length args)
    (0 (kbd ""))
    (1 (cae-keyboard--kbd1 args))
    (_ (cae-keyboard--kbd2 args))))
