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

;;;###autoload
(defun cae-keyboard-kbd (&rest args)
  (declare (pure t) (side-effect-free t))
  (cae-keyboard-remap-char (kbd (string-join args " "))))
