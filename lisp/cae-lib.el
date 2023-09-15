;;; lisp/cae-lib.el -*- lexical-binding: t; -*-

(defmacro cae-defun (name arglist &optional docstring &rest body)
  "`defun' but guaranteed return the created function."
  (declare (doc-string 3) (indent 2))
  `(progn (defun ,name ,arglist ,docstring ,@body)
          #',name))

(defun cae-ignore-errors-a (fun &rest args)
  "Ignore errors in FUN with ARGS."
  (ignore-errors (apply fun args)))

(defun cae-display-graphic-p ()
  (and (display-graphic-p)
       (not (daemonp))))

(defun cae-tty-disable-unicode-p ()
  (not (cae-display-graphic-p)))

(defmacro cae-oneshot-keymap (keymap package)
  `(if (featurep ',package)
       (symbol-value ',keymap)
     (lambda () (interactive)
       (require ',package)
       (let* ((once t)
              (timer
               (when (featurep 'which-key)
                 (run-with-idle-timer
                  which-key-idle-delay nil
                  (lambda ()
                    (when once
                      (let ((which-key-show-prefix t))
                        (which-key--show-keymap
                         (symbol-name ',keymap)
                         (symbol-value ',keymap)
                         nil nil t))))))))
         (set-transient-map (symbol-value ',keymap)
                            (lambda ()
                              (prog1 once
                                (setq once nil)))
                            (lambda ()
                              (cancel-timer timer)))))))

;; A generic adviser for responding yes to yes or no prompts automatically.
(defun cae-always-yes-a (oldfun &rest args)
  (cl-letf (((symbol-function #'yes-or-no-p) (symbol-function #'always))
            ((symbol-function #'y-or-n-p) (symbol-function #'always)))
    (apply oldfun args)))
