;;; private/lisp/autoload/eval.el -*- lexical-binding: t; -*-

(defun adviced:edebug-compute-previous-result (_ &rest r)
  "Adviced `edebug-compute-previous-result'."
  (let ((previous-value (nth 0 r)))
    (if edebug-unwrap-results
        (setq previous-value
              (edebug-unwrap* previous-value)))
    (setq edebug-previous-result
          (edebug-safe-prin1-to-string previous-value))))

(advice-add #'edebug-compute-previous-result
            :around
            #'adviced:edebug-compute-previous-result)

(defun adviced:edebug-previous-result (_ &rest r)
  "Adviced `edebug-previous-result'."
  (eros--make-result-overlay edebug-previous-result
    :where (point)
    :duration eros-eval-result-duration))

(advice-add #'edebug-previous-result
            :around
            #'adviced:edebug-previous-result)
