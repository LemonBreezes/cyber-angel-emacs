;;; test.el --- Description -*- lexical-binding: t; -*-

;;; Code:

(require 'cl)

(defmacro +f1 ()
  "T."
  '(let ((chars "abcdefghijklmnopqrstuvwxyz0123456789"))
     (cl-loop for i from 0 to 32
              concat (char-to-string (seq-random-elt chars)))))

(defmacro +f2 ()
  "T."
  '(let* ((chars "abcdefghijklmnopqrstuvwxyz0123456789")
          (len 33)
          (str (make-string len ?0)))
     (dotimes (i len)
       (setf (aref str i) (aref chars (random len))))))

(defmacro +f3 ()
  "T."
  '(let* ((chars '( 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112
                    113 114 115 116 117 118 119 120 121 122 48 49 50 51 52 53 54 55 56 57))
          (limit (length chars)))
     (apply #'string (cl-loop for i below limit collect (nth (random limit) chars)))))

(defmacro +f4 ()
  '(let* ((chars [ 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112
                      113 114 115 116 117 118 119 120 121 122 48 49 50 51 52 53 54 55 56 57])
          (limit (length chars)))
     (apply #'string (cl-loop for i below limit collect (aref chars (random limit))))))

(cl-loop
 for speed from -1 to 3
 for fastest =
 (let ((native-comp-speed speed))
   (car (cl-sort
         (cl-loop for fn in '(+f1 +f2 +f3 +f4)
                  for test = `(benchmark-run-compiled 1000 ,(macroexpand `(,fn)))
                  do (garbage-collect)
                  collect (cons fn (eval test t)))
         #'<
         :key #'cadr)))
 collect (cons speed fastest))

;; On my 7950X machine
;;((-1 +f2 0.0006711719999999999 0 0.0)
;; (0 +f2 0.000646522 0 0.0)
;; (1 +f2 0.000636132 0 0.0)
;; (2 +f2 0.0005894019999999999 0 0.0)
;; (3 +f2 0.000485391 0 0.0))

;; On my LLVM17 machine
;;((-1 +f2 0.001389721 0 0.0)
;; (0 +f2 0.001389064 0 0.0)
;; (1 +f2 0.001405449 0 0.0)
;; (2 +f2 0.001209127 0 0.0)
;; (3 +f2 0.001140642 0 0.0))

;; Work laptop
;; ((-1 +f2 0.001549871 0 0.0)
;;  (0 +f2 0.001534135 0 0.0)
;;  (1 +f2 0.001087957 0 0.0)
;;  (2 +f2 0.0009529030000000001 0 0.0)
;;  (3 +f2 0.0010227460000000002 0 0.0))

(provide 'test)

;;; test.el ends here
