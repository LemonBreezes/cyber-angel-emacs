;;; org/silly-loop.el -*- lexical-binding: t; -*-

(defun silly-loop (n)
  "Return the time, in seconds, to run N iterations of a loop."
  (garbage-collect)
  (let ((t1 (float-time)))
    (while (> (setq n (1- n)) 0))
    (- (float-time) t1)))

(silly-loop 50000000)
2.0972912311553955
2.0973563194274902
2.0977120399475098
2.125957727432251
2.115626335144043

(byte-compile #'silly-loop)
(silly-loop 50000000)
0.36838269233703613
0.3694281578063965
0.3716592788696289
0.37466907501220703
0.3739044666290283

(silly-loop 50000000)
(native-compile #'silly-loop)
0.1163184642791748
0.1159524917602539
0.11637520790100098
0.11661386489868164
0.11790752410888672
