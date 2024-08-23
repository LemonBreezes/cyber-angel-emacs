;;; org/silly-loop.el -*- lexical-binding: t; -*-

(defun silly-loop (n)
  "Return the time, in seconds, to run N iterations of a loop."
  (garbage-collect)
  (let ((t1 (float-time)))
    (while (> (setq n (1- n)) 0))
    (- (float-time) t1)))

(silly-loop 50000000)
1.315833330154419
1.5213894844055176
1.5475499629974365
1.5262279510498047
1.5520870685577393
1.5634260177612305
1.5971400737762451
1.5982520580291748
1.6331171989440918
1.5799591541290283
1.5810275077819824
2.200998306274414
2.1636734008789062
2.1946732997894287
2.155579090118408
2.0972912311553955
2.0973563194274902
2.0977120399475098
2.125957727432251
2.115626335144043

(byte-compile #'silly-loop)
(silly-loop 50000000)
0.20738005638122559
0.22696828842163086
0.24700474739074707
0.2248544692993164
0.27098727226257324
0.2384016513824463
0.3303389549255371
0.2597498893737793
0.22865056991577148
0.20664596557617188
0.37905359268188477
0.3692502975463867
0.39057445526123047
0.36838269233703613
0.3694281578063965
0.3716592788696289
0.37466907501220703
0.3739044666290283

(native-compile #'silly-loop)
(silly-loop 50000000)
0.06923437118530273
0.10269403457641602
0.12079381942749023
0.11171603202819824
0.10267472267150879
0.09244418144226074
0.09148073196411133
0.0915517807006836
0.09273505210876465
0.09252405166625977
0.09097909927368164
0.09097909927368164
0.11763501167297363
0.12026810646057129
0.1236412525177002
0.1163184642791748
0.1159524917602539
0.11637520790100098
0.11661386489868164
0.11790752410888672
