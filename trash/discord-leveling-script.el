;;; lisp/discord-leveling-script.el -*- lexical-binding: t; -*-

;; Focus Discord.
(+workspace-switch "Discord" t)

;; Initialize state.
(defvar discord-leveling-script-timer nil)
(defvar discord-leveling-script--last-message nil)
(defvar discord-leveling-script--last-channels nil)
(defvar discord-leveling-script--inc 1)

(defun discord-leveling-script-stop ()
  (remove-hook 'pre-command-hook #'discord-leveling-script-stop)
  (cancel-timer discord-leveling-script-timer))

;; Set up our exit condition.
(add-hook 'pre-command-hook #'discord-leveling-script-stop)

(defun discord-leveling-script--send-message (s)
  (mapc (lambda (c)
          (run-at-time (* 0.07 (cl-incf discord-leveling-script--inc)) nil
                       #'exwm-input--fake-key
                       c))
        s)
  (run-at-time (* 0.07 (cl-incf discord-leveling-script--inc)) nil
               #'exwm-input--fake-key
               'return))

(defun discord-leveling-script--delete-last-message ()
  (run-at-time (+ (* 0.07 (cl-incf discord-leveling-script--inc)) 0.18) nil
               #'+exwm-do-mouse-click 482 1267)
  (cl-incf discord-leveling-script--inc (/ 0.18 0.07)))

(defvar discord-leveling-script--channels
  '(("gm"           (202 . 1026))
    ("voicechat"    (181 . 727))
    ("library"      (197 . 802))
    ("milady-art"   (194 . 682))
    ("nfts-crypto"  (170 . 878))
    ("milady-pasta" (171 . 760))
    ("art"          (171 . 1217))
    ("fashion"      (166 . 1246))
    ("figures"      (166 . 1295))))

(defun discord-leveling-script--select-random-channel ()
  (cl-destructuring-bind (channel (x-position . y-position))
      (seq-random-elt
       ;; Do not message the same channel twice in a row.
       (cl-remove-if
        (lambda (x)
          (cl-member (car x)
                     discord-leveling-script--last-channels
                     :test #'equal))
        discord-leveling-script--channels))
    (run-at-time (* 0.07 (cl-incf discord-leveling-script--inc 7)) nil
                 #'+exwm-do-mouse-click x-position y-position)
    (setq discord-leveling-script--last-channels
          ;; Never pick the same channel twice in any 5 consecutive selections.
          (if (< (length discord-leveling-script--last-channels) 5)
              (cons channel discord-leveling-script--last-channels)
            (butlast (-rotate 1 (cons channel discord-leveling-script--last-channels))))))
  (cl-incf discord-leveling-script--inc 2))

;; Run the loop without hanging Emacs
(setq discord-leveling-script-timer
      (run-at-time
       ;; Do not set the delay too small. You will cause unpredictable behavior if you do.
       1 59
       (lambda ()
         (setq discord-leveling-script--inc 1)
         (discord-leveling-script--select-random-channel)
         (discord-leveling-script--send-message "milady")
         (discord-leveling-script--delete-last-message))))
