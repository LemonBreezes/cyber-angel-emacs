;;; private/misc-applications/+zone.el -*- lexical-binding: t; -*-

(use-package! zone
  :defer-incrementally t
  :init
  (map! :leader
        :prefix +misc-applications-eyecandy-prefix
        "z" #'zone-choose)
  :config
  ;; remove not interesting programs
  (setq zone-programs [zone-nyan
                       zone-rainbow
                       qzone-pgm-md5
                       zone-pgm-sl
                       zone-pgm-jitter
                       zone-pgm-putz-with-case
                       zone-pgm-dissolve
                       ;; zone-pgm-explode
                       zone-pgm-whack-chars
                       zone-pgm-rotate
                       zone-pgm-rotate-LR-lockstep
                       zone-pgm-rotate-RL-lockstep
                       zone-pgm-rotate-LR-variable
                       zone-pgm-rotate-RL-variable
                       zone-pgm-drip

                       ;; zone-pgm-drip-fretfully
                       ;; zone-pgm-five-oclock-swan-dive
                       ;; zone-pgm-martini-swan-dive
                       zone-pgm-rat-race
                       zone-pgm-paragraph-spaz])
  ;; zone-pgm-stress
  ;; zone-pgm-stress-destress
  ;; zone-pgm-random-life
  (zone-when-idle (* 5 60))

  ;; Focus
  (defadvice! +zone-switch-to-root-window-a (oldfun &rest args)
    :around #'zone
    (with-selected-window (car (doom-visible-windows))
      (apply oldfun args))))
