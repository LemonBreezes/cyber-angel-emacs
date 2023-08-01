;;; lisp/cae-ido.el -*- lexical-binding: t; -*-

(map! :map (ido-common-completion-map ido-file-completion-map)
        "C-w"  #'ido-delete-backward-word-updir
        :map (ido-common-completion-map ido-file-dir-completion-map)
        "C-n"  #'ido-next-match
        "C-p"  #'ido-prev-match
        [down] #'ido-next-match
        [up]   #'ido-prev-match
        :map ido-file-completion-map
        ;; Go to $HOME with ~
        "~"    (cmd! (if (looking-back "/" (point-min))
                             (insert "~/")
                         (call-interactively #'self-insert-command))))

(ido-vertical-mode +1)
(flx-ido-mode +1)
