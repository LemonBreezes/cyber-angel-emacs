;;; lisp/cae-ido.el -*- lexical-binding: t; -*-

(pushnew! ido-ignore-files "\\`.DS_Store$" "Icon\\?$")
(setq ido-ignore-buffers
      '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*[Hh]elp" "^\\*Buffer"
        "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-" "_region_"
        " output\\*$" "^TAGS$" "^\*Ido")
      ido-auto-merge-work-directories-length -1
      ido-confirm-unique-completion t
      ido-case-fold t
      ido-create-new-buffer 'always
      ido-enable-flex-matching t
      ido-everywhere t)

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
