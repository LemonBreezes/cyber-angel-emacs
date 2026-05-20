;;; cae/ghostel/config.el -*- lexical-binding: t; -*-

(use-package! ghostel
  :when (when (and (bound-and-true-p module-file-suffix) ; requires dynamic-modules support
                   ;; BUG The text is invisibe in Linux TTY.
                   (not (eq (cae-terminal-type) 0))))
  :commands (ghostel ghostel-mode ghostel-project)
  :hook (ghostel-mode . mode-line-invisible-mode) ; modeline serves no purpose in ghostel
  :init
  (map! :leader
        :desc "Toggle ghostel popup"  "ot" #'cae-ghostel-toggle
        :desc "Open ghostel here"     "oT" #'cae-ghostel-here)
  :config
  (set-popup-rule! "^\\*doom:ghostel-popup" :size 0.25 :vslot -4 :select t :quit nil :ttl 0)

  (map! :map ghostel-semi-char-mode-map "C-q" #'ghostel-send-next-key)

  ;; Don't rename buffers based on the terminal title (mirrors vterm's
  ;; default). Keeps the popup buffer name stable so `set-popup-rule!'
  ;; keeps matching it after the shell sets a title via OSC 2.
  (setq ghostel-set-title-function nil)

  ;; Once ghostel is dead, the ghostel buffer is useless. Why keep it around? We
  ;; can spawn another if want one.
  (setq ghostel-kill-buffer-on-exit t)

  ;; 100k lines of scrollback (matching the user's vterm config). The value is
  ;; in bytes; ~100B per line gives us 10MB which is plenty.
  (setq ghostel-max-scrollback (* 10 1024 1024))

  (setq-hook! 'ghostel-mode-hook
    ;; Don't prompt about dying processes when killing ghostel
    confirm-kill-processes nil
    ;; Prevent premature horizontal scrolling
    hscroll-margin 0))
