;;; cae/ghostel/config.el -*- lexical-binding: t; -*-

(use-package! ghostel
  :when (bound-and-true-p module-file-suffix)
  :commands (ghostel ghostel-mode ghostel-project)
  :defer t :init
  (map! :leader
        :desc "Toggle ghostel popup"  "ot" #'cae-ghostel-toggle
        :desc "Open ghostel here"     "oT" #'cae-ghostel-here
        (:when (modulep! :ui workspaces)
          :desc "Open ghostel workspace" "o C-t" #'cae-ghostel-workspace))
  :config
  (add-hook 'ghostel-mode-hook #'mode-line-invisible-mode)
  (add-hook 'ghostel-mode-hook #'evil-ghostel-mode)
  ;; Mirror Doom's own term/eshell popup rule (the
  ;; "^\\*doom:\\(?:v?term\\|e?shell\\)-popup" entry in `+popup-default-rules').
  (set-popup-rule! "^\\*doom:ghostel-popup" :vslot -5 :size 0.3 :select t :modeline nil :quit nil :ttl nil)

  (map! :map ghostel-semi-char-mode-map "C-q" #'ghostel-send-next-key)

  (when (modulep! :editor evil +everywhere)
    (map! :map ghostel-mode-map
          :localleader "e" #'cae-ghostel-send-escape)
    (setq evil-ghostel-escape 'evil))


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
    hscroll-margin 0)

  (map! :map ghostel-mode-map
        ;; Doom's corfu module binds C-SPC to `completion-at-point' in evil's
        ;; insert/normal state maps, which outrank `ghostel-mode-map'. Bind per
        ;; evil-state here so it actually reaches the terminal instead of
        ;; popping up completion.
        :g "C-SPC" #'cae-ghostel-send-C-SPC
        (:prefix "C-c"
                 "C-x" #'cae-ghostel-send-C-x)
        :n "p" #'ghostel-yank
        :n "P" #'ghostel-yank
        :n [remap evil-paste-pop] #'ghostel-yank-pop)

  (when (modulep! :completion corfu)
    (after! corfu
      (when (not (listp global-corfu-modes))
        (setq global-corfu-modes (list t)))
      (add-to-list 'global-corfu-modes '(not ghostel-mode)))))
