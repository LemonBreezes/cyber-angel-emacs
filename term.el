;;; term.el -*- lexical-binding: t; -*-

(after! em-glob
  ;; Allow us to type HEAD~1, HEAD~2, etc., as arguments to git commands.
  (setq eshell-error-if-no-glob nil))

(after! vterm
  (setq vterm-max-scrollback 100000))

(after! comint
  (setq comint-history-isearch 'dwim
        comint-buffer-maximum-size 8192))
(setq-hook! 'comint-mode-hook
  imenu-generic-expression
  `(("Prompts" ,(concat comint-prompt-regexp "\\(.*\\)") 1))
  ;; For aider.el since the AI model outputs unicode whitespace characters.
  nobreak-char-display nil)

(use-package! comint-histories
  :after comint :config
  (comint-histories-mode 1)
  (setq comint-histories-global-filters '((lambda (x) (<= (length x) 3))))
  (comint-histories-add-history
    gdb
    :predicates '((lambda () (string-match-p "^(gdb)"
                                             (comint-histories-get-prompt))))
    :length 2000)
  (comint-histories-add-history
    python
    :predicates
    '((lambda () (or (derived-mode-p 'inferior-python-mode)
                     (string-match-p "^>>>" (comint-histories-get-prompt)))))
    :length 2000)
  (comint-histories-add-history
    ielm
    :predicates '((lambda () (derived-mode-p 'inferior-emacs-lisp-mode)))
    :length 2000)
  (comint-histories-add-history
    shell
    :predicates '((lambda () (derived-mode-p 'shell-mode)))
    :filters '("^ls" "^cd")
    :length 2000)
  (define-key comint-mode-map (kbd "M-r")
    (lambda () (interactive)
      (let ((ivy-sort-functions-alist nil)
            (ivy-prescient-enable-sorting nil)
            (vertico-sort-function nil)
            (vertico-sort-override-function nil)
            (vertico-prescient-enable-sorting nil)
            (selectrum-should-sort nil)
            (selectrum-prescient-enable-sorting nil))
        (call-interactively #'comint-histories-search-history)))))

(after! em-term
  ;; Some of the commands I copied from other configurations and will likely
  ;; never use.
  (setq eshell-visual-commands
        '("ranger" "vi" "screen" "top" "less" "more" "lynx"
          "ncftp" "pine" "tin" "trn" "elm" "vim" "nmtui" "alsamixer" "htop"
          "elinks" "tail" "nano" "ssh" "python" "tmux" "telnet" "fzf"
          "pulsemixer" "ranger" "bluetoothctl" "watch" "ncmpcpp" "btm"
          "ptpython" "ipython" "pshell" "nmtui" "dstat" "pgcli" "vue" "ngrok")
        eshell-visual-subcommands `(("gh" "repo" "fork")
                                    ("geth" "attach")
                                    ,@(unless (string= (getenv "GIT_PAGER")
                                                       "cat")
                                        '(("git" "log" "diff" "show"))))
        eshell-visual-options '(("git" "--help" "--paginate"))))
