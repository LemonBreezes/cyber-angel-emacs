;;; ~/.doom.d/lisp/cae-keyboard.el -*- lexical-binding: t; -*-

;;; Remap keys

(define-key key-translation-map (cae-keyboard-kbd "C-x t" "0") (kbd "C-x t 0"))
(define-key key-translation-map (cae-keyboard-kbd "C-x t" "1") (kbd "C-x t 1"))
(define-key key-translation-map (cae-keyboard-kbd "C-x t" "2") (kbd "C-x t 2"))
(define-key key-translation-map (cae-keyboard-kbd "C-c w" "0") (kbd "C-c w 0"))
(define-key key-translation-map (cae-keyboard-kbd "C-c w" "1") (kbd "C-c w 1"))
(define-key key-translation-map (cae-keyboard-kbd "C-c w" "2") (kbd "C-c w 2"))
(define-key key-translation-map (cae-keyboard-kbd "C-c w" "3") (kbd "C-c w 3"))
(define-key key-translation-map (cae-keyboard-kbd "C-c w" "4") (kbd "C-c w 4"))
(define-key key-translation-map (cae-keyboard-kbd "C-c w" "5") (kbd "C-c w 5"))
(define-key key-translation-map (cae-keyboard-kbd "C-c w" "6") (kbd "C-c w 6"))
(define-key key-translation-map (cae-keyboard-kbd "C-c w" "7") (kbd "C-c w 7"))
(define-key key-translation-map (cae-keyboard-kbd "C-c w" "8") (kbd "C-c w 8"))
(define-key key-translation-map (cae-keyboard-kbd "C-c w" "9") (kbd "C-c w 9"))

;; other window prefix
(define-key key-translation-map (kbd "C-x 4 C-x 4") (kbd "C-x 4 4"))
(define-key key-translation-map (kbd "C-x 4 C-x 1") (kbd "C-x 4 1"))
(define-key key-translation-map (kbd "C-x 4 C-x 0") (kbd "C-x 4 0"))
(define-key key-translation-map (kbd "C-x 4 C-x C-b") (kbd "C-x 4 b"))
(define-key key-translation-map (kbd "C-x 4 C-x C-f") (kbd "C-x 4 f"))
(define-key key-translation-map (kbd "C-x 4 C-x C-.") (kbd "C-x 4 ."))
(define-key key-translation-map (kbd "C-x 4 C-x C-i") (kbd "C-x 4 i"))
(define-key key-translation-map (kbd "C-x 4 C-x C-a") (kbd "C-x 4 a"))
(define-key key-translation-map (kbd "C-x 4 C-x C-c") (kbd "C-x 4 c"))
(define-key key-translation-map (kbd "C-x 4 C-x C-d") (kbd "C-x 4 d"))
(define-key key-translation-map (kbd "C-x 4 C-x C-m") (kbd "C-x 4 m"))
(define-key key-translation-map (kbd "C-x 4 C-x C-p") (kbd "C-x 4 p"))
(define-key key-translation-map (kbd "C-x 4 C-x C-r") (kbd "C-x 4 r"))
(define-key key-translation-map (kbd "C-x 4 C-x C-o") (kbd "C-x 4 C-o"))
(define-key key-translation-map (kbd "C-x 4 C-x C-j") (kbd "C-x 4 C-j"))
(define-key key-translation-map (cae-keyboard-kbd "C-x 4" "0") (kbd "C-x 4 0"))
(define-key key-translation-map (cae-keyboard-kbd "C-x 4" "1") (kbd "C-x 4 1"))
(define-key key-translation-map (cae-keyboard-kbd "C-x 4" "4") (kbd "C-x 4 4"))

;; other frame prefix
(define-key key-translation-map (kbd "C-x 5 C-x 5") (kbd "C-x 5 5"))
(define-key key-translation-map (kbd "C-x 5 C-x 0") (kbd "C-x 5 0"))
(define-key key-translation-map (kbd "C-x 5 C-x 1") (kbd "C-x 5 1"))
(define-key key-translation-map (kbd "C-x 5 C-x 2") (kbd "C-x 5 2"))
(define-key key-translation-map (kbd "C-x 5 C-x C-o") (kbd "C-x 5 C-o"))
(define-key key-translation-map (kbd "C-x 5 C-x C-.") (kbd "C-x 5 ."))
(define-key key-translation-map (kbd "C-x 5 C-x C-b") (kbd "C-x 5 b"))
(define-key key-translation-map (kbd "C-x 5 C-x C-c") (kbd "C-x 5 c"))
(define-key key-translation-map (kbd "C-x 5 C-x C-d") (kbd "C-x 5 d"))
(define-key key-translation-map (kbd "C-x 5 C-x C-f") (kbd "C-x 5 f"))
(define-key key-translation-map (kbd "C-x 5 C-x C-m") (kbd "C-x 5 m"))
(define-key key-translation-map (kbd "C-x 5 C-x C-p") (kbd "C-x 5 p"))
(define-key key-translation-map (kbd "C-x 5 C-x C-r") (kbd "C-x 5 r"))
(define-key key-translation-map (kbd "C-x 5 C-x C-u") (kbd "C-x 5 u"))
(define-key key-translation-map (cae-keyboard-kbd "C-x 5" "0") (kbd "C-x 5 0"))
(define-key key-translation-map (cae-keyboard-kbd "C-x 5" "1") (kbd "C-x 5 1"))
(define-key key-translation-map (cae-keyboard-kbd "C-x 5" "2") (kbd "C-x 5 2"))
(define-key key-translation-map (cae-keyboard-kbd "C-x 5" "5") (kbd "C-x 5 5"))

(when (modulep! :ui popup)
  ;; For Meow, it's for some reason, not enough to just use a key translation
  ;; map. We need to bind the keys to the commands. This is worth investigating.
  (unless (boundp 'cae-keyboard-old-c-\`-command)
    (defvar cae-keyboard-old-c-\`-command (lookup-key (current-global-map) (kbd "C-`")))
    (defvar cae-keyboard-old-c-~-command (lookup-key (current-global-map) (kbd "C-~"))))
  (global-set-key (kbd (cae-keyboard-kbd "C-" "`"))
                  cae-keyboard-old-c-\`-command)
  (global-set-key (kbd (cae-keyboard-kbd "C-" "~"))
                  cae-keyboard-old-c-~-command))

;;; Universal argument

(eval `(map! :map universal-argument-map
             ,(cae-keyboard-kbd "1") #'cae-keyboard-digit-argument
             ,(cae-keyboard-kbd "2") #'cae-keyboard-digit-argument
             ,(cae-keyboard-kbd "3") #'cae-keyboard-digit-argument
             ,(cae-keyboard-kbd "4") #'cae-keyboard-digit-argument
             ,(cae-keyboard-kbd "5") #'cae-keyboard-digit-argument
             ,(cae-keyboard-kbd "6") #'cae-keyboard-digit-argument
             ,(cae-keyboard-kbd "7") #'cae-keyboard-digit-argument
             ,(cae-keyboard-kbd "8") #'cae-keyboard-digit-argument
             ,(cae-keyboard-kbd "9") #'cae-keyboard-digit-argument
             ,(cae-keyboard-kbd "0") #'cae-keyboard-digit-argument))
(unless (modulep! :private meow)
  (defconst home-row-numbers-qwerty
    (cae-keyboard-remap '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;)))
  (home-row-numbers))
(map! :map universal-argument-map
      "#" #'cae-keyboard-insert-current-prefix)


;;; Distinguishing dual-purpose keycodes

;; Look up the "TAB" key in the current major mode. If it is bound and "<tab>"
;; is not bound, then bind it also to <tab>. This way, our global <tab> key
;; does not bind "C-i" everywhere.
(when (display-graphic-p)
  (defun cae-keyboard-conditionally-remap-C-i ()
    (run-at-time
     0.0 nil
     (lambda ()
       (let ((tab-command (lookup-key (current-local-map) (kbd "<tab>")))
             (C-i-command (lookup-key (current-local-map) (kbd "C-i"))))
         (when (and C-i-command
                    (or (not tab-command)
                        (eq tab-command 'self-insert-command))
                    (not (eq tab-command C-i-command)))
           (define-key (current-local-map) (kbd "<tab>") C-i-command))))))

  (add-hook 'after-change-major-mode-hook #'cae-keyboard-conditionally-remap-C-i)
  (map! "<tab>" #'indent-for-tab-command
        "C-i" #'doom/dumb-indent
        "C-S-i" #'doom/dumb-dedent))

;;; Lispy

(when (modulep! :editor lispy)
  (after! lispy
    ;; navigation
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "l") 'lispy-right)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "h") 'lispy-left)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "f") 'lispy-flow)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "j") 'lispy-down)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "k") 'lispy-up)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "d") 'lispy-different)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "o") 'lispy-other-mode)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "p") 'lispy-eval-other-window)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "P") 'lispy-paste)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "y") 'lispy-occur)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "z") 'lh-knight/body)
    ;; outline
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "J") 'lispy-outline-next)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "K") 'lispy-outline-prev)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "L") 'lispy-outline-goto-child)
    ;; Paredit transformations
    (lispy-define-key lispy-mode-map-special ">" 'lispy-slurp)
    (lispy-define-key lispy-mode-map-special "<" 'lispy-barf)
    (lispy-define-key lispy-mode-map-special "/" 'lispy-splice)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "r") 'lispy-raise)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "R") 'lispy-raise-some)
    (lispy-define-key lispy-mode-map-special "+" 'lispy-join)
    ;; more transformations
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "C") 'lispy-convolute)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "X") 'lispy-convolute-left)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "w") 'lispy-move-up)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "s") 'lispy-move-down)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "O") 'lispy-oneline)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "M") 'lispy-alt-multiline)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "S") 'lispy-stringify)
    ;; marking
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "a") 'lispy-ace-symbol
      :override '(cond ((looking-at lispy-outline)
                        (lispy-meta-return))))
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "H") 'lispy-ace-symbol-replace)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "m") 'lispy-mark-list)
    ;; dialect-specific
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "e") 'lispy-eval)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "E") 'lispy-eval-and-insert)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "g") 'lispy-goto-local)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "G") 'lispy-goto)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "F") 'lispy-follow t)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "D") 'pop-tag-mark)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "A") 'lispy-beginning-of-defun)
    (lispy-define-key lispy-mode-map-special "_" 'lispy-underscore)
    ;; miscellanea
    (define-key lispy-mode-map-special (kbd "SPC") 'lispy-space)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "i") 'lispy-tab)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "I") 'lispy-shifttab)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "N") 'lispy-narrow)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "W") 'lispy-widen)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "c") 'lispy-clone)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "u") 'lispy-undo)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "q") 'lispy-ace-paren
      :override '(cond ((bound-and-true-p view-mode)
                        (View-quit))))
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "Q") 'lispy-ace-char)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "v") 'lispy-view)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "t") 'lispy-teleport
      :override '(cond ((looking-at lispy-outline)
                        (end-of-line))))
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "n") 'lispy-new-copy)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "b") 'lispy-back)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "B") 'lispy-ediff-regions)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "x") 'lispy-x)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "Z") 'lispy-edebug-stop)
    (lispy-define-key lispy-mode-map-special (cae-keyboard-kbd "V") 'lispy-visit)
    (lispy-define-key lispy-mode-map-special "-" 'lispy-ace-subword)
    (lispy-define-key lispy-mode-map-special "." 'lispy-repeat)
    (lispy-define-key lispy-mode-map-special "~" 'lispy-tilde)
    ;; TODO digit argument
    (eval `(defhydra lh-knight ()
             "knight"
             (,(cae-keyboard-kbd"j") lispy-knight-down)
             (,(cae-keyboard-kbd"k") lispy-knight-up)
             (,(cae-keyboard-kbd"z") nil)))
    (eval `(lispy-defverb
            "goto"
            ((,(cae-keyboard-kbd "d") lispy-goto)
             (,(cae-keyboard-kbd "l") lispy-goto-local)
             (,(cae-keyboard-kbd "r") lispy-goto-recursive)
             (,(cae-keyboard-kbd "p") lispy-goto-projectile)
             (,(cae-keyboard-kbd "f") lispy-follow)
             (,(cae-keyboard-kbd "b") pop-tag-mark)
             (,(cae-keyboard-kbd "q") lispy-quit)
             (,(cae-keyboard-kbd "j") lispy-goto-def-down)
             (,(cae-keyboard-kbd "a") lispy-goto-def-ace)
             (,(cae-keyboard-kbd "e") lispy-goto-elisp-commands))))
    (eval `(lispy-defverb
            "other"
            ((,(cae-keyboard-kbd "h") lispy-move-left)
             (,(cae-keyboard-kbd "j") lispy-down-slurp)
             (,(cae-keyboard-kbd "k") lispy-up-slurp)
             (,(cae-keyboard-kbd "l") lispy-move-right)
             ("SPC" lispy-other-space)
             (,(cae-keyboard-kbd "g") lispy-goto-mode))))
    (eval `(defhydra hydra-lispy-x (:exit t
                                    :hint nil
                                    :columns 3)
             "x"
             (,(cae-keyboard-kbd "b") lispy-bind-variable "bind variable")
             (,(cae-keyboard-kbd "c") lispy-to-cond "to cond")
             (,(cae-keyboard-kbd "C") lispy-cleanup "cleanup")
             (,(cae-keyboard-kbd "d") lispy-to-defun "to defun")
             (,(cae-keyboard-kbd "D") lispy-extract-defun "extract defun")
             (,(cae-keyboard-kbd "e") lispy-edebug "edebug")
             (,(cae-keyboard-kbd "f") lispy-flatten "flatten")
             (,(cae-keyboard-kbd "F") lispy-let-flatten "let-flatten")
             ;; ("g" nil)
             (,(cae-keyboard-kbd "h") lispy-describe "describe")
             (,(cae-keyboard-kbd "i") lispy-to-ifs "to ifs")
             (,(cae-keyboard-kbd "j") lispy-debug-step-in "debug step in")
             (,(cae-keyboard-kbd "k") lispy-extract-block "extract block")
             (,(cae-keyboard-kbd "l") lispy-to-lambda "to lambda")
             (,(cae-keyboard-kbd "m") lispy-cursor-ace "multi cursor")
             (,(cae-keyboard-kbd "n") lispy-cd)
             ;; ("o" nil)
             (,(cae-keyboard-kbd "p") lispy-set-python-process "process")
             ;; ("q" nil)
             (,(cae-keyboard-kbd "r") lispy-eval-and-replace "eval and replace")
             (,(cae-keyboard-kbd "s") save-buffer)
             (,(cae-keyboard-kbd "t") lispy-view-test "view test")
             (,(cae-keyboard-kbd "u") lispy-unbind-variable "unbind let-var")
             (,(cae-keyboard-kbd "v") lispy-eval-expression "eval")
             (,(cae-keyboard-kbd "w") lispy-show-top-level "where")
             ;; ("x" nil)
             ;; ("y" nil)
             ;; ("z" nil)
             (,(cae-keyboard-kbd "B") lispy-store-region-and-buffer "store list bounds")
             (,(cae-keyboard-kbd "R") lispy-reverse "reverse")
             (,(cae-keyboard-kbd "T") lispy-ert "ert")
             (">" lispy-toggle-thread-last "toggle last-threaded form")
             ("" lispy-x-more-verbosity :exit nil)
             ("?" lispy-x-more-verbosity "help" :exit nil)))
    ))
