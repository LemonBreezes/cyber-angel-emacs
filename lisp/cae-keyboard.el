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

;; Doom popup module
(when (modulep! :ui popup)
  (define-key key-translation-map (cae-keyboard-kbd "C-" "`") (kbd "C-`"))
  (define-key key-translation-map (cae-keyboard-kbd "C-" "~") (kbd "C-~")))

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
;;(unless (or (modulep! :private meow)
;;            (modulep! :editor lispy))
;;  (defconst home-row-numbers-qwerty
;;    (cae-keyboard-remap '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;)))
;;  (home-row-numbers)
;;  (map! :map universal-argument-map
;;        "l" #'cae-keyboard-insert-current-prefix))

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
    (unless (boundp 'cae-keyboard--lispy-mode-map-backup)
      (defvar cae-keyboard--lispy-mode-map-backup lispy-mode-map))
    (let ((bindings
           `((,(cae-keyboard-kbd "l") special-lispy-right "")
             (,(cae-keyboard-kbd "h") special-lispy-left "")
             (,(cae-keyboard-kbd "f") special-lispy-flow "")
             (,(cae-keyboard-kbd "j") special-lispy-down "")
             (,(cae-keyboard-kbd "k") special-lispy-up "")
             (,(cae-keyboard-kbd "d") special-lispy-different "Move")
             (,(cae-keyboard-kbd "o") special-lispy-other-mode "")
             (,(cae-keyboard-kbd "p") special-lispy-eval-other-window "Eval")
             (,(cae-keyboard-kbd "P") special-lispy-paste "")
             (,(cae-keyboard-kbd "y") special-lispy-occur "Move")
             (,(cae-keyboard-kbd "z") special-lh-knight/body "Move")
             (,(cae-keyboard-kbd "J") special-lispy-outline-next "")
             (,(cae-keyboard-kbd "K") special-lispy-outline-prev "")
             (,(cae-keyboard-kbd "L") special-lispy-outline-goto-child "")
             (,(cae-keyboard-kbd "r") special-lispy-raise "Edit")
             (,(cae-keyboard-kbd "R") special-lispy-raise-some "Edit")
             (,(cae-keyboard-kbd "C") special-lispy-convolute "Edit")
             (,(cae-keyboard-kbd "X") special-lispy-convolute-left "Edit")
             (,(cae-keyboard-kbd "w") special-lispy-move-up "Edit")
             (,(cae-keyboard-kbd "s") special-lispy-move-down "Edit")
             (,(cae-keyboard-kbd "O") special-lispy-oneline "Edit")
             (,(cae-keyboard-kbd "M") special-lispy-alt-multiline "Edit")
             (,(cae-keyboard-kbd "S") special-lispy-stringify "Edit")
             (,(cae-keyboard-kbd "a") special-lispy-ace-symbol "Move")
             (,(cae-keyboard-kbd "H") special-lispy-ace-symbol-replace "Edit")
             (,(cae-keyboard-kbd "m") special-lispy-mark-list "Select")
             (,(cae-keyboard-kbd "e") special-lispy-eval "Eval")
             (,(cae-keyboard-kbd "E") special-lispy-eval-and-insert "Eval")
             (,(cae-keyboard-kbd "g") special-lispy-goto-local "Move")
             (,(cae-keyboard-kbd "G") special-lispy-goto "Move")
             (,(cae-keyboard-kbd "F") special-lispy-follow "Move")
             (,(cae-keyboard-kbd "D") special-pop-tag-mark "Move")
             (,(cae-keyboard-kbd "A") special-lispy-beginning-of-defun "Move")
             (,(cae-keyboard-kbd "i") special-lispy-tab "Edit")
             (,(cae-keyboard-kbd "I") special-lispy-shifttab "View")
             (,(cae-keyboard-kbd "N") special-lispy-narrow "View")
             (,(cae-keyboard-kbd "W") special-lispy-widen "View")
             (,(cae-keyboard-kbd "c") special-lispy-clone "Edit")
             (,(cae-keyboard-kbd "u") special-lispy-undo "Other")
             (,(cae-keyboard-kbd "q") special-lispy-ace-paren "Move")
             (,(cae-keyboard-kbd "Q") special-lispy-ace-char "Move")
             (,(cae-keyboard-kbd "v") special-lispy-view "View")
             (,(cae-keyboard-kbd "t") special-lispy-teleport "Edit")
             (,(cae-keyboard-kbd "n") special-lispy-new-copy "Select")
             (,(cae-keyboard-kbd "b") special-lispy-back "Move")
             (,(cae-keyboard-kbd "B") special-lispy-ediff-regions "View")
             (,(cae-keyboard-kbd "x") special-lispy-x "Other")
             (,(cae-keyboard-kbd "Z") special-lispy-edebug-stop "Other")
             (,(cae-keyboard-kbd "V") special-lispy-visit "Other")
             ;; If any of the above keys get mapped to these, the keybinding will be lost!
             (,(cae-keyboard-kbd ">") special-lispy-slurp "")
             (,(cae-keyboard-kbd "<") special-lispy-barf "")
             (,(cae-keyboard-kbd ".") special-lispy-repeat "Other")
             ("+" special-lispy-join "")
             ("/" special-lispy-splice "")
             ("-" special-lispy-ace-subword "")
             ("~" special-lispy-tilde "")
             ("_" special-lispy-underscore "")
             ("'" lispy-tick "")
             ("," nil "")
             ("=" ,(lookup-key cae-keyboard--lispy-mode-map-backup
                               (cae-keyboard-kbd-reverse "'"))
              ,(if (string-match-p
                    "eval"
                    (symbol-name (lookup-key
                                  cae-keyboard--lispy-mode-map-backup
                                  (cae-keyboard-kbd-reverse "'"))))
                   "Eval" "Edit")))))
      (dolist (binding bindings)
        (define-key lispy-mode-map (car binding) (cadr binding)))
      (eval
       (append '(defhydra cae-lispy-cheat-sheet (:hint nil :foreign-keys run)
                  ("<f6>" nil "Exit" :exit t))
               (cl-loop for binding in bindings
                        unless (string= (caddr binding) "")
                        collect
                        `(,(car binding)
                          ,(cadr binding)
                          ,(thread-last (symbol-name (cadr binding))
                                        (string-remove-prefix "special-")
                                        (string-remove-prefix "lispy-"))
                          :column ,(caddr binding))))))

    ;; TODO `lispy-other-mode-map'
    (eval `(defhydra lh-knight ()
             "knight"
             (,(cae-keyboard-kbd "j") lispy-knight-down)
             (,(cae-keyboard-kbd "k") lispy-knight-up)
             (,(cae-keyboard-kbd "z") nil)))
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
             (,(cae-keyboard-kbd "B") lispy-store-region-and-buffer "store ,bounds")
             (,(cae-keyboard-kbd "R") lispy-reverse "reverse")
             (,(cae-keyboard-kbd "T") lispy-ert "ert")
             (">" lispy-toggle-thread-last "toggle last-threaded form")
             ("" lispy-x-more-verbosity :exit nil)
             ("?" lispy-x-more-verbosity "help" :exit nil)))))

;;; Basically a custom input method

(use-package! aas
  :defer t :init
  (add-hook 'doom-first-input-hook #'aas-global-mode)
  :config
  (aas-set-snippets 'global
    ";--" "—"
    ";-." "→"
    ";=." "⇒"
    "-." "->"
    "=." "=>"))

;; Make typing all-caps more ergonomic.
(use-package! casease
  :config
  (casease-setup
   :hook c-mode-common-hook
   :separator ?-
   :entries
   ((screaming "\\(-\\)[a-z]" "[A-Z]"))))
