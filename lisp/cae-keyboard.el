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
  (define-key key-translation-map (kbd (concat "C-" (cae-keyboard-remap "`"))) (kbd "C-`"))
  (define-key key-translation-map (kbd (concat "C-" (cae-keyboard-remap "~"))) (kbd "C-~"))
  ;; Ensure no keybindings are left inaccessible by the above remapping.
  (cond ((and (eq (cae-keyboard-remap ?\`) ?~)
              (eq (cae-keyboard-remap ?~) ?\`))
         ;; ` → ~ and ~ → `
         (ignore))
        ((eq (cae-keyboard-remap ?~) ?\`)
         ;; ~ → ` → " (as an example) so we need to map C-~ → C-" so that C-"
         ;; does not get lost.
         (define-key key-translation-map
           (kbd "C-~")
           (kbd (concat "C-" (cae-keyboard-remap "`")))))
        ((eq (cae-keyboard-remap ?\`) ?~)
         ;; Similar to the last scenario
         (define-key key-translation-map
           (kbd "C-`")
           (kbd (concat "C-" (cae-keyboard-remap "~")))))
        (t
         ;; ~ → a and ` → b and a ≠ b so we can just map C-~ → C-a and
         ;; C-` → C-b as those keybindings are "free".
         (define-key key-translation-map
           (kbd "C-`")
           (kbd (concat "C-" (cae-keyboard-remap "`"))))
         (define-key key-translation-map
           (kbd "C-~")
 ))))

;;; Number row

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
(after! embark
  (eval `(map! :map embark-collect-mode-map
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
  (map! :map minibuffer-local-map
        ;; `C-c ;' is a little easier to type on my keyboard. This alternate
        ;; keybinding also works in the terminal.
        "C-c ;" #'embark-export
        "C-c C-;" nil))

(defun cae-make-conditional-key-translation (key-from key-to translate-keys-p)
  "Make a Key Translation such that if the translate-keys-p function returns true,
   key-from translates to key-to, else key-from translates to itself.  translate-keys-p
   takes key-from as an argument. "
  (define-key key-translation-map key-from
    (lambda (prompt)
      (if (funcall translate-keys-p key-from) key-to key-from))))

(defun cae-translate-number-row-p (key-from)
  "Return true if we should translate the number row keys."
  (and
   ;; Only allow a non identity translation if we're beginning a Key Sequence.
   (equal key-from (this-command-keys))
   (or (and (minibufferp)
            (not (bound-and-true-p vertico--input))
            (cl-member (minibuffer-prompt)
                       '("Go to line: "
                         "Go to char: "
                         "Move to column: ")
                       :test #'string-match-p))
       (and (featurep 'ispell)
            (not (minibufferp))         ;I added this check basically out of paranoia.
            (get-buffer-window ispell-choices-buffer)))))

(dolist (key-from (mapcar #'char-to-string '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)))
  (cae-make-conditional-key-translation (cae-keyboard-kbd key-from)
                                        (kbd key-from)
                                        #'cae-translate-number-row-p))

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
             (,(cae-keyboard-kbd "o") special-lispy-goto-mode "Other")
             (,(cae-keyboard-kbd "p") special-lispy-eval-other-window "Eval")
             (,(cae-keyboard-kbd "P") special-lispy-paste "Edit")
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
             (,(cae-keyboard-kbd ">") special-lispy-slurp "")
             (,(cae-keyboard-kbd "<") special-lispy-barf "")
             (,(cae-keyboard-kbd ".") special-lispy-repeat "Other")
             (,(cae-keyboard-kbd-reverse ",")
              ,(lookup-key cae-keyboard--lispy-mode-map-backup
                           (cae-keyboard-kbd-reverse ","))
              "Other")                  ; This bit is really annoying. I don't
                                        ; want to separately remap every lispy
                                        ; command to a category just for this
                                        ; one keycode. Lol.
             ("+" special-lispy-join "")
             ("/" special-lispy-splice "")
             ("-" special-lispy-ace-subword "")
             ("~" special-lispy-tilde "")
             ("_" special-lispy-underscore "")
             ("'" lispy-tick "")
             ("," nil ""))))
      (dolist (binding bindings)
        (define-key lispy-mode-map (car binding) (cadr binding)))
      (eval
       (append '(defhydra cae-lispy-cheat-sheet (:hint nil :foreign-keys run)
                  ("<f6>" nil "Exit" :exit t))
               (cl-loop for binding in bindings
                        unless (or (string= (caddr binding) "")
                                   (string= (car binding) ","))
                        collect
                        `(,(car binding)
                          ,(cadr binding)
                          ,(thread-last (symbol-name (cadr binding))
                                        (string-remove-prefix "special-")
                                        (string-remove-prefix "lispy-"))
                          :column ,(caddr binding))))))
    (define-key lispy-mode-map (kbd "<f6>") #'cae-lispy-cheat-sheet/body)
    (when (modulep! :editor multiple-cursors)
      (after! multiple-cursors-core
        (add-to-list 'mc/cmds-to-run-once #'cae-lispy-cheat-sheet/body)
        (add-to-list 'mc/cmds-to-run-once #'cae-lispy-cheat-sheet/nil)))

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
    ;; These keys are not bound geometrically, they are bound based on
    ;; mnemonics, so it's better to leave them unchanged.
    ;;(eval `(defhydra hydra-lispy-x (:exit t
    ;;                                :hint nil
    ;;                                :columns 3)
    ;;         "x"
    ;;         (,(cae-keyboard-kbd "b") lispy-bind-variable "bind variable")
    ;;         (,(cae-keyboard-kbd "c") lispy-to-cond "to cond")
    ;;         (,(cae-keyboard-kbd "C") lispy-cleanup "cleanup")
    ;;         (,(cae-keyboard-kbd "d") lispy-to-defun "to defun")
    ;;         (,(cae-keyboard-kbd "D") lispy-extract-defun "extract defun")
    ;;         (,(cae-keyboard-kbd "e") lispy-edebug "edebug")
    ;;         (,(cae-keyboard-kbd "f") lispy-flatten "flatten")
    ;;         (,(cae-keyboard-kbd "F") lispy-let-flatten "let-flatten")
    ;;         ;; ("g" nil)
    ;;         (,(cae-keyboard-kbd "h") lispy-describe "describe")
    ;;         (,(cae-keyboard-kbd "i") lispy-to-ifs "to ifs")
    ;;         (,(cae-keyboard-kbd "j") lispy-debug-step-in "debug step in")
    ;;         (,(cae-keyboard-kbd "k") lispy-extract-block "extract block")
    ;;         (,(cae-keyboard-kbd "l") lispy-to-lambda "to lambda")
    ;;         (,(cae-keyboard-kbd "m") lispy-cursor-ace "multi cursor")
    ;;         (,(cae-keyboard-kbd "n") lispy-cd)
    ;;         ;; ("o" nil)
    ;;         (,(cae-keyboard-kbd "p") lispy-set-python-process "process")
    ;;         ;; ("q" nil)
    ;;         (,(cae-keyboard-kbd "r") lispy-eval-and-replace "eval and replace")
    ;;         (,(cae-keyboard-kbd "s") save-buffer)
    ;;         (,(cae-keyboard-kbd "t") lispy-view-test "view test")
    ;;         (,(cae-keyboard-kbd "u") lispy-unbind-variable "unbind let-var")
    ;;         (,(cae-keyboard-kbd "v") lispy-eval-expression "eval")
    ;;         (,(cae-keyboard-kbd "w") lispy-show-top-level "where")
    ;;         ;; ("x" nil)
    ;;         ;; ("y" nil)
    ;;         ;; ("z" nil)
    ;;         (,(cae-keyboard-kbd "B") lispy-store-region-and-buffer "store ,bounds")
    ;;         (,(cae-keyboard-kbd "R") lispy-reverse "reverse")
    ;;         (,(cae-keyboard-kbd "T") lispy-ert "ert")
    ;;         (">" lispy-toggle-thread-last "toggle last-threaded form")
    ;;         ("" lispy-x-more-verbosity :exit nil)
    ;;         ("?" lispy-x-more-verbosity "help" :exit nil)))
    ))

;;; Basically a custom input method

(use-package! aas
  :defer t :init
  (add-hook 'doom-first-input-hook #'aas-global-mode)
  :config
  (advice-add #'aas-embark-menu :before
              (cae-defun cae-aas-load-embark-h ()
                (require 'embark)))
  (aas-set-snippets 'global
    ";--" "—"
    ";-." "→"
    ";=." "⇒"
    ";!=" "≠"
    "-." "->"
    "=." "=>"
    (concat "g_dan" (cae-keyboard-kbd "7")) (cmd! () (cae-gpt-insert-dan7-prompt))
    "g_dan7" (cmd! () (cae-gpt-insert-dan7-prompt))
    "g_maximus" (cmd! () (cae-gpt-insert-maximus-prompt))))

;;; Programming language specific stuff

(use-package! smart-semicolon
  :defer t :init
  (add-hook 'c-mode-common-hook #'smart-semicolon-mode)
  (add-hook 'web-mode-hook  'smart-semicolon-mode)
  (add-hook 'java-mode-hook 'smart-semicolon-mode))

(use-package! electric-spacing
  :defer t :init
  (add-hook 'c-mode-common-hook #'electric-spacing-mode)
  (add-hook 'python-mode-hook #'electric-spacing-mode)
  :config
  (advice-add #'electric-spacing-\( :override #'cae-keyboard-electric-spacing-\())
