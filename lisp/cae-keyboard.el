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
(define-key key-translation-map (cae-keyboard-kbd "C-x w" "0") (kbd "C-x w 0"))
(define-key key-translation-map (cae-keyboard-kbd "C-x w" "1") (kbd "C-x w 1"))
(define-key key-translation-map (cae-keyboard-kbd "C-x w" "2") (kbd "C-x w 2"))
(define-key key-translation-map (cae-keyboard-kbd "C-x w" "3") (kbd "C-x w 3"))
(define-key key-translation-map (cae-keyboard-kbd "C-x w" "4") (kbd "C-x w 4"))
(define-key key-translation-map (cae-keyboard-kbd "C-x w" "5") (kbd "C-x w 5"))
(define-key key-translation-map (cae-keyboard-kbd "C-x w" "6") (kbd "C-x w 6"))
(define-key key-translation-map (cae-keyboard-kbd "C-x w" "7") (kbd "C-x w 7"))
(define-key key-translation-map (cae-keyboard-kbd "C-x w" "8") (kbd "C-x w 8"))
(define-key key-translation-map (cae-keyboard-kbd "C-x w" "9") (kbd "C-x w 9"))

;; My custom C-z prefix.
(define-key key-translation-map (cae-keyboard-kbd "C-z" "0") (kbd "C-z 0"))
(define-key key-translation-map (cae-keyboard-kbd "C-z" "1") (kbd "C-z 1"))
(define-key key-translation-map (cae-keyboard-kbd "C-z" "2") (kbd "C-z 2"))
(define-key key-translation-map (cae-keyboard-kbd "C-z" "3") (kbd "C-z 3"))
(define-key key-translation-map (cae-keyboard-kbd "C-z" "4") (kbd "C-z 4"))
(define-key key-translation-map (cae-keyboard-kbd "C-z" "5") (kbd "C-z 5"))
(define-key key-translation-map (cae-keyboard-kbd "C-z" "6") (kbd "C-z 6"))
(define-key key-translation-map (cae-keyboard-kbd "C-z" "7") (kbd "C-z 7"))
(define-key key-translation-map (cae-keyboard-kbd "C-z" "8") (kbd "C-z 8"))
(define-key key-translation-map (cae-keyboard-kbd "C-z" "9") (kbd "C-z 9"))
(define-key key-translation-map (kbd "C-z C-0") (kbd "C-z 0"))
(define-key key-translation-map (kbd "C-z C-1") (kbd "C-z 1"))
(define-key key-translation-map (kbd "C-z C-2") (kbd "C-z 2"))
(define-key key-translation-map (kbd "C-z C-3") (kbd "C-z 3"))
(define-key key-translation-map (kbd "C-z C-4") (kbd "C-z 4"))
(define-key key-translation-map (kbd "C-z C-5") (kbd "C-z 5"))
(define-key key-translation-map (kbd "C-z C-6") (kbd "C-z 6"))
(define-key key-translation-map (kbd "C-z C-7") (kbd "C-z 7"))
(define-key key-translation-map (kbd "C-z C-8") (kbd "C-z 8"))
(define-key key-translation-map (kbd "C-z C-9") (kbd "C-z 9"))
(define-key key-translation-map
  (kbd (concat "C-z C-" (cae-keyboard-kbd "0"))) (kbd "C-z 0"))
(define-key key-translation-map
  (kbd (concat "C-z C-" (cae-keyboard-kbd "1"))) (kbd "C-z 1"))
(define-key key-translation-map
  (kbd (concat "C-z C-" (cae-keyboard-kbd "2"))) (kbd "C-z 2"))
(define-key key-translation-map
  (kbd (concat "C-z C-" (cae-keyboard-kbd "3"))) (kbd "C-z 3"))
(define-key key-translation-map
  (kbd (concat "C-z C-" (cae-keyboard-kbd "4"))) (kbd "C-z 4"))
(define-key key-translation-map
  (kbd (concat "C-z C-" (cae-keyboard-kbd "5"))) (kbd "C-z 5"))
(define-key key-translation-map
  (kbd (concat "C-z C-" (cae-keyboard-kbd "6"))) (kbd "C-z 6"))
(define-key key-translation-map
  (kbd (concat "C-z C-" (cae-keyboard-kbd "7"))) (kbd "C-z 7"))
(define-key key-translation-map
  (kbd (concat "C-z C-" (cae-keyboard-kbd "8"))) (kbd "C-z 8"))
(define-key key-translation-map
  (kbd (concat "C-z C-" (cae-keyboard-kbd "9"))) (kbd "C-z 9"))
(define-key key-translation-map (kbd "C-z C-a") (kbd "C-z a"))
(define-key key-translation-map (kbd "C-z C-b") (kbd "C-z b"))
(define-key key-translation-map (kbd "C-z C-c") (kbd "C-z c"))
(define-key key-translation-map (kbd "C-z C-d") (kbd "C-z d"))
(define-key key-translation-map (kbd "C-z C-e") (kbd "C-z e"))
(define-key key-translation-map (kbd "C-z C-f") (kbd "C-z f"))
(define-key key-translation-map (kbd "C-z C-g") (kbd "C-z g"))
(define-key key-translation-map (kbd "C-z C-h") (kbd "C-z h"))
(define-key key-translation-map (kbd "C-z C-i") (kbd "C-z i"))
(define-key key-translation-map (kbd "C-z C-j") (kbd "C-z j"))
(define-key key-translation-map (kbd "C-z C-k") (kbd "C-z k"))
(define-key key-translation-map (kbd "C-z C-l") (kbd "C-z l"))
(define-key key-translation-map (kbd "C-z C-m") (kbd "C-z m"))
(define-key key-translation-map (kbd "C-z C-n") (kbd "C-z n"))
(define-key key-translation-map (kbd "C-z C-o") (kbd "C-z o"))
(define-key key-translation-map (kbd "C-z C-p") (kbd "C-z p"))
(define-key key-translation-map (kbd "C-z C-q") (kbd "C-z q"))
(define-key key-translation-map (kbd "C-z C-r") (kbd "C-z r"))
(define-key key-translation-map (kbd "C-z C-s") (kbd "C-z s"))
(define-key key-translation-map (kbd "C-z C-t") (kbd "C-z t"))
(define-key key-translation-map (kbd "C-z C-u") (kbd "C-z u"))
(define-key key-translation-map (kbd "C-z C-v") (kbd "C-z v"))
(define-key key-translation-map (kbd "C-z C-x") (kbd "C-z x"))
(define-key key-translation-map (kbd "C-z C-y") (kbd "C-z y"))
(define-key key-translation-map (kbd "C-z C-z") (kbd "C-z z"))

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

;; For Org mode
(define-key key-translation-map (cae-keyboard-kbd "C-c C-x" "0") (kbd "C-c C-x 0"))

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
           (kbd (concat "C-" (cae-keyboard-remap "~")))))))

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
             ,(cae-keyboard-kbd "0") #'cae-keyboard-digit-argument)
      t)
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
               ,(cae-keyboard-kbd "0") #'cae-keyboard-digit-argument)
        t)
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
   (or  (and (minibufferp)
             (not (bound-and-true-p vertico--input))
             (cl-member (minibuffer-prompt)
                        '("Go to line: "
                          "Go to char: "
                          "Move to column: ")
                        :test #'string-match-p))
        (and (featurep 'my-repo-pins)
             (get-buffer-window "my-repo-pins-ui-buffer"))
        (and (featurep 'ispell)
             (get-buffer-window ispell-choices-buffer))
        (and (symbol-file this-command)
             (cl-member (file-name-base (symbol-file this-command))
                        '("ace-window"
                          "tabgo")
                        :test #'string=))
        (and (not this-command)
             (minibufferp)
             (symbol-file last-command)
             (string= (file-name-base (symbol-file last-command)) "switch-window")))))

(dolist (key-from (mapcar #'char-to-string '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)))
  (cae-make-conditional-key-translation (cae-keyboard-kbd key-from)
                                        (kbd key-from)
                                        #'cae-translate-number-row-p))

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
    "=." "=>"))

;;; Programming language specific stuff

(use-package! smart-semicolon
  :defer t :init
  (add-hook 'c-mode-common-hook #'smart-semicolon-mode)
  (add-hook 'web-mode-hook  #'smart-semicolon-mode)
  (add-hook 'java-mode-hook #'smart-semicolon-mode)
  (add-hook 'js-mode-hook   #'smart-semicolon-mode))
