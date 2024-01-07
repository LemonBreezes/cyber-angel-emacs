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
        t))

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

;;; Lispy

(when (modulep! :editor lispy)
  (after! lispy
    ;; navigation
    (unless (boundp 'cae-keyboard--lispy-mode-map-backup)
      (defvar cae-keyboard--lispy-mode-map-backup (copy-keymap lispy-mode-map))
      (defvar cae-lispy-hydra--command-column-alist
        '((special-lispy-right . "")
          (special-lispy-left . "")
          (special-lispy-flow . "")
          (special-lispy-down . "")
          (special-lispy-up . "")
          (special-lispy-different . "Move")
          (special-lispy-goto-mode . "Other")
          (special-lispy-eval-other-window . "Eval")
          (special-lispy-paste . "Edit")
          (special-lispy-occur . "Move")
          (special-lh-knight/body . "Move")
          (special-lispy-outline-next . "")
          (special-lispy-outline-prev . "")
          (special-lispy-outline-goto-child . "")
          (special-lispy-raise . "Edit")
          (special-lispy-raise-some . "Edit")
          (special-lispy-convolute . "Edit")
          (special-lispy-convolute-left . "Edit")
          (special-lispy-move-up . "Edit")
          (special-lispy-move-down . "Edit")
          (special-lispy-oneline . "Edit")
          (special-lispy-alt-multiline . "Edit")
          (special-lispy-stringify . "Edit")
          (special-lispy-ace-symbol . "Move")
          (special-lispy-ace-symbol-replace . "Edit")
          (special-lispy-mark-list . "Select")
          (special-lispy-eval . "Eval")
          
          (special-lispy-goto-local . "Move")
          (special-lispy-goto . "Move")
          (special-lispy-follow . "Move")
          (special-pop-tag-mark . "Move")
          (special-lispy-beginning-of-defun . "Move")
          (special-lispy-tab . "Edit")
          (special-lispy-shifttab . "View")
          (special-lispy-narrow . "View")
          (special-lispy-widen . "View")
          (special-lispy-clone . "Edit")
          (special-lispy-undo . "Other")
          (special-lispy-ace-paren . "Move")
          (special-lispy-ace-char . "Move")
          (special-lispy-ace-subword "Select")
          (special-lispy-view . "View")
          (special-lispy-teleport . "Edit")
          (special-lispy-new-copy . "Select")
          (special-lispy-back . "Move")
          (special-lispy-ediff-regions . "View")
          (special-lispy-x . "Other")
          (special-lispy-edebug-stop . "Other")
          (special-lispy-visit . "Other")
          (special-lispy-slurp . "")
          (special-lispy-barf . "")
          (special-lispy-repeat . "Other"))))
    (let* ((cae-keyboard-orbits cae-keyboard-orbits-for-lispy)
           (bindings
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
              (,(if (not (= 1 (length (cl-find ?\;
                                               cae-keyboard-orbits
                                               :test (lambda (x y) (cl-find x y))))))
                    (cae-keyboard-kbd ";")
                  ";")
               nil "")
              (";" lispy-comment "")
              ("+" special-lispy-join "")
              ("/" special-lispy-splice "")
              ("-" special-lispy-ace-subword "Select")
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
                          :column ,(caddr binding))))
       t))
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
             (,(cae-keyboard-kbd "z") nil))
          t)
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
             (,(cae-keyboard-kbd "e") lispy-goto-elisp-commands)))
          t)
    (eval `(lispy-defverb
            "other"
            ((,(cae-keyboard-kbd "h") lispy-move-left)
             (,(cae-keyboard-kbd "j") lispy-down-slurp)
             (,(cae-keyboard-kbd "k") lispy-up-slurp)
             (,(cae-keyboard-kbd "l") lispy-move-right)
             ("SPC" lispy-other-space)
             (,(cae-keyboard-kbd "g") lispy-goto-mode)))
          t)
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
    ;;         ("?" lispy-x-more-verbosity "help" :exit nil))
    ;;      t)
    ))

;;; Worf
(after! worf
  ;; So E is the same as % in my layout because E is not bound in
  ;; `worf-mode-map'. I can use E to set the priority of the current heading.
  (worf-define-key worf-mode-map "E" #'org-priority)
  (defvar cae-keyboard--worf-mode-map-backup (copy-keymap worf-mode-map))
  (let ((bindings
         `((,(cae-keyboard-kbd "j") wspecial-worf-down "")
           (,(cae-keyboard-kbd "k") wspecial-worf-up "")
           (,(cae-keyboard-kbd "h") wspecial-worf-left "")
           (,(cae-keyboard-kbd "l") wspecial-worf-right "")
           (,(cae-keyboard-kbd "f") wspecial-hydra-worf-f/body "Move")
           (,(cae-keyboard-kbd "J") wspecial-outline-next-visible-heading "")
           (,(cae-keyboard-kbd "K") wspecial-outline-previous-visible-heading "")
           (,(cae-keyboard-kbd "g") wspecial-worf-goto "Move")
           (,(cae-keyboard-kbd "o") wspecial-worf-ace-link "Move") ;Visit Link
           (,(cae-keyboard-kbd "O") wspecial-worf-ace-link-eww "Move") ;Visit in EWW
           (,(cae-keyboard-kbd "b") wspecial-worf-back-link "Move")
           (,(cae-keyboard-kbd "i") wspecial-worf-tab "Hide/Show")
           (,(cae-keyboard-kbd "/") wspecial-worf-tab-contents "Hide/Show")
           (,(cae-keyboard-kbd "I") wspecial-worf-shifttab "Hide/Show")
           (,(cae-keyboard-kbd "v") wspecial-worf-view "Hide/Show")
           (,(cae-keyboard-kbd "F") wspecial-worf-attach-visit "Files")
           (,(cae-keyboard-kbd "A") wspecial-worf-attach "Files")
           (,(cae-keyboard-kbd "V") wspecial-worf-visit "Files")
           (,(cae-keyboard-kbd "r") wspecial-worf-hydra-refile/body "Files")
           (,(cae-keyboard-kbd "x") wspecial-hydra-worf-promote/body "Misc")
           (,(cae-keyboard-kbd "L") wspecial-worf-copy-heading-id "Misc")
           (,(cae-keyboard-kbd "a") wspecial-worf-add "Misc")
           (,(cae-keyboard-kbd "e") wspecial-worf-eval "Misc")
           (,(cae-keyboard-kbd "E") wspecial-org-priority "Misc")
           (,(cae-keyboard-kbd "s") wspecial-worf-save "Misc")
           (,(cae-keyboard-kbd "N") wspecial-org-narrow-to-subtree "Narrow/Widen")
           (,(cae-keyboard-kbd "W") wspecial-widen "Narrow/Widen")
           (,(cae-keyboard-kbd "c") wspecial-hydra-worf-change/body "Verbs")
           (,(cae-keyboard-kbd "d") wspecial-worf-delete-mode "Verbs")
           (,(cae-keyboard-kbd "y") wspecial-worf-occur "Verbs")
           (,(cae-keyboard-kbd "C") wspecial-worf-clock-mode "Verbs")
           (,(cae-keyboard-kbd "T") wspecial-worf-clock-in-and-out "Verbs")
           (,(cae-keyboard-kbd "w") wspecial-worf-keyword-mode "Verbs")
           (,(cae-keyboard-kbd "m") worf-mark "Verbs")
           (,(cae-keyboard-kbd "q") wspecial-worf-quit "Verbs")
           (,(cae-keyboard-kbd "n") wspecial-worf-new-copy "Verbs")
           (,(cae-keyboard-kbd "p") wspecial-worf-property "Nouns")
           (,(cae-keyboard-kbd "P") wspecial-worf-paste "Nouns")
           ;;(,(cae-keyboard-kbd "P") wspecial-org-priority "Misc")
           (,(cae-keyboard-kbd "t") wspecial-worf-todo "Misc")
           (,(cae-keyboard-kbd "u") wspecial-undo "Misc")
           (,(cae-keyboard-kbd "R") wspecial-worf-recenter-mode))))
    (dolist (binding bindings)
      (define-key worf-mode-map (car binding) (cadr binding)))
    (eval
     (append '(defhydra cae-worf-cheat-sheet (:hint nil :foreign-keys run)
                ("<f6>" nil "Exit" :exit t))
             (cl-loop for binding in bindings
                      unless (string= (caddr binding) "")
                      collect
                      `(,(car binding)
                        ,(cadr binding)
                        ,(thread-last (symbol-name (cadr binding))
                                      (string-remove-prefix "wspecial-")
                                      (string-remove-prefix "worf-")
                                      (string-remove-prefix "org-")
                                      (string-remove-prefix "outline-"))
                        :column ,(caddr binding))))
     t))
  (define-key worf-mode-map (kbd "<f6>") #'cae-worf-cheat-sheet/body)
  (when (modulep! :editor multiple-cursors)
    (after! multiple-cursors-core
      (add-to-list 'mc/cmds-to-run-once #'worf-lispy-cheat-sheet/body)
      (add-to-list 'mc/cmds-to-run-once #'worf-lispy-cheat-sheet/nil)))
  (eval
   `(defhydra hydra-worf-change (:idle 1.0
                                 :hint nil)
      ,(cae-keyboard-remap-hydra-hint
        "
^ ^ _w_ ^ ^    _t_ags    _p_rop    _r_: shiftcontrol
_h_ ^+^ _l_    _n_ame    _e_dit    _i_: shift
^ ^ _s_ ^ ^    _a_dd     _T_ime    _f_: shiftmeta (tree)")
      ;; arrows
      (,(cae-keyboard-kbd "j") worf-down :exit t)
      (,(cae-keyboard-kbd "k") worf-up :exit t)
      (,(cae-keyboard-kbd "w") org-metaup)
      (,(cae-keyboard-kbd "s") org-metadown)
      (,(cae-keyboard-kbd "h") org-metaleft)
      (,(cae-keyboard-kbd "l") org-metaright)
      (,(cae-keyboard-kbd "e") move-end-of-line :exit t)
      ;; modes
      (,(cae-keyboard-kbd "f") worf-change-tree-mode :exit t)
      (,(cae-keyboard-kbd "i") worf-change-shift-mode :exit t)
      (,(cae-keyboard-kbd "r") worf-change-shiftcontrol-mode :exit t)
      ;; misc
      (,(cae-keyboard-kbd "p") org-set-property :exit t)
      (,(cae-keyboard-kbd "t") org-set-tags-command :exit t)
      (,(cae-keyboard-kbd "T") worf-change-time :exit t)
      (,(cae-keyboard-kbd "n") worf-change-name :exit t)
      (,(cae-keyboard-kbd "a") org-meta-return :exit t)
      (,(cae-keyboard-kbd "o") hydra-worf-keyword/body :exit t)
      (,(cae-keyboard-kbd "m") worf-archive-and-commit :exit t)
      (,(cae-keyboard-kbd "q") nil)
      (,(cae-keyboard-kbd "c") nil))
   t)
  (eval
   `(worf-defverb
     "change"
     '((,(cae-keyboard-kbd "j") org-metadown)
       (,(cae-keyboard-kbd "k") org-metaup)
       (,(cae-keyboard-kbd "h") org-metaleft)
       (,(cae-keyboard-kbd "l") org-metaright)
       (,(cae-keyboard-kbd "t") org-set-tags-command :disable)
       (,(cae-keyboard-kbd "n") worf-change-name :disable :break)
       (,(cae-keyboard-kbd "a") org-meta-return :disable :break)))
   t)
  (eval
   `(worf-defverb
     "change-tree"
     '((,(cae-keyboard-kbd "j") org-shiftmetadown)
       (,(cae-keyboard-kbd "k") org-shiftmetaup)
       (,(cae-keyboard-kbd "h") org-shiftmetaleft)
       (,(cae-keyboard-kbd "l") org-shiftmetaright)))
   t)
  (eval
   `(worf-defverb
     "change-shift"
     '((,(cae-keyboard-kbd "j") org-shiftdown)
       (,(cae-keyboard-kbd "k") org-shiftup)
       (,(cae-keyboard-kbd "h") org-shiftleft)
       (,(cae-keyboard-kbd "l") org-shiftright)))
   t)
  (eval
   `(worf-defverb
     "change-shiftcontrol"
     '((,(cae-keyboard-kbd "j") org-shiftcontroldown)
       (,(cae-keyboard-kbd "k") org-shiftcontrolup)
       (,(cae-keyboard-kbd "h") org-shiftcontrolleft)
       (,(cae-keyboard-kbd "l") org-shiftcontrolright)))
   t)
  (eval
   `(worf-defverb
     "delete"
     '((,(cae-keyboard-kbd "p") org-delete-property :disable)
       (,(cae-keyboard-kbd "k") worf-delete-k :disable)
       (,(cae-keyboard-kbd "j") worf-cut-subtree :disable)
       (,(cae-keyboard-kbd "w") worf-delete-w :disable)
       (,(cae-keyboard-kbd "n") worf-delete-name :disable)))
   t)
  (eval
   `(worf-defverb
     "yank"
     '((,(cae-keyboard-kbd "j") org-copy-subtree :disable)))
   t)
  (eval
   `(defhydra worf-new (:exit t)
      (,(cae-keyboard-kbd "j") worf-new-down)
      (,(cae-keyboard-kbd "k") org-insert-heading)
      (,(cae-keyboard-kbd "h") org-metaleft)
      (,(cae-keyboard-kbd "l") worf-new-right))
   t))

;; TODO Rebind Evil commands.

;; Ideally, we would undefine the C-j, C-k, etc keys and remap them for their
;; corresponding keyboard layout but because some control keybindings are
;; identified with RET, ESC, etc, that is not possible in terminal Emacs.

