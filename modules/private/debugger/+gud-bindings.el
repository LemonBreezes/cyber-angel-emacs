;;; private/debugger/+gud-bindings.el -*- lexical-binding: t; -*-

(after! gud
  (gud-def gud-tbreak "tbreak %f:%l" "\C-t"
           "Set temporary breakpoint at current line.")
  (gud-def gud-jump
           (progn (gud-call "tbreak %f:%l" arg) (gud-call "jump %f:%l"))
           "\C-j" "Set execution address to current line.")

  (gud-def gud-up     "up %p"     "<" "Up N stack frames (numeric arg).")
  (gud-def gud-down   "down %p"   ">" "Down N stack frames (numeric arg).")
  (gud-def gud-print  "print %e"  "\C-p" "Evaluate C expression at point.")
  (gud-def gud-pstar  "print* %e" nil
           "Evaluate C dereferenced pointer expression at point.")

  (gud-def gud-step   (gdb-gud-context-call "-exec-step" "%p" t)
           "\C-s"
           "Step one source line with display.")
  (gud-def gud-stepi  (gdb-gud-context-call "-exec-step-instruction" "%p" t)
           "\C-i"
           "Step one instruction with display.")
  (gud-def gud-next   (gdb-gud-context-call "-exec-next" "%p" t)
           "\C-n"
           "Step one line (skip functions).")
  (gud-def gud-nexti  (gdb-gud-context-call "-exec-next-instruction" "%p" t)
           nil
           "Step one instruction (skip functions).")
  (gud-def gud-cont   (gdb-gud-context-call "-exec-continue")
           "\C-r"
           "Continue with display.")
  (gud-def gud-finish (gdb-gud-context-call "-exec-finish" nil t)
           "\C-f"
           "Finish executing current function.")
  (gud-def gud-run    "-exec-run"
           nil
           "Run the program.")

  (gud-def gud-break (if (not (string-match "Disassembly" mode-name))
                         (gud-call "break %f:%l" arg)
                       (save-excursion
                         (beginning-of-line)
                         (forward-char 2)
                         (gud-call "break *%a" arg)))
           "\C-b" "Set breakpoint at current line or address.")

  (gud-def gud-remove (if (not (string-match "Disassembly" mode-name))
                          (gud-call "clear %f:%l" arg)
                        (save-excursion
                          (beginning-of-line)
                          (forward-char 2)
                          (gud-call "clear *%a" arg)))
           "\C-d" "Remove breakpoint at current line or address.")

  ;; -exec-until doesn't support --all yet
  (gud-def gud-until  (if (not (string-match "Disassembly" mode-name))
                          (gud-call "-exec-until %f:%l" arg)
                        (save-excursion
                          (beginning-of-line)
                          (forward-char 2)
                          (gud-call "-exec-until *%a" arg)))
           "\C-u" "Continue to current line or address.")
  (gud-def
   gud-go (progn
            (when arg
              (gud-call (concat "-exec-arguments "
                                (read-string "Arguments to exec-run: "))))
            (gud-call
             (if gdb-active-process
                 (gdb-gud-context-command "-exec-continue")
               "-exec-run")))
   "\C-v" "Start or continue execution.  Use a prefix to specify arguments."))
