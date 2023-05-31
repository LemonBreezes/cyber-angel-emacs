;;; private/debugger/+gud-bindings.el -*- lexical-binding: t; -*-

(defvar-keymap gud-global-map
  "C-l" #'gud-refresh)

(defmacro cae-gud-global-def (func cmd key &optional doc)
  "Define FUNC to be a command sending CMD and bound to KEY, with
optional doc string DOC.  Certain %-escapes in the string arguments
are interpreted specially if present.  These are:

  %f -- Name (without directory) of current source file.
  %F -- Name (without directory or extension) of current source file.
  %d -- Directory of current source file.
  %l -- Number of current source line.
  %e -- Text of the C lvalue or function-call expression surrounding point.
  %a -- Text of the hexadecimal address surrounding point.
  %p -- Prefix argument to the command (if any) as a number.
  %c -- Fully qualified class name derived from the expression
        surrounding point (jdb only).

  The `current' source file is the file of the current buffer (if
we're in a C file) or the source file current at the last break or
step (if we're in the GUD buffer).
  The `current' line is that of the current buffer (if we're in a
source file) or the source line number at the last break or step (if
we're in the GUD buffer)."
  `(progn
     (defalias ',func (lambda (arg)
                        ,@(if doc (list doc))
                        (interactive "p")
                        (if (not gud-running)
	                    ,(if (stringp cmd)
	                         `(gud-call ,cmd arg)
	                       ;; Unused lexical warning if cmd does not use "arg".
	                       cmd))))
     ,(if key `(define-key gud-global-map ,key #',func))))

(cae-gud-global-def gud-tbreak "tbreak %f:%l" "\C-t"
                    "Set temporary breakpoint at current line.")
(cae-gud-global-def gud-jump
                    (progn (gud-call "tbreak %f:%l" arg) (gud-call "jump %f:%l"))
                    "\C-j" "Set execution address to current line.")

(cae-gud-global-def gud-up     "up %p"     "<" "Up N stack frames (numeric arg).")
(cae-gud-global-def gud-down   "down %p"   ">" "Down N stack frames (numeric arg).")
(cae-gud-global-def gud-print  "print %e"  "\C-p" "Evaluate C expression at point.")
(cae-gud-global-def gud-pstar  "print* %e" nil
                    "Evaluate C dereferenced pointer expression at point.")

(cae-gud-global-def gud-step   (gdb-gud-context-call "-exec-step" "%p" t)
                    "\C-s"
                    "Step one source line with display.")
(cae-gud-global-def gud-stepi  (gdb-gud-context-call "-exec-step-instruction" "%p" t)
                    "\C-i"
                    "Step one instruction with display.")
(cae-gud-global-def gud-next   (gdb-gud-context-call "-exec-next" "%p" t)
                    "\C-n"
                    "Step one line (skip functions).")
(cae-gud-global-def gud-nexti  (gdb-gud-context-call "-exec-next-instruction" "%p" t)
                    nil
                    "Step one instruction (skip functions).")
(cae-gud-global-def gud-cont   (gdb-gud-context-call "-exec-continue")
                    "\C-r"
                    "Continue with display.")
(cae-gud-global-def gud-finish (gdb-gud-context-call "-exec-finish" nil t)
                    "\C-f"
                    "Finish executing current function.")
(cae-gud-global-def gud-run    "-exec-run"
                    nil
                    "Run the program.")

(cae-gud-global-def gud-break (if (not (string-match "Disassembly" mode-name))
                                  (gud-call "break %f:%l" arg)
                                (save-excursion
                                  (beginning-of-line)
                                  (forward-char 2)
                                  (gud-call "break *%a" arg)))
                    "\C-b" "Set breakpoint at current line or address.")

(cae-gud-global-def gud-remove (if (not (string-match "Disassembly" mode-name))
                                   (gud-call "clear %f:%l" arg)
                                 (save-excursion
                                   (beginning-of-line)
                                   (forward-char 2)
                                   (gud-call "clear *%a" arg)))
                    "\C-d" "Remove breakpoint at current line or address.")

;; -exec-until doesn't support --all yet
(cae-gud-global-def gud-until  (if (not (string-match "Disassembly" mode-name))
                                   (gud-call "-exec-until %f:%l" arg)
                                 (save-excursion
                                   (beginning-of-line)
                                   (forward-char 2)
                                   (gud-call "-exec-until *%a" arg)))
                    "\C-u" "Continue to current line or address.")
(cae-gud-global-def
 gud-go (progn
          (when arg
            (gud-call (concat "-exec-arguments "
                              (read-string "Arguments to exec-run: "))))
          (gud-call
           (if gdb-active-process
               (gdb-gud-context-command "-exec-continue")
             "-exec-run")))
 "\C-v" "Start or continue execution.  Use a prefix to specify arguments.")
