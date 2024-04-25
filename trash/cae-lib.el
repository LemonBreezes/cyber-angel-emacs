;;; trash/cae-lib.el -*- lexical-binding: t; -*-

(defun cae-posframe-message (msg &rest args)
  "Display MSG in a posframe."
  (let ((buffer "*posframe-message*"))
    (posframe-delete buffer)
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (insert msg)
      (posframe-show buffer
                     :string (buffer-string)
                     :poshandler #'posframe-poshandler-point-frame-center))))
;; (cae-posframe-message "Hello, world!")

(defun cae-check-processes-async (process-list callback)
  "Check asynchronously if none of the processes in PROCESS-LIST are running and then call CALLBACK."
  (let ((buffer (generate-new-buffer " *check-proc-async*")))
    (set-process-sentinel
     (apply #'start-process " *check-proc-async*" buffer "pidof" process-list)
     (lambda (_process _event)
       (unwind-protect
           (funcall callback (= (buffer-size buffer) 0)) ; if buffer is empty, no processes are running
         (kill-buffer buffer)))))
  nil)

(defmacro cae-when-none-of-these-processes-running (process-list short-circuit-form &rest args)
  "Evaluate ARGS if SHORT-CIRCUIT-FORM is true or if none of the processes in PROCESS-LIST are running.

PROCESS-LIST should be a list of strings, each string being the name of a process to check.
SHORT-CIRCUIT-FORM is an optional form. If it evaluates to non-nil, then ARGS are evaluated without checking the processes.
If SHORT-CIRCUIT-FORM is nil or evaluates to nil, then process checking occurs: ARGS are evaluated only if it is determined that none of the processes listed in PROCESS-LIST are currently running.

Usage:
(cae-when-none-of-these-processes-running
  '(\"process1\" \"process2\")            ; PROCESS-LIST
  (display-graphic-p)                     ; SHORT-CIRCUIT-FORM, an example condition
  (message \"No specified processes are running, or we are in a graphical display.\")) ; ARGS to be evaluated

In this example, if Emacs is running in a graphical display (meaning `display-graphic-p' returns non-nil), the message is displayed immediately.
Otherwise, the message is displayed only if neither \"process1\" nor \"process2\" are running at the time of the check."
  `(if ,short-circuit-form
       (progn ,@args)
     (cae-check-processes-async ',process-list
                                (lambda (none-running)
                                  (when none-running
                                    ,@args)))))
