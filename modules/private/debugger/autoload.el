;;; private/debugger/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-debugger-dap-kill-all-sessions-and-restart ()
  (interactive)
  (dolist (buf (doom-visible-buffers))
    (if (string-match-p (format "^\\*compilation\\*<%s>"
                                (doom-project-name))
                        (buffer-name buf))
        (kill-buffer buf)
      (dolist (session (dap--get-sessions))
        (when (string-match-p (dap--debug-session-name session)
                              (buffer-name buf))
          (let ((window (get-buffer-window buf)))
            (when (not (eq window (frame-first-window)))
              (delete-window window)))))))
  (unwind-protect (dap-delete-all-sessions)
    (when-let ((workspace-project (cl-find (+workspace-current-name)
                                           (projectile-relevant-known-projects)
                                           :test #'string-match-p)))
      (unless (string= (projectile-project-name)
                       (+workspace-current-name))
        (projectile-switch-project-by-name workspace-project)))
    (dap-debug-last)))

;;;###autoload
(defun cae-debugger-quit-or-delete-or-send-eof (arg)
  (interactive "p")
  (if (and (eobp)
           (looking-back (concat dap-ui-repl-prompt "\\s-*") nil))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

;;;###autoload
(defun cae-debugger-open-repl ()
  (interactive)
  (let ((repl-buffer
         (or (get-buffer "*dap-ui-repl*")
             (progn (dap-ui-repl)
                    (let ((buf (get-buffer "*dap-ui-repl*")))
                      (bury-buffer buf)
                      buf)))))
    (with-current-buffer repl-buffer
      (doom-mark-buffer-as-real-h))
    (pop-to-buffer repl-buffer)))

;;;###autoload (autoload 'cae-debug-dap-hydra/body "private/debugger/autoload" t nil)
(defhydra cae-debug-dap-hydra (:color pink :hint nil :foreign-keys run)
  "
^Stepping^          ^Switch^                 ^Breakpoints^         ^Debug^                     ^Eval
^^^^^^^^----------------------------------------------------------------------------------------------------------------
_n_: Next           _ss_: Session            _bb_: Toggle          _dd_: Debug                 _ee_: Eval
_i_: Step in        _st_: Thread             _bd_: Delete          _dr_: Debug recent          _er_: Eval region
_o_: Step out       _sf_: Stack frame        _ba_: Add             _dl_: Debug last            _es_: Eval thing at point
_c_: Continue       _su_: Up stack frame     _bc_: Set condition   _de_: Edit debug template   _ea_: Add expression
_r_: Restart frame  _sd_: Down stack frame   _bh_: Set hit count   _ds_: Debug restart         _er_: Remove expression
_Q_: Disconnect     _sl_: List locals        _bl_: Set log message
_R_: Restart        _sb_: List breakpoints
                  _sS_: List sessions
"
  ("n" dap-next)
  ("i" dap-step-in)
  ("o" dap-step-out)
  ("c" dap-continue)
  ("r" dap-restart-frame)
  ("ss" dap-switch-session)
  ("st" dap-switch-thread)
  ("sf" dap-switch-stack-frame)
  ("su" dap-up-stack-frame)
  ("sd" dap-down-stack-frame)
  ("sl" dap-ui-locals)
  ("sb" dap-ui-breakpoints)
  ("sS" dap-ui-sessions)
  ("bb" dap-breakpoint-toggle)
  ("ba" dap-breakpoint-add)
  ("bd" dap-breakpoint-delete)
  ("bc" dap-breakpoint-condition)
  ("bh" dap-breakpoint-hit-condition)
  ("bl" dap-breakpoint-log-message)
  ("dd" dap-debug)
  ("dr" dap-debug-recent)
  ("ds" dap-debug-restart)
  ("dl" dap-debug-last)
  ("de" dap-debug-edit-template)
  ("ee" dap-eval)
  ("ea" dap-ui-expressions-add)
  ("er" dap-eval-region)
  ("es" dap-eval-thing-at-point)
  ("er" dap-ui-expressions-remove)
  ("q" nil "quit" :color blue)
  ("Q" dap-disconnect :color red)
  ("<f6>" nil "quit")
  ("R" cae-debugger-dap-kill-all-sessions-and-restart nil :color red))
