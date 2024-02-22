;;; lisp/cae-lib.el -*- lexical-binding: t; -*-

(defun cae-ignore-errors-a (fun &rest args)
  "Ignore errors in FUN with ARGS."
  (ignore-errors (apply fun args)))

(defun cae-display-graphic-p ()
  (or (display-graphic-p)
      (daemonp)))

(defun cae-tty-disable-unicode-p ()
  (not (cae-display-graphic-p)))

;; Lazy load keymaps from packages.
(defmacro cae-oneshot-keymap (keymap package)
  `(if (featurep ',package)
       (symbol-value ',keymap)
     (lambda () (interactive)
       (and ',package (require ',package))
       (let* ((once t)
              (timer
               (when (featurep 'which-key)
                 (run-with-idle-timer
                  which-key-idle-delay nil
                  (lambda ()
                    (when once
                      (let ((which-key-show-prefix t))
                        (which-key--show-keymap
                         (symbol-name ',keymap)
                         ,keymap
                         nil nil t))))))))
         (set-transient-map (symbol-value ',keymap)
                            (lambda ()
                              (prog1 once
                                (setq once nil)))
                            (lambda ()
                              (cancel-timer timer)))))))

;; A generic adviser for responding yes to yes or no prompts automatically.
(defun cae-always-yes-a (oldfun &rest args)
  (cl-letf (((symbol-function #'yes-or-no-p) (symbol-function #'always))
            ((symbol-function #'y-or-n-p) (symbol-function #'always)))
    (apply oldfun args)))

;; For shutting up noisy functions.
(defun cae-shut-up-a (oldfun &rest args)
  (advice-add #'message :override #'ignore)
  (unwind-protect (apply oldfun args)
    (advice-remove #'message #'ignore)))

(defun cae-posframe-message (msg &rest args)
  "Display MSG in a posframe."
  (let ((buffer "*posframe-message*"))
    (posframe-delete buffer)
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (insert msg)
      (posframe-show buffer
                     :string (buffer-string)
                     :position (point)))))
