;;; private/exwm/autoload/evil.el -*- lexical-binding: t; -*-

(defvar +exwm-refocus-application--message "")
(defvar +exwm-refocus-application--delays '(0.015 0.03))
(defvar +exwm-refocus-application--timer nil)
(defvar +exwm-refocus-application--last-time 0)
(defvar +exwm-refocus-application--last-state nil)

(defvar +exwm-vanilla-emacs-config-dir
  (concat doom-user-dir "vanilla-emacs-configs/"))
(defvar +exwm-vanilla-emacs--config-history nil)
(defvar +exwm-vanilla-doom-emacs-config-dir
  (concat doom-user-dir "vanilla-doom-emacs-configs/"))
(defvar +exwm-vanilla-doom-emacs--config-history nil)

;;;###autoload
(defun +exwm-refocus-application (&rest _)
  "Refocus input for the currently selected EXWM buffer, if any."
  (when (and (derived-mode-p 'exwm-mode)
             (not (memq +exwm-refocus-application--timer timer-list))
             (> (float-time)
                (+ +exwm-refocus-application--last-time
                   0.01
                   (cl-reduce #'+ +exwm-refocus-application--delays))))
    (run-at-time (nth 0 +exwm-refocus-application--delays)
                 nil #'+exwm-refocus-application--timer)))

(defun +exwm-refocus-application--timer ()
  (when (derived-mode-p 'exwm-mode)
    (setq +exwm-refocus-application--message (current-message))
    (let ((+exwm-refocus-application--last-state (bound-and-true-p evil-state)))
      (minibuffer-with-setup-hook
          #'+exwm-refocus-application-minibuffer-quit-timer
        (read-string "")))))

(defun +exwm-refocus-application-minibuffer-quit-timer ()
  (setq +exwm-refocus-application--timer
        (run-at-time (nth 1 +exwm-refocus-application--delays) nil
                     (lambda ()
                       (run-at-time
                        0.0 nil
                        (lambda ()
                          (when +exwm-refocus-application--message
                            (minibuffer-message +exwm-refocus-application--message))
                          (pcase +exwm-refocus-application--last-state
                            ('insert (exwm-evil-core-insert))
                            ('normal (exwm-evil-core-normal))
                            (_ nil))))
                       (when (minibufferp nil t)
                         (setq +exwm-refocus-application--last-time (float-time))
                         (throw 'exit nil))))))
;;;###autoload
(defun +exwm-open-nested-emacs (arg)
  "Open a separate GUI instance of Emacs. If ARG is non-nil, debug init as
well."
  (interactive "P")
  (apply #'start-process "Emacs" nil "emacs"
         (when arg (list "--debug-init"))))

;;;###autoload
(defun +exwm-open-nested-vanilla-emacs (arg)
  "Open a separate GUI instance of Emacs with a vanilla config. If ARG is
non-nil, debug init as well."
  (interactive "P")
  (apply #'start-process "Emacs" nil "emacs" "-Q"
         "-l"
         (expand-file-name
          (completing-read "Load file: "
                           (seq-filter (lambda (f)
                                         (not (string-prefix-p "flycheck_" f)))
                                       (directory-files +exwm-vanilla-emacs-config-dir nil "^[^.]"))
                           nil t nil '+exwm-vanilla-emacs--config-history)
          +exwm-vanilla-emacs-config-dir)
         (when arg (list "--debug-init"))))

;;;###autoload
(defun +exwm-open-nested-vanilla-doom-emacs (arg)
  "Open a separate GUI instance of Doom Emacs. If ARG is non-nil, debug init"
  (interactive "P"))
