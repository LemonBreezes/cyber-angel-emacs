;;; preamble.el -*- lexical-binding: t; -*-

;; Print the features that a `doom-load' call ends up providing.
;;(cae-defadvice! cae-hacks-doom-load-report-features-a (fn &rest args)
;;  :around #'doom-load
;;  (let ((features-before features))
;;    (prog1 (apply fn args)
;;      (let ((new-features (cl-set-difference features features-before)))
;;        (when new-features
;;          (message "doom-load %s provided: %s"
;;                   (car args)
;;                   (mapconcat #'symbol-name (nreverse new-features) " ")))))))

(setq load-path-filter-function #'load-path-filter-cache-directory-files)

(defun remove-from-list (list-var element &optional append compare-fn)
  "Remove ELEMENT from the value of LIST-VAR if it is present.
The test for presence is done with `equal', or with COMPARE-FN if that's
non-nil.

This is the inverse of `add-to-list'. If LIST-VAR contains a list,
this function removes the first occurrence of ELEMENT from it.

The return value is the new value of LIST-VAR.

If optional argument APPEND is non-nil, and if ELEMENT is a cons cell,
then this function uses `eq' instead of `equal' to determine whether
ELEMENT is already present in LIST-VAR, and it only compares the car
of ELEMENT."
  (let ((lst (symbol-value list-var))
        (test-func (or compare-fn (if (and append (consp element)) #'eq #'equal))))
    (when lst
      (if (and append (consp element))
          ;; For cons cells with APPEND, we only compare the car
          (let ((car-element (car element)))
            (set list-var (delq (car (cl-member car-element lst
                                                :key #'car
                                                :test test-func))
                                lst)))
        ;; Normal case - remove the element if it exists
        (set list-var (cl-remove element lst :count 1 :test test-func))))
    (symbol-value list-var)))

(defvar cae-pdump--building nil
  "Non-nil while the pdump image is being built (set by cli.el).
`cae-after-frame!' checks this to decide whether to run its body now or defer it
to the first real frame at runtime.")

(defun cae-run-on-first-frame (fn)
  "Call FN once, on the first real frame.
On a daemon there is no usable frame until a client connects, so defer to
`server-after-make-frame-hook' (fires once the daemon is connected to);
otherwise the initial `-nw'/GUI frame already exists, so call FN immediately.
The `daemonp' test happens when this runs (at runtime), not when the dump is
built (always batch, never a daemon).  Self-removing in the daemon case."
  (if (daemonp)
      (letrec ((fn* (lambda (&rest _)
                      (remove-hook 'server-after-make-frame-hook fn*)
                      (funcall fn))))
        (add-hook 'server-after-make-frame-hook fn* t))
    (funcall fn)))

(defmacro cae-after-frame! (&rest body)
  "Run BODY now (a frame already exists), or — during the pdump build — defer it
to the first real frame at runtime so the display/WM/tty predicates see the
actual launch environment.

Deferral fires on the FIRST real frame via `cae-run-on-first-frame': at startup
for a `-nw'/GUI Emacs (the initial frame already exists by `emacs-startup-hook'
time), and on `server-after-make-frame-hook' for a daemon (once a client
connects).  Plain `after-make-frame-functions' would miss the initial frame of a
non-daemon Emacs entirely."
  `(if (bound-and-true-p cae-pdump--building)
       (add-hook 'emacs-startup-hook
                 (lambda () (cae-run-on-first-frame (lambda () ,@body)))
                 t)                     ; append → preserve load order
     ,@body))

(defmacro cae-before-frame! (&rest body)
  "Run BODY now, or — during the pdump build — defer it to `before-init-hook'
at runtime, so it runs OUTSIDE the dumped image (seeing the live environment,
e.g. `CAE_EXWM') and BEFORE the initial frame is created.

The counterpart to `cae-after-frame!': use it when BODY must *precede* the first
frame rather than follow it — enabling EXWM as the window manager is the case in
point.  `before-init-hook' fires inside `command-line' after `early-init.el' but
before `frame-initialize' (see startup.el), which is the earliest runtime point
a dumped hook can reach.  Predicates that pivot on the frame must therefore use
`initial-window-system' (set by then) rather than `(framep (selected-frame))'
\(still the initial terminal frame at this point).

When NOT building the dump, config loads during `init.el', after the initial
frame already exists, so BODY runs inline — the earliest point still reachable,
and EXWM simply takes over the existing frame.  Because the env-/frame-sensitive
decision is deferred into the dump as a hook, the image never bakes in the build
host's environment.  Intended for the non-daemon WM launch; a daemon has no
initial frame to precede."
  `(if (bound-and-true-p cae-pdump--building)
       (add-hook 'before-init-hook (lambda () ,@body) t) ; append → preserve order
     ,@body))

(defun cae-add-dir-to-path (dir)
  "Add DIR to the PATH environment variable and `exec-path` if it exists and isn't a duplicate."
  (let ((expanded-dir (expand-file-name dir)))
    (when (and (file-directory-p expanded-dir)
               (not (member expanded-dir exec-path)))
      (setenv "PATH" (concat expanded-dir ":" (getenv "PATH")))
      (push expanded-dir exec-path))))

(let ((paths-to-add
       (list
        "~/.nix-profile/bin"
        "~/.config/bin"
        "/usr/lib/ccache/bin"
        "~/.local/bin"
        "~/.cargo/bin"
        "~/go/bin"
        "~/.foundry/bin"
        "~/.config/emacs/bin"
        "~/.huff/bin"
        "~/.elan/bin"
        "~/.npm-packages/bin"
        "~/.ghcup/bin"
        "~/.opencode/bin"
        (when (string-equal system-type "android")
          "/data/data/com.termux/files/usr/bin")
        (when (getenv "WSL_DISTRO_NAME")
          "/mnt/c/Windows/System32/WindowsPowerShell/v1.0")
        (when (getenv "WSL_DISTRO_NAME")
          "/mnt/c/Windows/System32"))))

  (dolist (path (delq nil paths-to-add))
    (cae-add-dir-to-path path))
  
  (when (file-exists-p "/usr/bin/conda")
    (cae-add-dir-to-path "/usr/bin")))

;; This is so that I don't accidentally start Emacs as a daemon.
(when (and (daemonp) (not (getenv "EMACS_DAEMON_SERVICE")))
  (kill-emacs))

(when (boundp 'safe-local-variable-directories)
  (dolist (dir (list doom-user-dir doom-emacs-dir "~/org" (getenv "HOME")))
    (add-to-list 'safe-local-variable-directories dir)))

;; Do not start incrementally loading packages until I am actually AFK.
(setq doom-incremental-first-idle-timer 60)

(setq native-comp-async-jobs-number (num-processors)
      native-comp-async-report-warnings-errors 'silent)

(when cae-init-debug-enabled-p
  (load! "lisp/cae-debug" doom-user-dir))
(when cae-init-dir-locals-enabled-p
  (load! "lisp/cae-dir-locals" doom-user-dir))
(when cae-init-hacks-enabled-p
  (load! "lisp/cae-hacks" doom-user-dir))

;; Set a fallback theme.
(setq doom-theme 'wheatgrass)

(setq evil-undo-system 'undo-fu)

;; Make it easier to debug lazy loading issues.
(when init-file-debug (setq doom-incremental-first-idle-timer nil))

;; I never want to see loading messages.
(unless init-file-debug
  (cae-defadvice! cae-load-ignore-message-a (args)
    :filter-args #'load
    (cl-destructuring-bind (file &optional noerror nomessage nosuffix must-suffix) args
      (list file noerror t nosuffix must-suffix))))
