;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(defvar cae-config-finished-loading nil)

;;; Core aka stuff that should not be disabled.

(when cae-init-core-enabled-p
  (load! "core" doom-user-dir))

(when cae-init-tty-enabled-p
  (load! "lisp/cae-tty" doom-user-dir))

(when cae-init-bindings-enabled-p
  (load! "lisp/cae-bindings" doom-user-dir))

(when cae-init-multi-enabled-p
  (load! "lisp/cae-multi" doom-user-dir))

(when cae-init-smartparens-enabled-p
  (load! "lisp/cae-smartparens" doom-user-dir))

(when cae-init-projectile-enabled-p
  (load! "lisp/cae-projectile" doom-user-dir))

(when (and (modulep! :editor evil)
           cae-init-evil-enabled-p)
  (after! evil
    (load! "lisp/cae-evil" doom-user-dir)))

(when (and (modulep! :cae exwm)
           cae-init-exwm-enabled-p)
  (after! exwm
    (load! "lisp/cae-exwm" doom-user-dir)))

(when cae-init-geolocation-enabled-p
  (load! "lisp/cae-geolocation" doom-user-dir))


;;; UI

(when cae-init-ui-enabled-p
  (load! "ui" doom-user-dir))


;;; Tools

(when cae-init-tools-enabled-p
  (load! "tools" doom-user-dir))


;;; Editor

(when cae-init-editor-enabled-p
  (load! "editor" doom-user-dir))


;;; Autocompletion

(when cae-init-autocompletion-enabled-p
  (load! "autocompletion" doom-user-dir))


;;; Term

(when cae-init-term-enabled-p
  (load! "term" doom-user-dir))


;;; Text

(when cae-init-text-enabled-p
  (load! "text" doom-user-dir))


;;; Email

(when cae-init-email-enabled-p
  (load! "email" doom-user-dir))


;;; Applications

(when cae-init-applications-enabled-p
  (load! "applications" doom-user-dir))


;;; Languages

(when cae-init-languages-enabled-p
  (load! "languages" doom-user-dir))


;;; Appendix

(when cae-init-appendix-enabled-p
  (load! "appendix" doom-user-dir))

(setq cae-config-finished-loading t)

;;Local Variables:
;;eval: (when (featurep 'aggressive-indent) (aggressive-indent-mode -1))
;;jinx-local-words: "ui"
;;End:
