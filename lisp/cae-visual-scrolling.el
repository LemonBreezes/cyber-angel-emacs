;;; lisp/cae-visual-scrolling.el -*- lexical-binding: t; -*-

;; This code was copied from here:
;; https://web.archive.org/web/20221002160330/https://with-emacs.com/posts/ui-hacks/keep-scrollin-scrollin-scrollin/
;; I like these scrolling functions a lot.

(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")

;; Some modes remap `scroll-down-command', etc rather than binding the keys directly.
;;(map! "C-v" #'View-scroll-half-page-forward
;;      "M-v" #'View-scroll-half-page-backward
;;      "C-M-v" #'my-View-scroll-half-page-forward-other-window
;;      "C-M-S-v" #'my-View-scroll-half-page-backward-other-window)

(map! [remap scroll-up-command] #'View-scroll-half-page-forward
      [remap scroll-down-command] #'View-scroll-half-page-backward
      [remap scroll-other-window] #'my-View-scroll-half-page-forward-other-window
      [remap scroll-other-window-down] #'my-View-scroll-half-page-backward-other-window)

(defun my-View-scroll-half-page-forward-other-window ()
  (interactive)
  (with-selected-window (next-window)
    (call-interactively 'View-scroll-half-page-forward)))

(defun my-View-scroll-half-page-backward-other-window ()
  (interactive)
  (with-selected-window (next-window)
    (call-interactively 'View-scroll-half-page-backward)))

;;(setq scroll-preserve-screen-position 'always)

(advice-add #'View-scroll-half-page-forward :around
            #'my-indicate-scroll-forward)

(advice-add #'View-scroll-half-page-backward :around
            #'my-indicate-scroll-backward)

(defun my-indicate-scroll-get-line (pos)
  (save-excursion
    (goto-char pos)
    (string-to-number (format-mode-line "%l"))))

(defun my-indicate-scroll (linep f args)
  (let ((linen (my-indicate-scroll-get-line linep))
        (pulse-delay 0.1))
    (save-excursion
      (goto-line linen)
      (pulse-momentary-highlight-one-line (point) 'highlight))
    (sit-for 0.1)
    (apply f args)))

(defun my-indicate-scroll-forward (f &rest args)
  (my-indicate-scroll (1- (window-end)) f args))

(defun my-indicate-scroll-backward (f &rest args)
  (my-indicate-scroll (window-start) f args))

(after! repeat
  (define-repeat-map View-scroll-half-page-forward
    ("v" View-scroll-half-page-forward)
    (:exit "V" View-scroll-half-page-backward))
  (define-repeat-map View-scroll-half-page-backward
    ("v" View-scroll-half-page-backward)
    (:exit "V" View-scroll-half-page-forward))
  (define-repeat-map my-View-scroll-half-page-forward-other-window
    ("v" my-View-scroll-half-page-forward-other-window)
    (:exit "V" my-View-scroll-half-page-backward-other-window))
  (define-repeat-map my-View-scroll-half-page-backward-other-window
    ("v" my-View-scroll-half-page-backward-other-window)
    (:exit "V" my-View-scroll-half-page-forward-other-window)))

;; For some reason this doesn't work. `restore-point' doesn't restore point for
;; these commands, even though it does push the point to the ring.
(after! restore-point
  (dolist (fn '(View-scroll-half-page-forward
                View-scroll-half-page-backward
                my-View-scroll-half-page-forward-other-window
                my-View-scroll-half-page-backward-other-window))
    (add-to-list 'rp/restore-point-commands fn)))
