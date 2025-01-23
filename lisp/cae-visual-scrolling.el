;;; lisp/cae-visual-scrolling.el -*- lexical-binding: t; -*-

(defun cae-scroll-with-highlight (scroll-fn direction &optional arg)
  "Highlight the appropriate line before calling SCROLL-FN with ARGS.
DIRECTION should be 'up or 'down."
  (if (or arg
          (cl-some (lambda (x) (and (boundp x) (symbol-value x)))
                   '(executing-kbd-macro)))
      ;; Passthrough for specified conditions.
      (funcall scroll-fn arg)
    (let ((pos (if (eq direction 'down) (1- (window-end)) (window-start)))
          (pulse-delay 0.1))
      (pulse-momentary-highlight-one-line pos 'highlight)
      (sit-for 0.1)
      (funcall scroll-fn arg))))

(defmacro cae-advise-scroll (func direction)
  "Add around advice to FUNC to highlight line before scrolling in DIRECTION."
  `(advice-add #',func :around (lambda (f &optional arg)
                                 (cae-scroll-with-highlight f ',direction arg))))

;; Advise scrolling functions to include line highlighting.
(cae-advise-scroll evil-scroll-down down)
(cae-advise-scroll evil-scroll-up up)
(cae-advise-scroll View-scroll-half-page-forward down)
(cae-advise-scroll View-scroll-half-page-backward up)

(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")

;; Remap standard scroll commands to use half-page scrolling.
(global-set-key [remap scroll-up-command] 'View-scroll-half-page-forward)
(global-set-key [remap scroll-down-command] 'View-scroll-half-page-backward)
(global-set-key [remap scroll-other-window] 'cae-view-scroll-half-page-forward-other-window)
(global-set-key [remap scroll-other-window-down] 'cae-view-scroll-half-page-backward-other-window)

(defun cae-view-scroll-half-page-forward-other-window ()
  "Scroll half a page forward in the next window."
  (interactive)
  (with-selected-window (next-window)
    (call-interactively 'View-scroll-half-page-forward)))

(defun cae-view-scroll-half-page-backward-other-window ()
  "Scroll half a page backward in the next window."
  (interactive)
  (with-selected-window (next-window)
    (call-interactively 'View-scroll-half-page-backward)))

;; Define repeat maps for scrolling commands.
(after! repeat
  (define-repeat-map View-scroll-half-page-forward
    ("v" View-scroll-half-page-forward)
    (:exit "V" View-scroll-half-page-backward))
  (define-repeat-map View-scroll-half-page-backward
    ("v" View-scroll-half-page-backward)
    (:exit "V" View-scroll-half-page-forward))
  (define-repeat-map cae-View-scroll-half-page-forward-other-window
    ("v" cae-View-scroll-half-page-forward-other-window)
    (:exit "V" cae-View-scroll-half-page-backward-other-window))
  (define-repeat-map cae-View-scroll-half-page-backward-other-window
    ("v" cae-View-scroll-half-page-backward-other-window)
    (:exit "V" cae-View-scroll-half-page-forward-other-window)))
