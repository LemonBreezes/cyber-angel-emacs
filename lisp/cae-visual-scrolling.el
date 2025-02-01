;;; lisp/cae-visual-scrolling.el -*- lexical-binding: t; -*-

;;Error: wrong-type-argument (overlayp nil)
;;  whitespace-point--flush-used(113479)
;;  font-lock-fontify-keywords-region(113424 113479 nil)
;;  font-lock-default-fontify-region(113424 113479 nil)
;;  font-lock-fontify-region(113424 113479)
;;  #f(compiled-function (fun) #<bytecode 0x1ba31c79f97a0c7f>)(font-lock-fontify-region)
;;  jit-lock--run-functions(113424 113479)
;;  jit-lock-fontify-now(113424 114924)
;;  jit-lock-function(113424)
;;  scroll-down(19)
;;  scroll-down-command(19)
;;  view-scroll-lines(nil t 19 t)
;;  #<subr View-scroll-half-page-backward>(nil)
;;  funcall(#<subr View-scroll-half-page-backward> nil)
;;  (let ((pos (if (eq direction 'down) (1- (window-end)) (window-start))) (pulse-delay 0.1)) (pulse-momentary-highlight-one-line pos 'highlight) (sit-for 0.1) (funcall scroll-fn arg))
;;  (if (or arg (cl-some #'(lambda (x) (and (boundp x) (symbol-value x))) '(executing-kbd-macro))) (funcall scroll-fn arg) (let ((pos (if (eq direction 'down) (1- (window-end)) (window-start))) (pulse-delay 0.1)) (pulse-momentary-highlight-one-line pos 'highlight) (sit-for 0.1) (funcall scroll-fn arg)))
;;  cae-scroll-with-highlight(#<subr View-scroll-half-page-backward> up nil)
;;  #f(lambda (f &optional arg) [t] (cae-scroll-with-highlight f 'up arg))(#<subr View-scroll-half-page-backward> nil)
;;  apply(#f(lambda (f &optional arg) [t] (cae-scroll-with-highlight f 'up arg)) #<subr View-scroll-half-page-backward> nil)
;;  View-scroll-half-page-backward(nil)
;;  funcall-interactively(View-scroll-half-page-backward nil)
;;  command-execute(View-scroll-half-page-backward)

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
