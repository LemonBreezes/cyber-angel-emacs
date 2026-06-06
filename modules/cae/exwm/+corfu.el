;;; cae/exwm/+corfu.el -*- lexical-binding: t; -*-

;; Advise packages that use posframe for a multi-head setup

;; Declared special so the `let' rebind in the monitor-awareness advice below is
;; dynamic (corfu reads this inside `corfu--make-frame') -- guards against a
;; lexical-binding no-op if this file is byte/native-compiled before corfu loads.
(defvar corfu--frame-parameters)

;;(defun cae-get-focused-monitor-geometry ()
;;  "Get the geometry of the monitor displaying the selected frame in EXWM."
;;  (let* ((monitor-attrs (frame-monitor-attributes))
;;         (workarea (assoc 'workarea monitor-attrs))
;;         (geometry (cdr workarea)))
;;    (list (nth 0 geometry) ; X
;;          (nth 1 geometry) ; Y
;;          (nth 2 geometry) ; Width
;;          (nth 3 geometry) ; Height
;;          )))

(defun cae-advise-corfu-make-frame-with-monitor-awareness (orig-fun frame x y width height)
  "Advise `corfu--make-frame` to be monitor-aware, adjusting X and Y according to the focused monitor."

  ;; Get the geometry of the currently focused monitor
  (let* (
         ;;(monitor-geometry (cae-get-focused-monitor-geometry))
         ;;(monitor-x (nth 0 monitor-geometry))
         ;;(monitor-y (nth 1 monitor-geometry))
         (selected-frame-position (frame-position))
         (selected-frame-x (car selected-frame-position))
         (selected-frame-y (cdr selected-frame-position))
         ;;(new-x (+ monitor-x selected-frame-x x))
         ;;(new-y (+ monitor-y selected-frame-y y))
         ;; Use the frame's absolute position coordinates
         ;; Don't add monitor-x/y as frame-position already includes monitor offset
         (new-x (+ selected-frame-x x))
         (new-y (+ selected-frame-y y))
         )

    ;; Call the original function with adjusted coordinates.  Under EXWM, also
    ;; make Corfu's child frame born override-redirect: corfu--make-frame builds
    ;; its make-frame params from `corfu--frame-parameters' (with visibility nil),
    ;; then deparents the frame to root internally -- so setting override-redirect
    ;; afterward (a :filter-return) is too late: EXWM grabs it via MapRequest on
    ;; the first show.  Born override-redirect, the X server emits no MapRequest,
    ;; so EXWM never manages it (no fullscreen, no auto-persp window glitch).
    (let ((corfu--frame-parameters
           (cons '(override-redirect . t) corfu--frame-parameters)))
      (funcall orig-fun frame new-x new-y width height))))
