;;; cae/exwm/+corfu.el -*- lexical-binding: t; -*-

;; Advise packages that use posframe for a multi-head setup

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

    ;; Call the original function with adjusted coordinates
    (funcall orig-fun frame new-x new-y width height)))

(defun cae-exwm-corfu-override-redirect-a (frame)
  "Keep Corfu's child frame off EXWM's books.
Under EXWM, `corfu--make-frame' deparents its child frame (sets
`parent-frame' to nil) so EXWM windows aren't drawn over it -- but that
makes the frame a root-level window EXWM then manages and tiles to fill
the monitor.  Mark it override-redirect so the WM ignores it and Corfu's
own fit-to-content size sticks.  This is the Corfu analogue of the
`posframe-show' override-redirect advice (Corfu does not use posframe).
Used as a `:filter-return' advice on `corfu--make-frame', which returns
the frame; the setter is a no-op when the value is unchanged."
  (when (and (framep frame) (display-graphic-p frame))
    (set-frame-parameter frame 'override-redirect t))
  frame)
