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
