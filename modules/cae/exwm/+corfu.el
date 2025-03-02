;;; cae/exwm/+corfu.el -*- lexical-binding: t; -*-

;; Advise packages that use posframe for a multi-head setup

(defun cae-get-focused-monitor-geometry ()
  "Get the geometry of the monitor displaying the selected frame in EXWM."
  (let* ((monitor-attrs (frame-monitor-attributes))
         (workarea (assoc 'workarea monitor-attrs))
         (geometry (cdr workarea)))
    (list (nth 0 geometry) ; X
          (nth 1 geometry) ; Y
          (nth 2 geometry) ; Width
          (nth 3 geometry) ; Height
          )))

(defun cae-advise-corfu-make-frame-with-monitor-awareness (orig-fun frame x y width height)
  "Advise `corfu--make-frame` to be monitor-aware, adjusting X and Y according to the focused monitor."

  ;; Get the geometry of the currently focused monitor
  (let* ((monitor-geometry (cae-get-focused-monitor-geometry))
         (monitor-x (nth 0 monitor-geometry))
         (monitor-y (nth 1 monitor-geometry))
         ;; You may want to adjust the logic below if you have specific preferences
         ;; on where on the monitor the posframe should appear.
         ;; Currently, it places the posframe at its intended X and Y, but ensures
         ;; it's within the bounds of the focused monitor.
         (new-x (+ monitor-x x))
         (new-y (+ monitor-y y)))

    ;; Call the original function with potentially adjusted coordinates
    (funcall orig-fun frame new-x new-y width height)))

(advice-add 'corfu--make-frame :around #'cae-advise-corfu-make-frame-with-monitor-awareness)
