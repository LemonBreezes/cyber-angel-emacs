;;; lisp/cae-geolocation.el -*- lexical-binding: t; -*-

(defvar cae-geolocation-significant-change-threshold 0.05
  "Threshold for determining if a location change is significant.
This is measured in degrees of latitude/longitude, where ~0.01 is roughly 1km.
Only changes larger than this will trigger updates to dependent systems.")

(defvar cae-geolocation-update-interval 3600
  "Frequency in seconds for periodic geolocation updates.")

(defvar cae-geolocation-idle-delay 120
  "Idle time in seconds before running geolocation updates.")

(defvar cae-geolocation-update-hook nil
  "Hook run after geolocation information is updated.
Functions in this hook are run with no arguments after the location
information has been updated and calendar-latitude/longitude have been set.")

(defun cae-geolocation-restore-location ()
  "Restore location data from doom store if available.
Returns t if a significant change occurred compared to the current state, nil otherwise."
  (let ((lat (doom-store-get 'calendar-latitude))
        (lng (doom-store-get 'calendar-longitude)))
    (if (and lat lng (not (= 0 lat)) (not (= 0 lng)))
        (progn
          (message "Geolocation: Restoring cached location: Lat %s, Lon %s" lat lng)
          ;; Update location using the helper, return its result
          (cae-geolocation--update-location lat lng "cached" 'cache))
      ;; No valid cached location found
      (message "Geolocation: No valid cached location found.")
      nil)))

;; Schedule geolocation updates
(defun cae-geolocation-schedule-updates ()
  "Schedule periodic geolocation updates."
  (interactive)
  (message "Geolocation: Scheduling periodic updates (every %ds after %ds idle)."
           cae-geolocation-update-interval cae-geolocation-idle-delay)
  ;; Schedule to run periodically when Emacs is idle
  (run-with-idle-timer cae-geolocation-idle-delay
                       cae-geolocation-update-interval
                       #'cae-geolocation-setup 0))

;; Fast startup function that doesn't require loading geo packages
(defun cae-geolocation-init ()
  "Initialize geolocation system during startup.
Restores cached location if available, schedules initial fetch if needed,
and sets up periodic updates."
  ;; Attempt to restore location from cache.
  (unless (cae-geolocation-restore-location)
    ;; If restore failed or location was invalid, schedule an initial fetch attempt
    ;; after a short idle period (5s).
    (message "Geolocation: Scheduling initial fetch.")
    (run-with-idle-timer 5 nil #'cae-geolocation-setup 0))

  ;; Schedule the regular periodic updates.
  (cae-geolocation-schedule-updates))

;; Start the geolocation system
(cae-geolocation-init)
