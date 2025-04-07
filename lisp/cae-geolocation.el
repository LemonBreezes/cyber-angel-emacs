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

(defvar cae-geolocation-current-location-name nil
  "The human-readable name of the current location (e.g., \"City, State\").
Updated asynchronously via NOAA API.")

(defun cae-geolocation-restore-location ()
  "Restore location data from doom store if available.
Returns t if a significant change occurred compared to the current state, nil otherwise."
  (let ((lat (doom-store-get 'calendar-latitude))
        (lng (doom-store-get 'calendar-longitude))
        (name (doom-store-get 'cae-geolocation-current-location-name)))
    (if (and lat lng (not (= 0 lat)) (not (= 0 lng)))
        (progn
          (setq cae-geolocation-current-location-name name)
          (message "Geolocation: Restoring cached location: %s (%s, %s)" (or name "Unknown") lat lng)
          ;; Update location using the helper, return its result
          (let ((significant-change (cae-geolocation--update-location lat lng "cached" 'cache)))
            ;; Update weather packages with restored data
            (cae-geolocation--update-weather-packages lat lng name)
            significant-change))
      ;; No valid cached location found
      (message "Geolocation: No valid cached location found.")
      nil)))

(defun cae-geolocation--update-weather-packages (lat lng name)
  "Update variables in weather packages (biome, noaa) if they are loaded.
LAT, LNG are coordinates. NAME is the location name string."
  (when (and lat lng name (stringp name) (> (length name) 0))
    (message "Geolocation: Updating weather packages (biome, noaa) with location: %s (%s, %s)" name lat lng)

    ;; Update noaa
    (setq noaa-location name
          noaa-latitude lat
          noaa-longitude lng)))

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
sets up periodic updates, and updates weather packages with initial data."
  ;; Attempt to restore location from cache. This sets calendar vars and name var.
  (let ((restored-successfully (cae-geolocation-restore-location)))
    ;; Update weather packages with restored/current data *after* attempting restore.
    (cae-geolocation--update-weather-packages calendar-latitude calendar-longitude cae-geolocation-current-location-name)

    ;; If restore failed or location was invalid, schedule an initial fetch.
    (unless restored-successfully
      (message "Geolocation: Scheduling initial fetch.")
      (run-with-idle-timer 5 nil #'cae-geolocation-setup 0)))

  ;; Schedule the regular periodic updates.
  (cae-geolocation-schedule-updates))

;; Start the geolocation system
(cae-geolocation-init)
