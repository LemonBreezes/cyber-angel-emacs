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

(defun cae-geolocation--update-location (lat lng accuracy source)
  "Update location, store it, and run hooks if change is significant.
LAT and LNG are the new coordinates.
ACCURACY is the reported accuracy in meters.
SOURCE indicates the origin ('api, 'cache, etc.).
Returns t if the location change was significant, nil otherwise."
  (let ((significant-change
         (cae-geolocation-significant-change-p calendar-latitude calendar-longitude lat lng)))
    ;; Update calendar variables
    (setq calendar-latitude lat
          calendar-longitude lng)
    ;; Store the location
    (doom-store-put 'calendar-latitude lat)
    (doom-store-put 'calendar-longitude lng)
    ;; Run hooks only if change is significant
    (when significant-change
      (message "Geolocation: Location changed significantly (> %s°), running update hook."
               cae-geolocation-significant-change-threshold)
      (run-hooks 'cae-geolocation-update-hook))
    significant-change))

(defun cae-geolocation-restore-location ()
  "Restore location data from doom store if available.
Returns t if a significant change occurred compared to the current state, nil otherwise."
  (let ((lat (doom-store-get 'calendar-latitude))
        (lng (doom-store-get 'calendar-longitude))
        (name (doom-store-get 'cae-geolocation-current-location-name)))
    (if (and lat lng (not (= 0 lat)) (not (= 0 lng)))
        (progn
          (setq cae-geolocation-current-location-name name)
          ;; Update location using the helper, return its result
          ;; The hook mechanism now handles updating weather packages.
          (cae-geolocation--update-location lat lng "cached" 'cache))
      nil)))

(defun cae-geolocation--update-weather-packages (lat lng name)
  "Update variables in the noaa weather package if loaded.
LAT, LNG are coordinates. NAME is the location name string."
  (when (and lat lng name (stringp name) (> (length name) 0))
    (setq noaa-location name
          noaa-latitude lat
          noaa-longitude lng)
    (setq calendar-location-name name)))

;; Schedule geolocation updates
(defun cae-geolocation-schedule-updates ()
  "Schedule periodic geolocation updates."
  (interactive)
  ;; Schedule to run periodically when Emacs is idle
  (run-with-idle-timer cae-geolocation-idle-delay
                       cae-geolocation-update-interval
                       #'cae-geolocation-setup 0))

;; Fast startup function that doesn't require loading geo packages
(defun cae-geolocation-init ()
  "Initialize geolocation system during startup.
Restores cached location if available, schedules initial fetch if needed,
sets up periodic updates, and updates weather packages with initial data."
  ;; Add the hook function to fetch name/update weather when coords change
  (add-hook 'cae-geolocation-update-hook #'cae-geolocation--fetch-name-and-update-weather-h)
  ;; Attempt to restore location from cache. This sets calendar vars and name var.
  (let ((restored-successfully (cae-geolocation-restore-location)))
    ;; If restore failed or location was invalid, schedule an initial fetch.
    (unless restored-successfully
      (run-with-idle-timer 5 nil #'cae-geolocation-setup 0)))

  ;; Schedule the regular periodic updates.
  (cae-geolocation-schedule-updates))

;; Start the geolocation system
(cae-geolocation-init)
