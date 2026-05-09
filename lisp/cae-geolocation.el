;;; lisp/cae-geolocation.el -*- lexical-binding: t; -*-

(defvar cae-geolocation-verbose nil
  "When non-nil, display geolocation status messages.
When nil, geolocation operates silently unless errors occur.")

(defvar cae-geolocation-significant-change-threshold 0.20
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
  "The human-readable name of the current location (e.g., \"City, Region\").
Sourced from ipinfo.io alongside the coordinates.")

(defvar cae-geolocation-current-timezone nil
  "The IANA time zone name for the current location (e.g., \"America/New_York\").
Sourced from ipinfo.io alongside the coordinates.")

(defun cae-geolocation-restore-location ()
  "Restore location data from doom store if available.
Returns t if a significant change occurred compared to the current state,
nil otherwise (including when any required field is missing from the cache)."
  (let ((lat (doom-store-get 'calendar-latitude))
        (lng (doom-store-get 'calendar-longitude))
        (timezone (doom-store-get 'cae-geolocation-current-timezone))
        (name (doom-store-get 'cae-geolocation-current-location-name)))
    (when (and (numberp lat) (numberp lng) (stringp timezone) (stringp name)
               (not (= 0 lat)) (not (= 0 lng)))
      (cae-geolocation--update-location lat lng timezone name))))

;; Attempt to restore location from cache. This sets calendar vars and name var.
(let ((restored-successfully (cae-geolocation-restore-location)))
  ;; If restore failed or location was invalid, schedule an initial fetch.
  (unless restored-successfully
    (run-with-idle-timer 5 nil #'cae-geolocation-setup 0)))

;; Schedule the regular periodic updates.
(cae-run-with-idle-timer cae-geolocation-idle-delay
                           cae-geolocation-update-interval
                           "cae-geolocation-setup"
                           #'cae-geolocation-setup 0)
