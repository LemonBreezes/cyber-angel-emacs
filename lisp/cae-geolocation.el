;;; lisp/cae-geolocation.el -*- lexical-binding: t; -*-

(defvar cae-geolocation-verbose t
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

(defun cae-geolocation-significant-change-p (lat1 lng1 lat2 lng2)
  "Return t if the change in location from LAT1,LNG1 to LAT2,LNG2 is significant.
Uses `cae-geolocation-significant-change-threshold' to determine significance.
A change is considered significant if either the latitude or longitude
changes by more than the threshold amount, or if the previous location
(LAT1, LNG1) was not set (i.e., nil)."
  ;; If previous location wasn't set, any valid new location is significant.
  (if (not (and (numberp lat1) (numberp lng1)))
      t
    ;; Otherwise, compare the coordinates.
    (or (> (abs (- lat1 lat2)) cae-geolocation-significant-change-threshold)
        (> (abs (- lng1 lng2)) cae-geolocation-significant-change-threshold))))

(defun cae-geolocation--update-location (lat lng timezone name &optional force)
  "Update location, store it, and run hooks if change is significant.
LAT and LNG are the new coordinates.
TIMEZONE is an IANA tz name (e.g. \"America/New_York\") used to set
`calendar-time-zone' (in minutes east of UTC).
NAME is a human-readable location string like \"City, Region\".
If FORCE is non-nil, the significance check is bypassed and the update hook
is run unconditionally.
Returns t if the hook was run, nil otherwise."
  (let* ((significant-change
          (cae-geolocation-significant-change-p
           (doom-store-get 'calendar-latitude) (doom-store-get 'calendar-longitude)
           lat lng))
         (run-hook (or force significant-change))
         (tz-info (ignore-errors (current-time-zone nil timezone)))
         (tz-offset-minutes (and tz-info (car tz-info) (/ (car tz-info) 60))))
    ;; Update in-memory calendar variables and current-location vars.
    (setq calendar-latitude lat
          calendar-longitude lng
          cae-geolocation-current-location-name name
          cae-geolocation-current-timezone timezone)
    (when tz-offset-minutes
      (setq calendar-time-zone tz-offset-minutes))
    ;; Persist to doom-store.
    (doom-store-put 'calendar-latitude lat)
    (doom-store-put 'calendar-longitude lng)
    (doom-store-put 'cae-geolocation-current-timezone timezone)
    (doom-store-put 'cae-geolocation-current-location-name name)
    (when tz-offset-minutes
      (doom-store-put 'calendar-time-zone tz-offset-minutes))
    ;; Update dependent weather packages.
    (when (and (stringp name) (> (length name) 0))
      (setq noaa-location name
            noaa-latitude lat
            noaa-longitude lng
            calendar-location-name name))
    ;; Run hooks if forced or change is significant.
    (when run-hook
      (when cae-geolocation-verbose
        (message "Geolocation: Running update hook (%s)."
                 (if force "forced"
                   (format "change > %s°" cae-geolocation-significant-change-threshold))))
      (run-hooks 'cae-geolocation-update-hook))
    run-hook))

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
