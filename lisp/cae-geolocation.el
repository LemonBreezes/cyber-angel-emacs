;;; lisp/cae-geolocation.el -*- lexical-binding: t; -*-

;; TODO Integrate with NOAA for weather.

(defgroup cae-geolocation nil
  "Settings for CAE geolocation features."
  :group 'cae)

(defcustom cae-geolocation-significant-change-threshold 0.05
  "Threshold for determining if a location change is significant.
This is measured in degrees of latitude/longitude, where ~0.01 is roughly 1km.
Only changes larger than this will trigger updates to dependent systems."
  :type 'float
  :group 'cae-geolocation)

(defcustom cae-geolocation-update-interval 3600
  "Frequency in seconds for periodic geolocation updates."
  :type 'integer
  :group 'cae-geolocation)

(defcustom cae-geolocation-idle-delay 120
  "Idle time in seconds before running geolocation updates."
  :type 'integer
  :group 'cae-geolocation)

(defcustom cae-geolocation-beacondb-api-key nil
  "API key for BeaconDB geolocation service.
If nil, attempts to use `geo-nm-moz-api-key'."
  :type '(choice (const :tag "Use geo-nm-moz-api-key" nil) string)
  :group 'cae-geolocation)

(defvar cae-geolocation-update-hook nil
  "Hook run after geolocation information is updated.
Functions in this hook are run with no arguments after the location
information has been updated and calendar-latitude/longitude have been set.")

(defun cae-geolocation-significant-change-p (lat1 lng1 lat2 lng2)
  "Return t if the change in location from LAT1,LNG1 to LAT2,LNG2 is significant.
Uses `cae-geolocation-significant-change-threshold' to determine significance.
A change is considered significant if either the latitude or longitude
changes by more than the threshold amount."
  (or (> (abs (- lat1 lat2)) cae-geolocation-significant-change-threshold)
      (> (abs (- lng1 lng2)) cae-geolocation-significant-change-threshold)))

(defun cae-geolocation--update-location (lat lng accuracy source)
  "Update location, store it, and run hooks if change is significant.
LAT and LNG are the new coordinates.
ACCURACY is the reported accuracy in meters.
SOURCE indicates the origin ('api, 'cache, etc.).
Returns t if the location change was significant, nil otherwise."
  (let ((significant-change
         (cae-geolocation-significant-change-p calendar-latitude calendar-longitude lat lng)))
    (message "Geolocation: Received update from %s: Lat %s, Lon %s (Accuracy: %s m). Significant change: %s"
             source lat lng accuracy significant-change)
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

(defun cae-geolocation-setup (&optional verbosity)
  "Set up geolocation using BeaconDB API and integrate with solar calendar asynchronously.
  Updates calendar-latitude and calendar-longitude and triggers circadian theme updates.

  Optional VERBOSITY controls output messages:
  0 - Silent operation, errors only if not network-related (default for non-interactive)
  1 - Basic confirmation (default for interactive)"
  (interactive (list 1))

  ;; Set default verbosity if not provided
  (unless verbosity
    (setq verbosity (if (called-interactively-p 'any) 1 0)))

  ;; Load required packages
  (require 'geo)
  (require 'geo-solar)
  (require 'url)
  (require 'json)

  ;; Override geo-nm to use BeaconDB
  (require 'geo-nm)

  (when (> verbosity 0)
    (message "Geolocation: Getting location from BeaconDB..."))

  ;; Collect WiFi networks
  (let* ((access-points (geo-nm--get-aps))
         (wifi-data (mapcar
                     (lambda (ap)
                       `((macAddress . ,(geo-nm--ap-hwaddr ap))
                         (signalStrength . ,(geo-nm--ap-dbm ap))))
                     access-points))
         (payload (json-encode `((wifiAccessPoints . ,wifi-data)))))

    ;; Setup for async request
    (let ((api-key (or cae-geolocation-beacondb-api-key geo-nm-moz-api-key)))
      (unless api-key
        (message "Geolocation Error: No API key found in 'cae-geolocation-beacondb-api-key' or 'geo-nm-moz-api-key'.")
        (error "Missing BeaconDB API key")) ; Or return nil if you prefer graceful failure
      (setq geo-nm-moz-url-format (format "https://api.beacondb.net/v1/geolocate?key=%s" api-key)))

    (let ((url-request-method "POST")
          (url-request-extra-headers '(("Content-Type" . "application/json")))
          (url-request-data payload))

      ;; Make async HTTP request
      (url-retrieve
       "https://api.beacondb.net/v1/geolocate"

       ;; Callback function to process response
       (lambda (status &rest _)
         ;; Process successful response
         (goto-char (point-min))
         (re-search-forward "^$" nil t)
         (let* ((json-object-type 'hash-table)
                (json-array-type 'list)
                (response (condition-case err
                              (json-read)
                            (error (message "Geolocation Error: JSON parsing failed: %s" err)
                                   nil))))

           (if (and response (gethash "location" response))
               (let* ((data (gethash "location" response))
                      (lat (gethash "lat" data))
                      (lng (gethash "lng" data))
                      (accuracy (gethash "accuracy" response)))
                 ;; Update location using the new helper
                 (cae-geolocation--update-location lat lng accuracy 'api))
             ;; Handle errors
             (progn
               (message "Geolocation Error: Failed to parse location from BeaconDB response. Status: %s" status)
               ;; Optionally log the buffer content for debugging
               ;; (message "Geolocation Error: Response buffer content:\n%s" (buffer-string))
               ))))

       nil (= verbosity 0))))) ;; Silence network errors if verbosity is 0

;; Store and retrieve location data

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
  (cae-geolocation-schedule-updates)
  ;; Note: Storing location is now handled by cae-geolocation--update-location,
  ;; so the kill-emacs-hook is no longer needed here.
  )

;; Start the geolocation system
(cae-geolocation-init)
