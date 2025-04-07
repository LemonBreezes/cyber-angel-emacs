;;; lisp/cae-geolocation.el -*- lexical-binding: t; -*-

(defvar cae-geolocation-significant-change-threshold 0.05
  "Threshold for determining if a location change is significant.
This is measured in degrees of latitude/longitude, where ~0.01 is roughly 1km.
Only changes larger than this will trigger updates to dependent systems.")

(defvar cae-geolocation-update-interval 3600
  "Frequency in seconds for periodic geolocation updates.")

(defvar cae-geolocation-idle-delay 120
  "Idle time in seconds before running geolocation updates.")

(defvar cae-geolocation-beacondb-api-key nil
  "API key for BeaconDB geolocation service.
If nil, attempts to use `geo-nm-moz-api-key'.")

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

(defun cae-geolocation-get-noaa-grid-url (lat lng callback)
  "Fetch the NOAA forecast grid data URL for LAT and LNG asynchronously.
LAT and LNG are the latitude and longitude coordinates.
CALLBACK is a function called with the result. The callback
receives one argument, which is a list:
- On success: (:success :grid-url \"URL_STRING\" :location-name \"City, State\")
- On error:   (:error \"ERROR_MESSAGE\" DETAILS)"
  (unless (and (numberp lat) (numberp lng))
    (funcall callback (list :error "Invalid coordinates provided" (list lat lng)))
    (error "Invalid coordinates provided to cae-geolocation-get-noaa-grid-url: %s, %s" lat lng))

  (let* ((noaa-url (format "https://api.weather.gov/points/%s,%s" lat lng))
         ;; NOAA requires a User-Agent.
         (url-request-extra-headers `(("User-Agent" . "cae-emacs-config/1.0 (https://github.com/alphapapa/cae)"))))
    (message "Geolocation: Fetching NOAA grid URL from %s" noaa-url)
    (url-retrieve
     noaa-url
     (lambda (status &rest _)
       (condition-case err
           (progn
             (goto-char (point-min))
             ;; Skip HTTP headers
             (unless (re-search-forward "^$" nil t)
               (error "Could not find end of HTTP headers"))
             (let* ((json-object-type 'hash-table)
                    (json-array-type 'list)
                    (response (json-read)))
               (if (and response (gethash "properties" response))
                   ;; Extract relative location first
                   (let* ((props (gethash "properties" response))
                          (relative-loc (gethash "relativeLocation" props))
                          (relative-props (and relative-loc (gethash "properties" relative-loc)))
                          (city (and relative-props (gethash "city" relative-props)))
                          (state (and relative-props (gethash "state" relative-props)))
                          (location-name (if (and city state) (format "%s, %s" city state) "Unknown Location")))
                     ;; Now get the grid URL and call the callback
                     (let ((grid-url (gethash "forecastGridData" props))) ; Use props directly
                       (if grid-url
                           (progn
                             (message "Geolocation: Successfully retrieved NOAA grid URL for %s." location-name)
                             (funcall callback (list :success :grid-url grid-url :location-name location-name)))
                         (progn
                           (message "Geolocation Error: 'forecastGridData' not found in NOAA response.")
                           (funcall callback (list :error "Missing 'forecastGridData' in NOAA response" response))))))
                 (progn
                   (message "Geolocation Error: Failed to parse 'properties' from NOAA response.")
                   (funcall callback (list :error "Failed to parse NOAA response properties" response))))))
         (error
          (message "Geolocation Error: Failed during NOAA request/parsing: %s" err)
          (funcall callback (list :error (format "NOAA request/parsing error: %s" err) (buffer-string))))))
     nil t)))

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
