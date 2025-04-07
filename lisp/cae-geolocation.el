;;; lisp/cae-geolocation.el -*- lexical-binding: t; -*-

;; TODO Integrate with NOAA for weather.

(defvar cae-geolocation-significant-change-threshold 0.05
  "Threshold for determining if a location change is significant.
This is measured in degrees of latitude/longitude, where ~0.01 is roughly 1km.
Only changes larger than this will trigger updates to dependent systems.")

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

  ;; Add project's lisp directory to load-path
  (add-to-list 'load-path "/home/st/src/geo/lisp-src")

  ;; Load required packages
  (require 'geo)
  (require 'geo-solar)
  (require 'url)
  (require 'json)

  ;; Override geo-nm to use BeaconDB
  (require 'geo-nm)
  (setq geo-nm-moz-url-format "https://api.beacondb.net/v1/geolocate?key=%s")

  (when (> verbosity 0)
    (message "Getting location from BeaconDB..."))

  ;; Collect WiFi networks
  (let* ((access-points (geo-nm--get-aps))
         (wifi-data (mapcar
                     (lambda (ap)
                       `((macAddress . ,(geo-nm--ap-hwaddr ap))
                         (signalStrength . ,(geo-nm--ap-dbm ap))))
                     access-points))
         (payload (json-encode `((wifiAccessPoints . ,wifi-data)))))

    ;; Setup for async request
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
                (response (condition-case nil
                              (json-read)
                            (error nil))))

           (if response
               (let* ((data (gethash "location" response))
                      (lat (gethash "lat" data))
                      (lng (gethash "lng" data))
                      (accuracy (gethash "accuracy" response)))

                 ;; Create location and update
                 (let ((location (geo-location lat lng (float-time)
                                               `((accuracy . ,accuracy)))))

                   ;; Only output location if verbose mode
                   (when (> verbosity 0)
                     (message "Location retrieved: Lat %s, Lon %s (accuracy: %s meters)"
                              lat lng accuracy))

                   ;; Update geo and solar
                   (run-hook-with-args 'geo-data-changed-hook location)

                   ;; Check if location has changed significantly
                   (let ((location-changed (and
                                            ;; Skip completely identical locations
                                            (or (not (equal calendar-latitude lat))
                                                (not (equal calendar-longitude lng)))
                                            ;; Only trigger updates if change is significant
                                            (cae-geolocation-significant-change-p
                                             calendar-latitude calendar-longitude
                                             lat lng))))

                     ;; Update calendar variables
                     (setq calendar-latitude lat
                           calendar-longitude lng)

                     ;; Store the location in doom-store for faster retrieval on startup
                     (doom-store-put 'calendar-latitude lat)
                     (doom-store-put 'calendar-longitude lng)

                     ;; Only run updates if location has significantly changed
                     (when location-changed
                       (when (> verbosity 0)
                         (message "Location has changed significantly (> %s°), updating dependent systems"
                                  cae-geolocation-significant-change-threshold))

                       ;; Update circadian theme if available
                       (when (and (featurep 'circadian)
                                  cae-theme-enable-day-night-theme-switching)
                         (circadian-setup))

                       ;; Run any other location-dependent hooks
                       (run-hooks 'cae-geolocation-update-hook)))))

             ;; Handle errors
             (when (> verbosity 0)
               (message "Failed to retrieve location from BeaconDB response")))))

       nil (= verbosity 0)))))

;; Store and retrieve location data
(defun cae-geolocation-store-location ()
  "Store current location data in doom store for faster retrieval on startup."
  (when (and calendar-latitude calendar-longitude
             (not (= 0 calendar-latitude))
             (not (= 0 calendar-longitude)))
    (doom-store-put 'calendar-latitude calendar-latitude)
    (doom-store-put 'calendar-longitude calendar-longitude)
    (message "Stored location: Lat %s, Lon %s"
             calendar-latitude calendar-longitude)))

(defun cae-geolocation-restore-location ()
  "Restore location data from doom store if available."
  (let ((lat (doom-store-get 'calendar-latitude))
        (lng (doom-store-get 'calendar-longitude)))
    (when (and lat lng)
      ;; Check if location has changed significantly
      (let ((location-changed (and
                               ;; Skip identical locations
                               (or (not (equal calendar-latitude lat))
                                   (not (equal calendar-longitude lng)))
                               ;; Only trigger if change is significant
                               (cae-geolocation-significant-change-p
                                calendar-latitude calendar-longitude
                                lat lng))))
        ;; Update values
        (setq calendar-latitude lat
              calendar-longitude lng)
        (message "Restored cached location: Lat %s, Lon %s" lat lng)

        ;; Only run hooks if location significantly changed
        (when location-changed
          (message "Location changed significantly from cached value (> %s°), updating dependent systems"
                   cae-geolocation-significant-change-threshold)
          (run-hooks 'cae-geolocation-update-hook))
        t))))

;; Schedule geolocation updates
(defun cae-geolocation-schedule-updates ()
  "Schedule periodic geolocation updates.
Uses a repeating timer to update location information every hour
and immediately runs an initial update."
  (interactive)
  ;; Try to restore location first, if successful, delay the initial update
  (if (cae-geolocation-restore-location)
      (run-with-idle-timer 300 nil #'cae-geolocation-setup 0)
    ;; Run once immediately if no cached location available
    (cae-geolocation-setup 0))

  ;; Schedule to run every hour when Emacs is idle for 2 minutes
  (run-with-idle-timer 120 3600 #'cae-geolocation-setup 0)

  ;; Store location on exit
  (add-hook 'kill-emacs-hook #'cae-geolocation-store-location))

;; Integrate with theme system
(defun cae-geolocation-setup-theme-integration ()
  "Set up integration between geolocation and theme system.
Adds geolocation hooks to update the theme based on current location and time."
  (when cae-init-ui-enabled-p
    ;; Make sure circadian is loaded if day/night switching is enabled
    (when cae-theme-enable-day-night-theme-switching
      (require 'circadian))

    ;; Add hook to update theme when location changes
    (add-hook 'cae-geolocation-update-hook
              (lambda ()
                (when (and (featurep 'circadian)
                           cae-theme-enable-day-night-theme-switching)
                  (message "Updating theme based on new location")
                  (circadian-setup))))))

;; Fast startup function that doesn't require loading geo packages
(defun cae-geolocation-init ()
  "Initialize geolocation system during startup.
This function avoids loading heavy geo libraries by using cached values
and scheduling the full location update for later."
  (if (cae-geolocation-restore-location)
      (run-with-idle-timer 120 3600 #'cae-geolocation-setup 0)
    ;; No cached location, need to schedule immediate update when packages are available
    (run-with-idle-timer 5 nil #'cae-geolocation-schedule-updates))

  ;; Always add the save hook
  (add-hook 'kill-emacs-hook #'cae-geolocation-store-location))

;; Start the geolocation system
(cae-geolocation-init)

;; Set up integration with theme system
(when cae-init-ui-enabled-p
  (cae-geolocation-setup-theme-integration))
