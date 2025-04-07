;;; autoload/cae-geolocation.el -*- lexical-binding: t; -*-

(require 'url)
(require 'json)

;;;###autoload
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
    (setq geo-nm-moz-url-format "https://api.beacondb.net/v1/geolocate?key=geoclue")

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

;;;###autoload
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
                             ;; Set and store the location name
                             (setq cae-geolocation-current-location-name location-name)
                             (doom-store-put 'cae-geolocation-current-location-name location-name)
                             ;; Update weather packages now that we have coordinates AND name
                             (cae-geolocation--update-weather-packages calendar-latitude calendar-longitude location-name)
                             (funcall callback (list :success :grid-url grid-url :location-name location-name)))
                         (progn
                           (message "Geolocation Error: 'forecastGridData' not found in NOAA response.")
                           (funcall callback (list :error "Missing 'forecastGridData' in NOAA response" response))))))
                 (progn
                   (message "Geolocation Error: Failed to parse 'properties' from NOAA response.")
                   (funcall callback (list :error "Failed to parse NOAA response properties" response))))))
         ;; Handle errors during request/parsing
         (error
          (message "Geolocation Error: Failed during NOAA request/parsing: %s" err)
          (funcall callback (list :error (format "NOAA request/parsing error: %s" err) (buffer-string))))))
     ;; Optional parameters for url-retrieve
     nil ; params - not needed for GET
     t   ; silent - suppress network messages unless error
     ))) ; End of url-retrieve call

;;;###autoload
(defun cae-geolocation--fetch-name-and-update-weather-h ()
  "Hook function to fetch NOAA location name and update weather packages.
Uses current calendar-latitude and calendar-longitude."
  (interactive) ; Make it interactive for easier testing if needed
  (message "Geolocation Hook: Triggered fetching name for %s, %s"
           calendar-latitude calendar-longitude)
  (if (and (boundp 'calendar-latitude) (numberp calendar-latitude) (not (= 0 calendar-latitude))
           (boundp 'calendar-longitude) (numberp calendar-longitude) (not (= 0 calendar-longitude)))
      (cae-geolocation-get-noaa-grid-url
       calendar-latitude
       calendar-longitude
       ;; Dummy callback - the main function handles updates internally now
       (lambda (result)
         (unless (eq (car result) :success)
           (message "Geolocation Hook: Failed to get NOAA details: %S" result))))
    (message "Geolocation Hook: Invalid coordinates (%s, %s), skipping NOAA fetch."
             calendar-latitude calendar-longitude)))
