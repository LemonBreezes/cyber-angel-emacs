;;; autoload/cae-geolocation.el -*- lexical-binding: t; -*-

(require 'url)
(require 'json)

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(cl-defun cae-geolocation-setup (&optional verbosity force)
  "Set up geolocation using ipinfo.io and integrate with solar calendar asynchronously.
Updates calendar-latitude, calendar-longitude, and calendar-time-zone, sets the
current location name, and triggers circadian theme updates.

Optional VERBOSITY controls output messages:
0 - Silent operation, errors only if not network-related (default for non-interactive)
1 - Basic confirmation (default for interactive)

If FORCE is non-nil, run the update hook regardless of whether the change
is significant. Defaults to t when called interactively."
  (interactive (list 1 t))

  (unless verbosity
    (setq verbosity (if (called-interactively-p 'any) 1 0)))
  (unless force
    (setq force (called-interactively-p 'any)))

  (require 'url)
  (require 'json)

  (when (and cae-geolocation-verbose (> verbosity 0))
    (message "Geolocation: Getting location from ipinfo.io..."))

  (url-retrieve
   "https://ipinfo.io/json"
   (lambda (status &rest _)
     (goto-char (point-min))
     (re-search-forward "^$" nil t)
     (let* ((json-object-type 'hash-table)
            (json-array-type 'list)
            (response (condition-case err
                          (json-read)
                        (error (when cae-geolocation-verbose
                                 (message "Geolocation Error: JSON parsing failed: %s" err))
                               nil))))
       (if response
           (let* ((loc (gethash "loc" response))
                  (timezone (gethash "timezone" response))
                  (city (gethash "city" response))
                  (region (gethash "region" response))
                  (name (cond ((and (stringp city) (stringp region)
                                    (> (length city) 0) (> (length region) 0))
                               (format "%s, %s" city region))
                              ((and (stringp city) (> (length city) 0)) city)
                              ((and (stringp region) (> (length region) 0)) region)))
                  (parts (and (stringp loc) (split-string loc ",")))
                  (lat (and parts (car parts) (string-to-number (car parts))))
                  (lng (and parts (cadr parts) (string-to-number (cadr parts)))))
             (if (and (numberp lat) (numberp lng)
                      (stringp timezone) (> (length timezone) 0)
                      (stringp name))
                 (cae-geolocation--update-location lat lng timezone name force)
               (when cae-geolocation-verbose
                 (message "Geolocation Error: ipinfo response missing fields (loc=%S tz=%S name=%S)"
                          loc timezone name))))
         (when cae-geolocation-verbose
           (message "Geolocation Error: Failed to parse location from ipinfo response. Status: %s" status)))))
   nil (= verbosity 0)))
