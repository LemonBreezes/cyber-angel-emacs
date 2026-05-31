;;; autoload/cae-geolocation.el -*- lexical-binding: t; -*-

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
