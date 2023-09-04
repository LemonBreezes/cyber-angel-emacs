;;; lisp/set-up-geolocation.el -*- lexical-binding: t; -*-

(require 'pcache nil t)
(require 'geo-ip nil t)
(require 'geo-fallback nil t)
(require 'geo-solar nil t)

(unless geo-ip--last-location
  (when-let* ((loc (pcache-get (pcache-repository "geo")
                               'geo-ip--last-location))
              (lat (alist-get 'lat loc))
              (lon (alist-get 'lon loc)))

    (setq geo-fallback-lat lat geo-fallback-lon lon)
    (geo-fallback-notify-changed)))

(defun +geo-ip--cache ()
  (when (and geo-ip--last-location
             (not (eq (alist-get 'lat loc) 0.0))
             (not (eq (lon (alist-get 'lon loc)) 0.0)))
    (pcache-put (pcache-repository "geo") 'geo-ip--last-location geo-ip--last-location)))

(add-hook 'geo-ip--changed-hook #'+geo-ip--cache)
