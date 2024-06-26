;;; private/exwm/autoload/exwm-firefox.el -*- lexical-binding: t; -*-

;;;###autoload
(defun exwm-firefox-core-hint-links-h ()
  "Helper function which enters normal state after the browser
switches pages."
  (when exwm-firefox-evil-mode (evil-normal-state))
  (remove-hook 'exwm-update-title-hook #'exwm-firefox-core-hint-links-h))

;;;###autoload
(defun exwm-firefox-core-hint-links ()
  "Select and open a link with your keyboard."
  (interactive)
  (exwm-input--fake-key ?\M-j)
  (exwm-evil-insert)
  (+exwm-refocus-application)
  (add-hook 'exwm-update-title-hook #'exwm-firefox-core-hint-links-h))

;;;###autoload
(defun exwm-firefox-core-hint-links-new-tab-and-switch ()
  "Select and open a link in a new tab using your keyboard."
  (interactive)
  (exwm-input--fake-key ?\M-l)
  (exwm-evil-insert)
  (+exwm-refocus-application)
  (add-hook 'exwm-update-title-hook #'exwm-firefox-core-hint-links-h))

;;;###autoload
(defun +exwm-firefox-core-cancel ()
  "General cancel action."
  (interactive)
  (exwm-input--fake-key 'escape)
  (+exwm-refocus-application))

;;;###autoload
(defun +exwm-firefox-core-focus-search-bar-a-h ()
  "Helper function to enter normal state after the next browser
page switch."
  (+exwm-firefox-core-focus-search-bar-a)
  (remove-hook 'exwm-update-title-hook #'+exwm-firefox-core-focus-search-bar-a-h))

;; These bookmark functions are too delicate as they involve clicking on an
;; empty space in the browser window. The spot used may not be empty for every
;; window.

;; ;;;###autoload
;; (defun +exwm-firefox-core-focus-bookmarks-bar (arg)
;;   "Focuses the bookmarks bar so that you can use TAB and RET to
;; open a bookmark."
;;   (interactive "P")
;;   (cl-destructuring-bind
;;       ((mouse-x . mouse-y) dim-x dim-y)
;;       (list (mouse-absolute-pixel-position)
;;             (window-pixel-width) (window-pixel-height))
;;     ;; All that matters is that this spot be blank in the application.
;;     (+exwm-do-mouse-click (floor (* dim-x (/ 1.0 1000)))
;;                           (floor (* dim-y (/ 120.0 1410)))))
;;   (exwm-input--fake-key 'f6)
;;   (exwm-input--fake-key 'f6)
;;   (exwm-input--fake-key 'f6)
;;   (+exwm-refocus-application))

;; ;;;###autoload
;; (defun +exwm-firefox-core-open-bookmark-in-new-tab (arg)
;;   "Opens a new tab and focuses the bookmarks bar so that you can
;; use TAB and RET to open a bookmark."
;;   (interactive "p")
;;   (exwm-firefox-core-tab-new)
;;   (+exwm-firefox-core-focus-bookmarks-bar))

(defun +exwm-firefox--current-url ()
  "Get the URL of the currently focused EXWM buffer. Currently only
works on Chromium with the Add Page URL to Title extension or in Firefox
with the KeePass Helper - URL in title add-on."
  (let ((title exwm-title))
    (or (string-match "|url:\\[\\(.+\\)\\]" title)
        (string-match "\\(https?://[^\s\t\n]+\\)" title))
    (unless (match-string-no-properties 1 title)
      (error "No URL found in EXWM web browser buffer"))
    (match-string-no-properties 1 title)))

(defun +exwm-firefox--current-title ()
  "Get the title of the currently focused EXWM buffer. Currently only works
on Chromium with the Add Page URL to Title extension or in Firefox with
the Add URL to Window Title (Advanced KeePass Usage) in title add-on
configured to show the full URL."
  (let ((title exwm-title))
    (or (string-match "\\(.+\\) |url:\\[" title)
        (string-match (format "\\(.+\\)\\(?:\\s-*-\\s-*\\) https?://" url-handler-regexp) title))
    (unless (match-string-no-properties 1 title)
      (error "No URL found in EXWM web browser buffer"))
    (string-trim (match-string-no-properties 1 title))))

;;;###autoload
(defun +exwm-firefox-bookmark-handler (bookmark)
  "Handler for EXWM Firefox bookmarks."
  (require 'ffap)
  (let ((url (bookmark-prop-get bookmark 'filename)))
    (if (ffap-url-p url)
        (browse-url-generic url)
      (message "Bookmark does not have a valid FILENAME property."))))

;;;###autoload
(defun +exwm-firefox-bookmark--make ()
  "Make bookmarks for web pages. Currently only works on Chromium
with the Add Page URL to Title extension."
  (let ((title (+exwm-firefox--current-title))
        (url (+exwm-firefox--current-url)))
    `((filename . ,url)
      (title . ,title)
      (time . ,(current-time-string))
      (handler . +exwm-firefox-bookmark-handler)
      (defaults . (,title)))))
