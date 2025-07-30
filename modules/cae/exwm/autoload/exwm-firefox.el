;;; cae/exwm/autoload/exwm-firefox.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-exwm-firefox-core-link-hints-h ()
  "Helper function which enters normal state after the browser
switches pages."
  (when exwm-firefox-evil-mode (evil-normal-state))
  (remove-hook 'exwm-update-title-hook #'cae-exwm-firefox-core-link-hints-h))

;;;###autoload
(defun cae-exwm-firefox-core-link-hints ()
  "Select and open a link with your keyboard."
  (interactive)
  (exwm-input--fake-key ?\M-j)
  (exwm-evil-insert)
  (add-hook 'exwm-update-title-hook #'cae-exwm-firefox-core-link-hints-h))

;;;###autoload
(defun cae-exwm-firefox-core-link-hints-new-tab-and-switch ()
  "Select and open a link in a new tab using your keyboard."
  (interactive)
  (exwm-input--fake-key ?\M-l)
  (exwm-evil-insert)
  (add-hook 'exwm-update-title-hook #'cae-exwm-firefox-core-link-hints-h))

;;;###autoload
(defun cae-exwm-firefox-core-cancel ()
  "General cancel action."
  (interactive)
  (exwm-input--fake-key 'escape))

;;;###autoload
(defun cae-exwm-firefox-core-focus-search-bar-a-h ()
  "Helper function to enter normal state after the next browser
page switch."
  (cae-exwm-firefox-core-focus-search-bar-a)
  (remove-hook 'exwm-update-title-hook #'cae-exwm-firefox-core-focus-search-bar-a-h))

;; These bookmark functions are too delicate as they involve clicking on an
;; empty space in the browser window. The spot used may not be empty for every
;; window.

;; ;;;###autoload
;; (defun cae-exwm-firefox-core-focus-bookmarks-bar (arg)
;;   "Focuses the bookmarks bar so that you can use TAB and RET to
;; open a bookmark."
;;   (interactive "P")
;;   (cl-destructuring-bind
;;       ((mouse-x . mouse-y) dim-x dim-y)
;;       (list (mouse-absolute-pixel-position)
;;             (window-pixel-width) (window-pixel-height))
;;     ;; All that matters is that this spot be blank in the application.
;;     (cae-exwm-do-mouse-click (floor (* dim-x (/ 1.0 1000)))
;;                           (floor (* dim-y (/ 120.0 1410)))))
;;   (exwm-input--fake-key 'f6)
;;   (exwm-input--fake-key 'f6)
;;   (exwm-input--fake-key 'f6))

;; ;;;###autoload
;; (defun cae-exwm-firefox-core-open-bookmark-in-new-tab (arg)
;;   "Opens a new tab and focuses the bookmarks bar so that you can
;; use TAB and RET to open a bookmark."
;;   (interactive "p")
;;   (exwm-firefox-core-tab-new)
;;   (cae-exwm-firefox-core-focus-bookmarks-bar))

(defun cae-exwm-firefox--current-url ()
  "Get the URL of the currently focused EXWM buffer. Currently only
works on Chromium with the Add Page URL to Title extension or in Firefox
with the KeePass Helper - URL in title add-on."
  (let ((title exwm-title))
    (or (string-match "|url:\\[\\(.+\\)\\]" title)
        (string-match "\\(https?://[^\s\t\n]+\\)" title))
    (unless (match-string-no-properties 1 title)
      (error "No URL found in EXWM web browser buffer"))
    (match-string-no-properties 1 title)))

(defun cae-exwm-firefox--current-title ()
  "Get the title of the currently focused EXWM buffer. Currently only works
on Chromium with the Add Page URL to Title extension or in Firefox with
the Add URL to Window Title (Advanced KeePass Usage) in title add-on
configured to show the full URL."
  (let ((title exwm-title))
    (or (string-match "\\(.+\\) |url:\\[" title)
        (string-match (format "\\(.+\\)\\(?:\\s-*-\\s-*\\) https?://" url-handler-regexp) title)
        (string-match "https?://[^[:space:]]+\\(?:\\s-*[-—–]\\s-*\\)\\(.+\\)" title))
    (unless (match-string-no-properties 1 title)
      (error "No URL found in EXWM web browser buffer"))
    (string-trim (match-string-no-properties 1 title))))

;;;###autoload
(defun cae-exwm-firefox-bookmark-handler (bookmark)
  "Handler for EXWM Firefox bookmarks."
  (require 'ffap)
  (let ((url (bookmark-prop-get bookmark 'filename)))
    (if (ffap-url-p url)
        (browse-url-generic url)
      (message "Bookmark does not have a valid FILENAME property."))))

;;;###autoload
(defun cae-exwm-firefox-bookmark--make ()
  "Make bookmarks for web pages. Currently only works on Chromium
with the Add Page URL to Title extension."
  (let ((title (cae-exwm-firefox--current-title))
        (url (cae-exwm-firefox--current-url)))
    `((filename . ,url)
      (title . ,title)
      (time . ,(current-time-string))
      (handler . cae-exwm-firefox-bookmark-handler)
      (defaults . (,title)))))

;;;###autoload
(defun cae-exwm-clear-session-and-reload ()
  (interactive)
  (let* ((url (cae-exwm-firefox--current-url)))
    (exwm-input--fake-key ?\M-c)
    (exwm-firefox-core-tab-close)
    (exwm-firefox-core-tab-new)
    (let ((total-chars (length url))
          (typed-chars 0))

      ;; Show progress
      (message "Typing %d characters..." total-chars)

      ;; Process each character
      (dolist (char (string-to-list url))
        (pcase char
          ;; Special characters
          (?\n (exwm-input--fake-key 'return))
          (?\t (exwm-input--fake-key 'tab))
          (?\b (exwm-input--fake-key 'backspace))

          ;; Characters requiring shift
          ((guard (and (>= char ?A) (<= char ?Z)))
           (exwm-input--fake-key char))

          ;; Special symbols that might need shift
          (?! (exwm-input--fake-key ?!))
          (?@ (exwm-input--fake-key ?@))
          (?# (exwm-input--fake-key ?#))
          (?$ (exwm-input--fake-key ?$))
          (?% (exwm-input--fake-key ?%))
          (?^ (exwm-input--fake-key ?^))
          (?& (exwm-input--fake-key ?&))
          (?* (exwm-input--fake-key ?*))
          (?\( (exwm-input--fake-key ?\())
          (?\) (exwm-input--fake-key ?\)))

          ;; Default: regular character
          (_ (exwm-input--fake-key char)))

        ;; Update progress occasionally
        (setq typed-chars (1+ typed-chars))
        (when (zerop (% typed-chars 10))
          (message "Typing... %d/%d" typed-chars total-chars))

        ;; Small delay
        (sit-for cae-exwm-fake-type-delay)))
    (exwm-input--fake-key 'return)))
