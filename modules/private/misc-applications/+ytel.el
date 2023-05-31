;;; private/misc-applications/+ytel.el -*- lexical-binding: t; -*-

(use-package! ytel
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        "y" #'ytel)
  :config
  (setq ytel-invidious-api-url "https://invidious.snopyta.org/")
  (define-key ytel-mode-map (kbd "<return>") #'ytel-watch))

(defvar invidious-instances-url
  "api.invidious.io/instances.json?pretty=1&sort_by=health")

(defun ytel-instances-fetch-json ()
  "Fetch list of invidious instances as json, sorted by health."
  (let
      ((url-request-method "GET")
       (url-request-extra-headers
        '(("Accept" . "application/json"))))
    (with-current-buffer
        (url-retrieve-synchronously invidious-instances-url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (json-key-type 'string))
        (json-read)))))

(defun ytel-instances-alist-from-json ()
  "Make the json of invidious instances into an alist."
  (let ((jsonlist (ytel-instances-fetch-json))
        (inst ()))
    (while jsonlist
      (push (concat "https://" (caar jsonlist)) inst)
      (setq jsonlist (cdr jsonlist)))
    (nreverse inst)))

(defun ytel-choose-instance ()
  "Prompt user to choose an invidious instance to use."
  (interactive)
  (setq ytel-invidious-api-url
        (or (condition-case nil
                (completing-read "Using instance: "
                                 (subseq (ytel-instances-alist-from-json) 0 11) nil "confirm" "https://") ; "healthiest" 12 instances; no require match
              (error nil))
            "invidious.tube"))) ; fallback

;; default fallback "invidious.synopyta.org"

(defun ytel-watch ()
  "Stream video at point in mpv."
  (interactive)
  (let* ((video (ytel-get-current-video))
         (id (ytel-video-id video)))
    (start-process "ytel mpv" nil
                   "mpv"
                   (concat "youtube.com/watch?v=" id))
    "--ytdl-format=bestvideo[height<=?720]+bestaudio/best")
  (message "Starting streaming..."))

;; (define-key ytel-mode-map "y" #'ytel-watch)

;; youtube-comments

;; xFA25E/ytel-show
;; (use-package ytel-show
;; :after ytel
;; :bind (:map ytel-mode-map ("RET" . ytel-show))
;; )
