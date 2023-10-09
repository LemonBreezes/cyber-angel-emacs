;;; private/misc-applications/autoload/ytel.el -*- lexical-binding: t; -*-

;;;###autoload
(defun ytel-watch ()
  "Stream video at point in mpv."
  (interactive)
  (let* ((video (ytel-get-current-video))
     	 (id    (ytel-video-id video)))
    (start-process "ytel mpv" nil
                   "mpv"
		   (concat "https://www.youtube.com/watch?v=" id))
    (princ (concat "https://www.youtube.com/watch?v=" id))
    "--ytdl-format=bestvideo[height<=?720]+bestaudio/best")
  (message "Starting streaming..."))

(defvar invidious-instances-url
  "https://api.invidious.io/instances.json?pretty=1&sort_by=health")

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

;;;###autoload
(defun ytel-choose-instance ()
  "Prompt user to choose an invidious instance to use."
  (interactive)
  (setq ytel-invidious-api-url
        (or (condition-case nil
                (completing-read "Using instance: "
                                 (cl-subseq (ytel-instances-alist-from-json) 0 11) nil "confirm" "https://") ; "healthiest" 12 instances; no require match
              (error nil))
            "invidious.tube"))          ; fallback
  (doom-store-put 'ytel-invidious-api-url ytel-invidious-api-url))


;;;###autoload (autoload '+ytel-hydra/body "cae/misc-applications/autoload/ytel" nil t)
(defhydra +ytel-hydra (:color pink :hint nil)
  ("<f6>" nil "Exit" :exit t)
  ("q" ytel-quit nil :exit t)
  ("s" ytel-search "Search")
  ("RET" ytel-watch "Watch"))
