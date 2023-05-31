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
    "--ytdl-format=bestvideo[height<=?720]+bestaudio/best")
  (message "Starting streaming..."))
