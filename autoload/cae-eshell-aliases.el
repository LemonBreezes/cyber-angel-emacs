;;; autoload/cae-eshell-aliases.el -*- lexical-binding: t; -*-

;;;###autoload
(defun eshell/update-gentoo ()
  "Update Gentoo system, upgrade packages, and restart Docker containers."
  (interactive)
  ;; Set the default directory to root.
  (let ((default-directory "/"))
    ;; Run system update commands.
    (shell-command "sudo emaint -a sync")
    (shell-command "sudo emerge -uDUv @world --with-bdeps=y --changed-use --keep-going --color=y")
    (shell-command "update-grub")
    (shell-command "smart-live-rebuild")
    (shell-command "pipx upgrade-all")
    (shell-command "go-global-update")
    (shell-command "update-emacs"))

  ;; Update Docker images.
  (let ((images (split-string
                 (shell-command-to-string "docker images --format '{{.Repository}}:{{.Tag}}' | sort | uniq")
                 "\n" t)))
    (message "Docker images to update: %S" images)
    (dolist (image images)
      (when (and image (not (string= image "")))
        (message "Pulling Docker image: %s" image)
        (let ((result (shell-command (format "docker pull %s" (shell-quote-argument image)))))
          (message "Result of docker pull: %s" result)))))

  ;; Restart Docker containers.
  (let ((containers (split-string
                     (shell-command-to-string "docker ps --format '{{.ID}}|||{{.Image}}'")
                     "\n" t)))
    (message "Docker containers to restart: %S" containers)
    (dolist (container containers)
      (message "Processing container: %s" container)
      (let* ((fields (split-string container "|||")))
        (if (< (length fields) 2)
            (message "Skipping container due to unexpected format: %s" container)
          (let* ((id (string-trim (nth 0 fields)))
                 (image (string-trim (nth 1 fields))))

            ;; Validate image name
            (if (or (string-empty-p id)
                    (string-empty-p image)
                    (not (string-match-p "^[a-zA-Z0-9_/.-]+(:[a-zA-Z0-9_.-]+)?$" image)))
                (message "Skipping container due to invalid ID or Image.")
              ;; Get the container name.
              (let* ((name-output (shell-command-to-string
                                   (format "docker inspect --format='{{.Name}}' %s" (shell-quote-argument id))))
                     (name (replace-regexp-in-string "^/" "" (string-trim name-output))))
                (message "Container name: %s" name)
                (if (or (string-empty-p name))
                    (message "Skipping container due to empty Name.")
                  ;; Stop, remove, and restart the container.
                  (message "Stopping container ID: %s" id)
                  (shell-command (format "docker stop %s" (shell-quote-argument id)))
                  (message "Removing container ID: %s" id)
                  (shell-command (format "docker rm %s" (shell-quote-argument id)))
                  ;; Add any additional Docker run options here as needed.
                  (message "Starting container Name: %s, Image: %s" name image)
                  (shell-command (format "docker run -d --name %s %s" (shell-quote-argument name) (shell-quote-argument image)))))))))))
  (message "System update complete."))
