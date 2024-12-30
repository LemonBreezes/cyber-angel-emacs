;;; autoload/cae-eshell-aliases.el -*- lexical-binding: t; -*-

;;;###autoload
(defun eshell/update-gentoo ()
  "Update Gentoo system, upgrade packages, and restart Docker containers."
  (interactive)
  ;; Save the current directory.
  (eshell/pushd ".")

  ;; Change to the root directory.
  (eshell/cd "/")

  ;; Run system update commands.
  (eshell-command "sudo emaint -a sync")
  (eshell-command "sudo emerge -uDUv @world --with-bdeps=y --changed-use --keep-going --color=y")
  (eshell-command "update-grub")
  (eshell-command "smart-live-rebuild")
  (eshell-command "pipx upgrade-all")
  (eshell-command "go-global-update")
  (eshell-command "update-emacs")

  ;; Update Docker images.
  (let ((images (split-string
                 (shell-command-to-string "docker images --format '{{.Repository}}:{{.Tag}}' | sort | uniq") "\n" t)))
    (dolist (image images)
      (message "Pulling Docker image: %s" image)
      (eshell-command (format "docker pull %s" image))))

  ;; Restart Docker containers.
  (let ((containers (split-string
                     (shell-command-to-string "docker ps --format '{{.ID}} {{.Image}}'") "\n" t)))
    (dolist (container containers)
      (let* ((fields (split-string container " "))
             (id (car fields))
             (image (cadr fields))
             (name (string-trim-left
                    (shell-command-to-string
                     (format "docker inspect --format='{{.Name}}' %s" id))
                    "/")))
        (message "Restarting Docker container: %s (ID: %s, Image: %s)" name id image)
        (eshell-command (format "docker stop %s" id))
        (eshell-command (format "docker rm %s" id))
        (eshell-command (format "docker run -d --name %s %s" name image)))))

  ;; Return to the original directory.
  (eshell/popd))
