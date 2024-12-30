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
  (eshell-command "sudo emaint -a sync" t)
  (eshell-command "sudo emerge -uDUv @world --with-bdeps=y --changed-use --keep-going --color=y" t)
  (eshell-command "update-grub" t)
  (eshell-command "smart-live-rebuild" t)
  (eshell-command "pipx upgrade-all" t)
  (eshell-command "go-global-update" t)
  (eshell-command "update-emacs" t)

  ;; Update Docker images.
  (let ((images (split-string
                 (shell-command-to-string "docker images --format '{{.Repository}}:{{.Tag}}' | sort | uniq") "\n" t)))
    (dolist (image images)
      (message "Pulling Docker image: %s" image)
      (eshell-command (format "docker pull %s" image) t)))

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
        (eshell-command (format "docker stop %s" id) t)
        (eshell-command (format "docker rm %s" id) t)
        (eshell-command (format "docker run -d --name %s %s" name image) t))))

  ;; Return to the original directory.
  (eshell/popd))
