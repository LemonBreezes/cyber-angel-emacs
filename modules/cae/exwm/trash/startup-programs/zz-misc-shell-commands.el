;;; startup/misc-shell-commands.el -*- lexical-binding: t; -*-

(defvar startup/misc-shell-commands-buffer
  (get-buffer-create " *misc shell commands*"))

(defvar startup/pipewire-process nil)

;; Run our GPU at max power.
;; (call-process-shell-command "nvidia-settings -a '[gpu:0]/gpupowermizermode=1'" nil startup/misc-shell-commands-buffer)

;; Run our CPU at max power.
(let ((command "echo 'performance' | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor"))
  (start-process "cpu scaling governor" startup/misc-shell-commands-buffer "sh" "-c" command))

;; Allow commands running under Sudo to access the diplay.
(start-process "xhost" startup/misc-shell-commands-buffer "xhost" "+")

;; Set our refresh rate to 144Hz.
;;(ignore-errors
;;  (call-process-shell-command "xrandr --output HDMI-0 --mode 2560x1440 -r 143.91" nil startup/misc-shell-commands-buffer))

;; Start up Pipewire.
;;(when (and (executable-find "gentoo-pipewire-launcher")
;;           (not (process-live-p startup/pipewire-process)))
;;  (setq startup/pipewire-process
;;        (start-process "pipewire" startup/misc-shell-commands-buffer "gentoo-pipewire-launcher")))

;; Start solaar for my Logitech mouse
(defvar startup/solaar-process nil)
(when (and (executable-find "solaar")
           (not (process-live-p startup/solaar-process)))
  (setq startup/solaar-process
        (start-process "solaar" startup/misc-shell-commands-buffer "solaar" "--window=hide")))

;; Start the Music Player Daemon
;;(defvar startup/mpd-process nil)
;;(when (and (executable-find "mpd")
;;           (not (process-live-p startup/mpd-process)))
;;  (setq startup/mpd-process
;;        (start-process "mpd" startup/misc-shell-commands-buffer "mpd" "--no-daemon" "--stdout")))

;; Start up Talon.
;; (start-process "talon" startup/misc-shell-commands-buffer "~/src/talon/run.sh")

;; Start up the Immersed VR streamer.
;; (defun startup/manage-immersed ()
;;   (when (string= "Immersed" exwm-class-name)
;;     (exwm-floating--unset-floating (exwm--buffer->id (current-buffer)))))
;; (add-hook 'exwm-floating-setup-hook #'startup/manage-immersed)

;; (start-process "immersed" startup/misc-shell-commands-buffer "Immersed-x86_64.AppImage")


;; (when (and (bound-and-true-p geth-wallet-address)
;;            (bound-and-true-p geth-wallet-password-file))
;;   (defvar startup/geth-process
;;     (start-process "geth" (get-buffer-create " *geth*") "geth"
;;                    "--http"
;;                    "--unlock" geth-wallet-address "--password" geth-wallet-password-file
;;                    "--http.corsdomain" "*"
;;                    "--http.api" "eth,net,web3"
;;                    "--allow-insecure-unlock"
;;                    "--ws"
;;                    "--cache" "4096"
;;                    "--maxpendpeers" "128"
;;                    "--maxpeers" "128"
;;                    "--light.maxpeers" "30"
;;                    "--light.serve" "30"
;;                    "--nat" "any"
;;                    )))

;; Start up Rtorrent.
;; (start-process "rtorrent" startup/misc-shell-commands-buffer "rtorrent" "-o" "system.daemon.set=true")
