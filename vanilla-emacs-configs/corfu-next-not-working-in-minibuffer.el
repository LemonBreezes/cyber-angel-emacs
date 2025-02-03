;;; vanilla-emacs-configs/corfu-next-not-working-in-minibuffer.el -*- lexical-binding: t; -*-

;; Bootstrap straight
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                                    'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'corfu)
(straight-use-package 'cape)

(scratch-buffer)
(insert "Pellentesque dapibus suscipit ligula. Donec posuere augue in quam. Etiam
vel tortor sodales tellus ultricies commodo. Suspendisse potenti. Aenean
in sem ac leo mollis blandit. Donec neque quam, dignissim in, mollis
nec, sagittis eu, wisi. Phasellus lacus. Etiam laoreet quam sed arcu.
Phasellus at dui in ligula mollis ultricies. Integer placerat tristique
nisl. Praesent augue. Fusce commodo. Vestibulum convallis, lorem a
tempus semper, dui dui euismod elit, vitae placerat urna tortor vitae
lacus. Nullam libero mauris, consequat quis, varius et, dictum id, arcu.
Mauris mollis tincidunt felis. Aliquam feugiat tellus ut neque. Nulla
facilisis, risus a rhoncus fermentum, tellus tellus lacinia purus, et
dictum nunc justo sit amet elit.")

(require 'corfu)
(require 'cape)
(setq global-corfu-minibuffer (lambda () t))

(defun +corfu-add-cape-dabbrev-h ()
  (add-hook 'completion-at-point-functions #'cape-dabbrev 20 t))
(add-hook 'minibuffer-setup-hook #'+corfu-add-cape-dabbrev-h)

(setq corfu-auto t
      corfu-auto-delay 0.24
      corfu-auto-prefix 2)
(read-string "%")
