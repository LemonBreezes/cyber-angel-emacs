;;; cae/ai/autoload/pi-coding-agent.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-pi-coding-agent-toggle ()
  (interactive)
  (require 'pi-coding-agent)
  (when (one-window-p)
    (split-window-horizontally)
    (other-window 1))
  (let ((buf (if (derived-mode-p 'pi-coding-agent-chat-mode 'pi-coding-agent-input-mode)
                 (pi-coding-agent--get-chat-buffer)
               (car (pi-coding-agent-project-buffers)))))
    (if (buffer-live-p buf)
        (call-interactively #'pi-coding-agent-toggle))
    (call-interactively #'pi-coding-agent)))
