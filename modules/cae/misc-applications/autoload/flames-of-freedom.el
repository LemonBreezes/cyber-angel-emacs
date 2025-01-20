;;; private/misc-applications/autoload/flames-of-freedom.el -*- lexical-binding: t; -*-

(cae-define-launcher
  cae-flames-of-freedom
  :launch-fn #'flames-of-freedom-default
  :workspace-name cae-flames-of-freedom-workspace-name
  :cleanup-fn cae-flames-of-freedom-quit)

(defun cae-flames-of-freedom-quit ()
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p cae-flames-of-freedom-workspace-name)
        (+workspace/kill cae-flames-of-freedom-workspace-name))
    (when cae-flames-of-freedom--old-wconf
      (set-window-configuration cae-flames-of-freedom--old-wconf))))

(advice-add #'flames-of-freedom-default :after #'cae-flames-of-freedom-quit)
