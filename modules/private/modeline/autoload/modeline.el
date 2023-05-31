;;; private/modeline/autoload/modeline.el -*- lexical-binding: t; -*-

(defun cae-create-file-buffer (filename)
  "Create a suitably named buffer for visiting FILENAME, and return it.
FILENAME (sans directory) is used unchanged if that name is free;
otherwise a string <2> or <3> or ... is appended to get an unused name.

Emacs treats buffers whose names begin with a space as internal buffers.
To avoid confusion when visiting a file whose name begins with a space,
this function prepends a \"|\" to the final result if necessary."
  (let* ((vc-backend (ignore-errors (vc-responsible-backend filename)))
         (vc-base-path (when vc-backend
                         (vc-call-backend vc-backend 'root filename)))
         (name-base (if vc-base-path
                        (file-relative-name filename vc-base-path)
                      (file-name-nondirectory filename)))
	 (name-base (if (string= name-base "")
	               filename name-base))
	 (buf (generate-new-buffer (if (string-prefix-p " " name-base)
			               (concat "|" name-base)
			             name-base))))
    (uniquify--create-file-buffer-advice buf filename)
    buf))
