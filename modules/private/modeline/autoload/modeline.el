;;; private/modeline/autoload/modeline.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-modeline-create-file-buffer (filename)
  "Create a suitably named buffer for visiting FILENAME, and return it.
FILENAME (sans directory) is used unchanged if that name is free;
otherwise a string <2> or <3> or ... is appended to get an unused name.

Emacs treats buffers whose names begin with a space as internal buffers.
To avoid confusion when visiting a file whose name begins with a space,
this function prepends a \"|\" to the final result if necessary."
  (let* ((project (project-current nil (file-name-directory filename)))
         (name-base (if project
                        (file-relative-name filename (project-root project))
                      (file-name-nondirectory filename)))
	 (name-base (if (string= name-base "")
	               filename name-base))
	 (buf (generate-new-buffer (if (string-prefix-p " " name-base)
			               (concat "|" name-base)
			             name-base))))
    (uniquify--create-file-buffer-advice buf filename)
    buf))
(project-current nil (file-name-directory filename))
