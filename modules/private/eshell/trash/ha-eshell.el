;;; private/eshell/ha-eshell.el -*- lexical-binding: t; -*-

;; There is more of Howard Abram's code in the autoload directory.

(defmacro prx (&rest expressions)
  "Convert the rx-compatible regular EXPRESSIONS to PCRE.
  Most shell applications accept Perl Compatible Regular Expressions."
  `(rx-let ((integer (1+ digit))
            (float   (seq integer "." integer))
            (b256    (seq (optional (or "1" "2"))
                          (regexp "[0-9]\\{1,2\\}")))
            (ipaddr  (seq b256 "." b256 "." b256 "." b256))
            (time    (seq digit (optional digit) ":" (= 2 digit) (optional ":" (= 2 digit))))
            (email   (seq (1+ (regexp "[^,< ]")) "@" (1+ (seq (1+ (any alnum "-"))) ".") (1+ alnum)))
            (date    (seq (= 2 digit) (or "/" "-") (= 2 digit) (or "/" "-") (= 4 digit)))
            (ymd     (seq (= 4 digit) (or "/" "-") (= 2 digit) (or "/" "-") (= 2 digit)))
            (uuid    (seq (= 8 hex) "-" (= 3 (seq (= 4 hex) "-")) (= 12 hex)))
            (guid    (seq uuid)))
     (rxt-elisp-to-pcre (rx ,@expressions))))

(defvar ha-eshell-output (make-ring 10)
  "A ring (looped list) storing history of eshell command output.")

(defun ha-eshell-store-last-output ()
  "Store the output from the last eshell command.
Called after every command by connecting to the `eshell-post-command-hook'."
  (when-let ((output
              (and eshell-last-input-end eshell-last-output-start
                   (buffer-substring-no-properties eshell-last-input-end
                                                   eshell-last-output-start))))
    (ring-insert ha-eshell-output output)))

(add-hook 'eshell-post-command-hook #'ha-eshell-store-last-output)

(defun eshell/output (&rest args)
  "Return an eshell command output from its history.

The first argument is the index into the historical past, where
`0' is the most recent, `1' is the next oldest, etc.

The second argument represents the returned output:
 * `text' :: as a string
 * `list' :: as a list of elements separated by whitespace
 * `file' :: as a filename that contains the output

If the first argument is not a number, it assumes the format
to be `:text'.
"
  (let (frmt element)
    (cond
     ((> (length args) 1)  (setq frmt (cadr args)
                                 element (car args)))
     ((= (length args) 0)  (setq frmt "text"
                                 element 0))
     ((numberp (car args)) (setq frmt "text"
                                 element (car args)))
     ((= (length args) 1)  (setq frmt (car args)
                                 element 0)))

    (if-let ((results (ring-ref ha-eshell-output (or element 0))))
        (cl-case (string-to-char frmt)
          (?l     (split-string results))
          (?f     (ha-eshell-store-file-output results))
          (otherwise (s-trim results)))
      "")))

(defun ha-eshell-store-file-output (results)
  "Writes the string, RESULTS, to a temporary file and returns that file name."
  (let ((filename (make-temp-file "ha-eshell-")))
    (with-temp-file filename
      (insert results))
    filename))

(defvar eshell-variable-aliases-list nil "Autoloading this eshell-defined variable")
(add-to-list 'eshell-variable-aliases-list '("$"  ha-eshell-output-text))
(add-to-list 'eshell-variable-aliases-list '(")"  ha-eshell-output-list))
(add-to-list 'eshell-variable-aliases-list '("OUTPUT" ha-eshell-output-file))

(defun ha-eshell-output (format-type indices)
  "Wrapper around `eshell/output' for the `eshell-variable-aliases-list'."
  (if indices
      (eshell/output (string-to-number (caar indices)) format-type)
    (eshell/output 0 format-type)))

(defun ha-eshell-output-text (&optional indices &rest ignored)
  "A _text_ wrapper around `eshell/output' for the `eshell-variable-aliases-list'."
  (ha-eshell-output "text" indices))

(defun ha-eshell-output-list (&optional indices &rest ignored)
  "A _list_ wrapper around `eshell/output' for the `eshell-variable-aliases-list'."
  (ha-eshell-output "list" indices))

(defun ha-eshell-output-file (&optional indices &rest ignored)
  "A _file_ wrapper around `eshell/output' for the `eshell-variable-aliases-list'."
  (ha-eshell-output "file" indices))

