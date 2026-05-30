;;; autoload/cae-packages.el -*- lexical-binding: t; -*-

;; Interactive commands to freeze every installed package to the SHA it is
;; *currently* checked out at, so that all of my Doom instances can be synced to
;; identical package versions.
;;
;; Doom's own `doom/bump-package*' commands pin to the *latest upstream* commit,
;; which is not what we want here.  These commands instead read the commit each
;; package's local repo is sitting on right now and write them out as Doom
;; `:pin's into `cae-packages-freeze-file' (packages.lock.el).  That file is
;; `load'ed at the tail of packages.el, so `doom sync' on any machine reproduces
;; the exact versions.
;;
;; CAVEAT: do NOT pair this with `(unpin! t)' in packages.el.  `unpin! t' stamps
;; `:unpin t' on every declared package, and `doom-package-pinned-alist' uses
;; `(unless unpin pin)', so it drops the pin of anything carrying `:unpin' --
;; which silently defeats every frozen pin.  The frozen pins already override
;; Doom's module pins by themselves (the file is loaded last, so its `:pin's
;; win), so `unpin! t' is both unnecessary and harmful here.

(defvar cae-packages-freeze-file
  (file-name-concat doom-user-dir "packages.lock.el")
  "File that `cae-packages-freeze' writes generated `:pin's into.
This file is `load'ed from the end of packages.el.")

(defvar cae-packages-freeze-excluded-packages nil
  "List of package symbols to never freeze.
Useful for packages you intentionally want to track upstream HEAD of.")

(defun cae-packages--current-pins ()
  "Return a sorted alist of (PACKAGE-NAME-STRING . COMMIT) for installed packages.
Enumerates every non-built-in package straight has a recipe for and reads the
commit its local repo is currently checked out at."
  (doom-initialize-packages)
  (let (pins)
    (dolist (recipe (doom-package-recipe-alist))
      (cl-destructuring-bind (&key package local-repo type &allow-other-keys)
          recipe
        (when (and package local-repo
                   (not (memq (intern package)
                              cae-packages-freeze-excluded-packages)))
          (when-let* ((commit (ignore-errors
                                (straight-vc-get-commit type local-repo))))
            (setf (alist-get package pins nil nil #'equal) commit)))))
    (cl-sort pins #'string< :key #'car)))

;;;###autoload
(defun cae-packages-freeze ()
  "Freeze every installed package to its currently checked-out SHA.
Writes `(package! NAME :pin \"SHA\")' forms for all installed packages into
`cae-packages-freeze-file' (packages.lock.el).  Commit that file (and
packages.el) and run `doom sync' on your other instances to put them on
identical versions."
  (interactive)
  (let ((pins (cae-packages--current-pins)))
    (unless pins
      (user-error "No installed packages found to freeze"))
    (with-temp-file cae-packages-freeze-file
      (insert ";; -*- no-byte-compile: t; -*-\n"
              ";;; packages.lock.el --- AUTO-GENERATED, DO NOT EDIT BY HAND\n;;\n"
              ";; Regenerate with `M-x cae-packages-freeze'.  Each entry pins a package\n"
              ";; to the commit it was installed at when this file was written.  Loaded\n"
              ";; from the end of packages.el.\n\n")
      (dolist (pin pins)
        (insert (format "(package! %s :pin %S)\n" (car pin) (cdr pin)))))
    (message "Froze %d package%s to %s"
             (length pins)
             (if (= (length pins) 1) "" "s")
             (abbreviate-file-name cae-packages-freeze-file))))

;;;###autoload
(defun cae-packages-unfreeze ()
  "Remove all frozen pins by clearing `cae-packages-freeze-file' (packages.lock.el).
Run `doom sync' afterwards to let packages float again."
  (interactive)
  (when (and (file-exists-p cae-packages-freeze-file)
             (or (not (called-interactively-p 'interactive))
                 (yes-or-no-p (format "Clear all frozen pins in %s? "
                                      (abbreviate-file-name
                                       cae-packages-freeze-file)))))
    (with-temp-file cae-packages-freeze-file
      (insert ";; -*- no-byte-compile: t; -*-\n"
              ";;; packages.lock.el --- AUTO-GENERATED, DO NOT EDIT BY HAND\n\n"))
    (message "Cleared frozen pins; run `doom sync' to thaw")))


;;; Bump frozen pins to upstream, reviewing the new commits in a magit log

(defvar cae-packages-bump-fetch-concurrency 16
  "Maximum number of concurrent `git fetch' jobs run by `cae-packages-bump-pins'.")

(defvar-local cae-packages--bump-specs nil
  "List of outdated package specs shown in a `cae-packages-bump-mode' buffer.
Each spec is a plist with :name :repo :old :new :count.")

(defvar-local cae-packages--bump-marked nil
  "Hash table of package names (strings) marked for bumping.")

(defconst cae-packages--bump-worker "\
b=$(git -C \"$1\" rev-parse --abbrev-ref HEAD 2>/dev/null)
git -C \"$1\" fetch --quiet origin \"$b\" 2>/dev/null
n=$(git -C \"$1\" rev-parse FETCH_HEAD 2>/dev/null)
printf '%s\\t%s\\t%s\\n' \"$1\" \"$n\" \"$(git -C \"$1\" rev-list --count \"$2..$n\" 2>/dev/null)\"
"
  "POSIX-sh worker run once per repo (args: $1=repo dir, $2=current pin).
Fetches the repo's current branch from origin and prints a TAB-separated
\"REPO<TAB>FETCH_HEAD<TAB>NEW-COMMIT-COUNT\" line.  Driven by `xargs -P' for
bounded concurrency.")

(defun cae-packages--lockfile-pins ()
  "Return an alist of (NAME-STRING . SHA) parsed from `cae-packages-freeze-file'."
  (let (pins)
    (when (file-readable-p cae-packages-freeze-file)
      (with-temp-buffer
        (insert-file-contents cae-packages-freeze-file)
        (goto-char (point-min))
        (while (re-search-forward
                "(package! \\([^ )]+\\) :pin \"\\([0-9a-f]+\\)\")" nil t)
          (push (cons (match-string 1) (match-string 2)) pins))))
    (nreverse pins)))

(defun cae-packages--bump-collect-specs ()
  "Return specs (plists :name :repo :old) for every pinned, on-disk package."
  (doom-initialize-packages)
  (let ((repo-by-name (make-hash-table :test #'equal))
        specs)
    (dolist (recipe (doom-package-recipe-alist))
      (cl-destructuring-bind (&key package local-repo &allow-other-keys) recipe
        (when (and package local-repo)
          (puthash package local-repo repo-by-name))))
    (pcase-dolist (`(,name . ,old) (cae-packages--lockfile-pins))
      (let* ((local-repo (gethash name repo-by-name))
             (dir (and local-repo (straight--repos-dir local-repo))))
        (when (and dir (file-directory-p dir))
          (push (list :name name :repo (directory-file-name dir) :old old)
                specs))))
    (nreverse specs)))

(defun cae-packages--bump-fetch (specs callback)
  "Fetch each spec's branch from origin asynchronously, then call CALLBACK.
Fetching is bounded by `cae-packages-bump-fetch-concurrency'.  CALLBACK receives
the subset of SPECS that have new upstream commits, each augmented with :new (the
upstream commit) and :count (number of new commits), sorted by :count."
  (let ((script (make-temp-file "cae-packages-bump-" nil ".sh"))
        (listf  (make-temp-file "cae-packages-bump-list-"))
        (total  (length specs))
        (by-repo (make-hash-table :test #'equal)))
    (with-temp-file script (insert cae-packages--bump-worker))
    (with-temp-file listf
      (dolist (spec specs)
        (puthash (plist-get spec :repo) spec by-repo)
        (insert (plist-get spec :repo) " " (plist-get spec :old) "\n")))
    (let ((proc (make-process
                 :name "cae-packages-bump-fetch"
                 :buffer (generate-new-buffer " *cae-packages-bump-fetch*")
                 :noquery t
                 :connection-type 'pipe
                 :command
                 (list shell-file-name shell-command-switch
                       (format "xargs -P %d -n 2 %s %s < %s"
                               cae-packages-bump-fetch-concurrency
                               shell-file-name
                               (shell-quote-argument script)
                               (shell-quote-argument listf)))
                 :filter #'cae-packages--bump-fetch-filter
                 :sentinel #'cae-packages--bump-fetch-sentinel)))
      ;; State travels on the process (via `process-put'), not via closures, so
      ;; the callbacks work regardless of how this file was loaded.
      (process-put proc 'reporter
                   (make-progress-reporter
                    (format "Fetching upstream for %d packages... " total) 0 total))
      (process-put proc 'total total)
      (process-put proc 'by-repo by-repo)
      (process-put proc 'callback callback)
      (process-put proc 'tempfiles (list script listf))
      proc)))

(defun cae-packages--bump-fetch-filter (proc string)
  "Process filter for `cae-packages--bump-fetch': accumulate output, show progress."
  (internal-default-process-filter proc string)
  (when (buffer-live-p (process-buffer proc))
    (progress-reporter-update
     (process-get proc 'reporter)
     (min (process-get proc 'total)
          (with-current-buffer (process-buffer proc)
            (count-lines (point-min) (point-max)))))))

(defun cae-packages--bump-fetch-sentinel (proc _event)
  "Process sentinel for `cae-packages--bump-fetch': parse results, run callback."
  (when (memq (process-status proc) '(exit signal))
    (let ((output   (with-current-buffer (process-buffer proc) (buffer-string)))
          (by-repo  (process-get proc 'by-repo))
          (callback (process-get proc 'callback)))
      (kill-buffer (process-buffer proc))
      (dolist (f (process-get proc 'tempfiles)) (ignore-errors (delete-file f)))
      (progress-reporter-done (process-get proc 'reporter))
      (let (outdated)
        (dolist (line (split-string output "\n" t))
          (pcase (split-string line "\t")
            (`(,repo ,new ,count)
             (let ((spec (gethash repo by-repo)))
               (when (and spec
                          (string-match-p "\\`[0-9a-f]\\{40,64\\}\\'" new)
                          (not (equal new (plist-get spec :old)))
                          (> (string-to-number count) 0))
                 (push (append spec (list :new new :count (string-to-number count)))
                       outdated))))))
        (funcall callback
                 (cl-sort outdated #'> :key (lambda (s) (plist-get s :count))))))))

(defvar cae-packages-bump-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")     #'cae-packages-bump-show-log)
    (define-key map (kbd "r")       #'cae-packages-bump-review)
    (define-key map (kbd "a")       #'cae-packages-bump-toggle-mark)
    (define-key map (kbd "A")       #'cae-packages-bump-mark-all)
    (define-key map (kbd "U")       #'cae-packages-bump-unmark-all)
    (define-key map (kbd "C-c C-c") #'cae-packages-bump-apply)
    map)
  "Keymap for `cae-packages-bump-mode'.")

(define-derived-mode cae-packages-bump-mode magit-section-mode "Package-Bumps"
  "Review and apply upstream pin bumps for frozen packages.
\\{cae-packages-bump-mode-map}")

(defun cae-packages--bump-spec-at-point ()
  "Return the package spec of the magit section at point, or nil."
  (when-let* ((section (magit-current-section))
              (value (oref section value)))
    (and (consp value) (plist-member value :name) value)))

(defun cae-packages--bump-goto (name)
  "Move point to the section for package NAME, if present."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (not (eobp)))
      (let ((spec (cae-packages--bump-spec-at-point)))
        (if (and spec (equal (plist-get spec :name) name))
            (setq found t)
          (forward-line 1))))
    (unless found (goto-char (point-min)))))

(defun cae-packages--bump-render ()
  "(Re)draw the bump overview buffer from `cae-packages--bump-specs'."
  (let ((inhibit-read-only t)
        (at (plist-get (cae-packages--bump-spec-at-point) :name)))
    (erase-buffer)
    (magit-insert-section (cae-packages-bump-root)
      (magit-insert-heading
        (format "Outdated frozen packages (%d)" (length cae-packages--bump-specs)))
      (insert "\n")
      (dolist (spec cae-packages--bump-specs)
        (let ((name (plist-get spec :name)))
          (magit-insert-section (cae-packages-bump-item spec)
            (magit-insert-heading
              (format "%s %-32s %s..%s  %d commit%s"
                      (if (gethash name cae-packages--bump-marked)
                          (propertize "*" 'face 'success)
                        " ")
                      name
                      (substring (plist-get spec :old) 0 7)
                      (substring (plist-get spec :new) 0 7)
                      (plist-get spec :count)
                      (if (= 1 (plist-get spec :count)) "" "s")))))))
    (when at (cae-packages--bump-goto at))))

(defun cae-packages-bump-show-log ()
  "Show a magit log of the new commits (old..new) for the package at point."
  (interactive)
  (let* ((spec (or (cae-packages--bump-spec-at-point)
                   (user-error "No package on this line")))
         (default-directory (file-name-as-directory (plist-get spec :repo))))
    ;; `magit-log-arguments' returns (ARGS FILES); pass ARGS, ignore file filter.
    (pcase-let ((`(,args ,_files) (magit-log-arguments)))
      (magit-log-setup-buffer
       (list (format "%s..%s" (plist-get spec :old) (plist-get spec :new)))
       args nil))))

(defun cae-packages-bump-toggle-mark ()
  "Toggle the bump mark on the package at point, then move to the next line."
  (interactive)
  (let* ((spec (or (cae-packages--bump-spec-at-point)
                   (user-error "No package on this line")))
         (name (plist-get spec :name)))
    (if (gethash name cae-packages--bump-marked)
        (remhash name cae-packages--bump-marked)
      (puthash name t cae-packages--bump-marked))
    (cae-packages--bump-render)
    (cae-packages--bump-goto name)
    (forward-line 1)))

(defun cae-packages-bump-mark-all ()
  "Mark every outdated package for bumping."
  (interactive)
  (dolist (spec cae-packages--bump-specs)
    (puthash (plist-get spec :name) t cae-packages--bump-marked))
  (cae-packages--bump-render))

(defun cae-packages-bump-unmark-all ()
  "Unmark every package."
  (interactive)
  (clrhash cae-packages--bump-marked)
  (cae-packages--bump-render))

(defun cae-packages--bump-write (specs)
  "Replace the :pin of each spec (with :new) in `cae-packages-freeze-file'."
  (with-temp-buffer
    (insert-file-contents cae-packages-freeze-file)
    (dolist (spec specs)
      (goto-char (point-min))
      (when (re-search-forward
             (format "(package! %s :pin \"\\([0-9a-f]+\\)\")"
                     (regexp-quote (plist-get spec :name)))
             nil t)
        (replace-match (plist-get spec :new) t t nil 1)))
    (write-region (point-min) (point-max) cae-packages-freeze-file nil 'silent)))

(defun cae-packages-bump-apply ()
  "Write the new pins for all marked packages into `cae-packages-freeze-file'."
  (interactive)
  (let ((marked (cl-remove-if-not
                 (lambda (s) (gethash (plist-get s :name) cae-packages--bump-marked))
                 cae-packages--bump-specs)))
    (unless marked
      (user-error "No packages marked; use `a' to mark, `A' to mark all"))
    (when (yes-or-no-p (format "Bump %d pin%s in %s? "
                               (length marked)
                               (if (= 1 (length marked)) "" "s")
                               (file-name-nondirectory cae-packages-freeze-file)))
      (cae-packages--bump-write marked)
      (let ((n (length marked)))
        (setq cae-packages--bump-specs
              (cl-remove-if (lambda (s)
                              (gethash (plist-get s :name) cae-packages--bump-marked))
                            cae-packages--bump-specs))
        (clrhash cae-packages--bump-marked)
        (if cae-packages--bump-specs
            (cae-packages--bump-render)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "All bumps applied.  Run `doom sync' to install them.\n")))
        (message "Bumped %d pin%s in %s; run `doom sync' to install"
                 n (if (= 1 n) "" "s")
                 (file-name-nondirectory cae-packages-freeze-file))))))


;;; AI review of a package's new commits (pluggable backend)

(defvar cae-packages-bump-review-api-endpoint nil
  "OpenAI-compatible chat-completions endpoint used to review commits.
When nil, it is derived from `cae-ip-address' at request time as
\"http://<cae-ip-address>:11434/v1/chat/completions\", matching the Ollama
server configured by the `cae/ai' module.  Set a full URL string to override,
e.g. any compatible server (OpenAI, OpenRouter, llama.cpp, vLLM, ...) -- the
request/response wire format is identical for all of them.")

(defvar cae-packages-bump-review-model nil
  "Model name sent to `cae-packages-bump-review-api-endpoint'.
When nil, falls back to `cae-chat-model' (the `cae/ai' general chat model) at
request time, so it tracks that setting.  Set a string to override.")

(defvar cae-packages-bump-review-api-key nil
  "Authorization for `cae-packages-bump-review-api-endpoint'.
Either a string (used as a Bearer token), a function of no arguments returning
one, or nil for endpoints that need no key (such as a local Ollama server).")

(defvar cae-packages-bump-review-function #'cae-packages--review-via-api
  "Function that runs an AI review.
Called with (PROMPT BUFFER).  It must send PROMPT to an LLM and insert the
response into BUFFER.  The default, `cae-packages--review-via-api', POSTs PROMPT
to `cae-packages-bump-review-api-endpoint'.  Replace it to use any other backend
\(gptel, llm, ...), e.g.:

  (setq cae-packages-bump-review-function
        (lambda (prompt buffer)
          (gptel-request prompt
            :callback (lambda (resp _info)
                        (when (stringp resp)
                          (with-current-buffer buffer (insert resp)))))))")

(defvar cae-packages-bump-review-include-diff nil
  "If non-nil, include the full `git diff' as well as the commit log and stat.
The full diff can be very large for big bumps; off by default.")

(defvar cae-packages-bump-review-warmup t
  "When non-nil, `cae-packages-bump-pins' preloads the review model.
A tiny throwaway request is fired at `cae-packages-bump-review-api-endpoint' as
the overview is fetched, so a local model (Ollama) is already warm in VRAM by
the time you press `r'.  Only done when `cae-packages-bump-review-function' is
the default API backend.")

(defvar cae-packages-bump-review-stream t
  "When non-nil, stream the review token-by-token via Server-Sent Events.
Requires the `curl' executable (used instead of `url.el', whose streaming is
unreliable).  Falls back to a single non-streamed request when nil or when curl
is unavailable.")

(defvar cae-packages-bump-review-prompt
  "You are reviewing whether to update an Emacs package's pinned Git commit.
Read the new commits (and diffstat) between the current pin and the candidate,
then write a concise summary: what changed, any breaking/API/behavior changes,
new dependencies or requirements, and anything risky for a downstream user.
Finish with one line exactly: \"Recommendation: SAFE | REVIEW | RISKY\"."
  "Instruction text prepended to the commit log/diff sent to the review backend.")

(defun cae-packages--git-string (dir &rest args)
  "Run \"git ARGS\" in DIR and return its trimmed standard output."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory dir)))
      (apply #'process-file "git" nil t nil args))
    (string-trim (buffer-string))))

(defun cae-packages--bump-review-input (spec)
  "Build the full review prompt (instructions + commit log + diffstat) for SPEC."
  (let ((repo (plist-get spec :repo))
        (range (format "%s..%s" (plist-get spec :old) (plist-get spec :new))))
    (concat
     cae-packages-bump-review-prompt "\n\n"
     (format "Package: %s\nCurrent pin: %s\nCandidate:   %s  (%d new commits)\n\n"
             (plist-get spec :name) (plist-get spec :old)
             (plist-get spec :new) (plist-get spec :count))
     "=== git log " range " ===\n"
     (cae-packages--git-string repo "log" "--no-merges"
                               "--pretty=format:* %h %s%n%w(0,2,2)%b" range)
     "\n\n=== git diff --stat " range " ===\n"
     (cae-packages--git-string repo "diff" "--stat" range)
     (when cae-packages-bump-review-include-diff
       (concat "\n\n=== git diff " range " ===\n"
               (cae-packages--git-string repo "diff" range))))))

(defun cae-packages--review-endpoint ()
  "Resolve the review endpoint URL (override, else derived from `cae-ip-address')."
  (or cae-packages-bump-review-api-endpoint
      (when-let* ((ip (bound-and-true-p cae-ip-address)))
        (format "http://%s:11434/v1/chat/completions" ip))
      (user-error "Set `cae-packages-bump-review-api-endpoint' (no `cae-ip-address')")))

(defun cae-packages--review-model ()
  "Resolve the review model (override, else `cae-chat-model')."
  (or cae-packages-bump-review-model
      (bound-and-true-p cae-chat-model)
      (user-error "Set `cae-packages-bump-review-model' (no `cae-chat-model')")))

(defun cae-packages--review-api-key ()
  "Resolve `cae-packages-bump-review-api-key' to a string or nil."
  (let ((k cae-packages-bump-review-api-key))
    (cond ((functionp k) (funcall k))
          ((stringp k) k))))

(defun cae-packages--review-insert (buffer text)
  "Replace BUFFER's contents with TEXT (if live), honoring read-only.
Swaps out the \"Querying...\" placeholder for the finished, non-streamed reply."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (goto-char (point-min))))))

(defvar cae-packages-bump-review-strip-reasoning t
  "When non-nil, strip <think>...</think> reasoning blocks from review output.
Reasoning models (e.g. Qwen3) wrap their answer in such a block; stripping it
leaves just the summary.")

(defun cae-packages--review-strip-think (text)
  "Strip leading <think>...</think> reasoning from TEXT when enabled."
  (if (and cae-packages-bump-review-strip-reasoning (stringp text))
      (string-trim
       (replace-regexp-in-string "<think>\\(?:.\\|\n\\)*?</think>" "" text))
    text))

(defun cae-packages--review-extract (json)
  "Return the assistant text from an OpenAI-compatible response JSON (alist)."
  (when (listp json)
    (let* ((choices (alist-get 'choices json))
           (choice  (and (vectorp choices) (> (length choices) 0) (aref choices 0)))
           (message (and choice (alist-get 'message choice))))
      (or (and message (cae-packages--review-strip-think
                        (alist-get 'content message)))
          ;; /api/generate style or error fallbacks
          (cae-packages--review-strip-think (alist-get 'response json))
          (let ((err (alist-get 'error json)))
            (and err (format "[API error] %s"
                             (if (listp err) (alist-get 'message err) err))))))))

(defun cae-packages--review-via-api (prompt buffer)
  "Default `cae-packages-bump-review-function'.
Stream the review into BUFFER via curl + SSE when `cae-packages-bump-review-stream'
is set and curl is available; otherwise fall back to a single non-streamed
request through `url.el'.  Both target the OpenAI-compatible endpoint."
  (if (and cae-packages-bump-review-stream (executable-find "curl"))
      (cae-packages--review-via-curl-stream prompt buffer)
    (cae-packages--review-via-url prompt buffer)))

(defun cae-packages--review-via-url (prompt buffer)
  "Non-streaming review: POST PROMPT to the endpoint via `url.el', reply to BUFFER."
  (let* ((key (cae-packages--review-api-key))
         (url-request-method "POST")
         (url-request-extra-headers
          (append '(("Content-Type" . "application/json"))
                  (and key `(("Authorization" . ,(concat "Bearer " key))))))
         (url-request-data
          (encode-coding-string
           (json-serialize
            `((model    . ,(cae-packages--review-model))
              (stream   . :false)
              (messages . [((role . "user") (content . ,prompt))])))
           'utf-8)))
    ;; Pass BUFFER through `url-retrieve' CBARGS, not a closure, so this works
    ;; regardless of how the file was loaded.
    (url-retrieve
     (cae-packages--review-endpoint)
     (lambda (status buf)
       (let ((resp (current-buffer)))
         (unwind-protect
             (if-let* ((err (plist-get status :error)))
                 (cae-packages--review-insert buf (format "[request error] %S" err))
               (goto-char (point-min))
               (if (not (re-search-forward "\n\n" nil t))
                   (cae-packages--review-insert buf "[no HTTP body in response]")
                 (let* ((body (buffer-substring-no-properties (point) (point-max)))
                        (json (ignore-errors
                                (json-parse-string body :object-type 'alist)))
                        (text (cae-packages--review-extract json)))
                   (cae-packages--review-insert
                    buf (or text (concat "[could not parse response]\n" body))))))
           (when (buffer-live-p resp) (kill-buffer resp)))))
     (list buffer) t t)))

(defun cae-packages--review-stream-display (raw)
  "Map accumulated RAW assistant text to display text, handling <think> blocks.
While a reasoning model is still inside <think>...</think>, show a progress
placeholder; once it closes (or when reasoning isn't stripped) show the text."
  (cond
   ((not cae-packages-bump-review-strip-reasoning) raw)
   ((string-match-p "</think>" raw)
    (string-trim (replace-regexp-in-string "<think>\\(?:.\\|\n\\)*?</think>" "" raw)))
   ((string-match-p "<think>" raw)
    (format "[reasoning... %d chars]" (length raw)))
   (t raw)))

(defun cae-packages--review-stream-delta (json)
  "Extract incremental assistant text from one SSE chunk JSON (alist)."
  (let* ((choices (alist-get 'choices json))
         (choice  (and (vectorp choices) (> (length choices) 0) (aref choices 0))))
    (and choice
         (let ((delta (alist-get 'delta choice)))
           (or (and delta (alist-get 'content delta))
               ;; some servers stream a full message object, not a delta
               (alist-get 'content (alist-get 'message choice)))))))

(defun cae-packages--review-stream-render (proc)
  "Render PROC's accumulated review text into its buffer, following output."
  (let ((buffer (process-get proc 'review-buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (win (get-buffer-window buffer t)))
          (erase-buffer)
          (insert (cae-packages--review-stream-display (process-get proc 'raw)))
          (when win (set-window-point win (point-max))))))))

(defun cae-packages--review-stream-filter (proc chunk)
  "Process filter: parse SSE CHUNK from curl, accumulate deltas, re-render."
  (process-put proc 'output (concat (process-get proc 'output) chunk))
  (let* ((pending (concat (process-get proc 'pending) chunk))
         (lines (split-string pending "\n"))
         (raw (process-get proc 'raw))
         (changed nil))
    ;; The final element is an as-yet-incomplete line; keep it for next chunk.
    (process-put proc 'pending (car (last lines)))
    (dolist (line (butlast lines))
      (setq line (string-trim-right line "\r"))
      (when (string-prefix-p "data:" line)
        (let ((payload (string-trim (substring line 5))))
          (unless (or (string-empty-p payload) (string= payload "[DONE]"))
            (when-let* ((json (ignore-errors
                                (json-parse-string payload :object-type 'alist)))
                        (delta (cae-packages--review-stream-delta json)))
              (when (> (length delta) 0)
                (setq raw (concat raw delta) changed t)))))))
    (when changed
      (process-put proc 'raw raw)
      (cae-packages--review-stream-render proc))))

(defun cae-packages--review-stream-sentinel (proc event)
  "Process sentinel: finalize the streamed review or surface an error."
  (when (memq (process-status proc) '(exit signal))
    (let ((buffer (process-get proc 'review-buffer))
          (raw    (process-get proc 'raw))
          (code   (process-exit-status proc)))
      (cae-packages--review-stream-render proc)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (cond
             ;; Nothing arrived as SSE: the backend likely returned a plain
             ;; (error) body -- surface it rather than leave a blank buffer.
             ((string-empty-p raw)
              (erase-buffer)
              (let* ((out  (string-trim (process-get proc 'output)))
                     (json (ignore-errors (json-parse-string out :object-type 'alist)))
                     (text (and json (cae-packages--review-extract json))))
                (insert (or text
                            (if (string-empty-p out)
                                (format "[no output from review backend (curl exit %d)]" code)
                              out)))))
             ((not (zerop code))
              (goto-char (point-max))
              (insert (format "\n\n[review: curl exited %d: %s]"
                              code (string-trim event)))))))))))

(defun cae-packages--review-via-curl-stream (prompt buffer)
  "Stream a review via curl + SSE from the OpenAI-compatible endpoint into BUFFER."
  (let* ((key  (cae-packages--review-api-key))
         (body (json-serialize
                `((model    . ,(cae-packages--review-model))
                  (stream   . t)
                  (messages . [((role . "user") (content . ,prompt))]))))
         (args (append
                (list "--silent" "--show-error" "--no-buffer" "--max-time" "600"
                      (cae-packages--review-endpoint)
                      "-H" "Content-Type: application/json")
                (and key (list "-H" (concat "Authorization: Bearer " key)))
                (list "--data-binary" "@-")))
         (proc (make-process
                :name "cae-packages-bump-review"
                :buffer nil
                :noquery t
                :connection-type 'pipe
                :coding 'utf-8
                :command (cons "curl" args)
                :filter   #'cae-packages--review-stream-filter
                :sentinel #'cae-packages--review-stream-sentinel)))
    (process-put proc 'review-buffer buffer)
    (process-put proc 'raw "")
    (process-put proc 'pending "")
    (process-put proc 'output "")
    (process-send-string proc body)
    (process-send-eof proc)
    proc))

(defun cae-packages--review-warmup ()
  "Best-effort: fire a tiny request to preload the local review model.
No-op unless `cae-packages-bump-review-warmup' is set and the default API
backend is in use.  Errors (e.g. unset endpoint) are ignored."
  (when (and cae-packages-bump-review-warmup
             (eq cae-packages-bump-review-function #'cae-packages--review-via-api))
    (ignore-errors
      (let* ((key (cae-packages--review-api-key))
             (url-request-method "POST")
             (url-request-extra-headers
              (append '(("Content-Type" . "application/json"))
                      (and key `(("Authorization" . ,(concat "Bearer " key))))))
             (url-request-data
              (encode-coding-string
               (json-serialize
                `((model      . ,(cae-packages--review-model))
                  (stream     . :false)
                  (max_tokens . 1)
                  (keep_alive . "30m") ; Ollama: keep it resident for the session
                  (messages   . [((role . "user") (content . "ok"))])))
               'utf-8)))
        (url-retrieve (cae-packages--review-endpoint)
                      (lambda (_status) (ignore-errors (kill-buffer (current-buffer))))
                      nil t t)))))

(defun cae-packages-bump-review ()
  "Summarize the new commits for the package at point with an AI backend.
Useful when a bump has too many commits to eyeball.  The backend is
`cae-packages-bump-review-function' (by default an OpenAI-compatible HTTP
request to `cae-packages-bump-review-api-endpoint'), so it can target Ollama,
OpenAI, or any compatible endpoint."
  (interactive)
  (let* ((spec (or (cae-packages--bump-spec-at-point)
                   (user-error "No package on this line")))
         (name (plist-get spec :name))
         (buffer (get-buffer-create (format "*cae bump review: %s*" name))))
    (with-current-buffer buffer
      (when (and (fboundp 'gfm-mode) (not (derived-mode-p 'gfm-mode)))
        (gfm-mode))
      (setq-local header-line-format
                  (format "AI review: %s  %s..%s  (%d commits)"
                          name (substring (plist-get spec :old) 0 7)
                          (substring (plist-get spec :new) 0 7)
                          (plist-get spec :count)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Placeholder so the buffer doesn't look dead while a (possibly slow,
        ;; reasoning) model generates with stream disabled.
        (insert "Querying the model... (a reasoning model can take a while)\n\n")))
    (display-buffer buffer)
    (message "Reviewing %s (%d commits) via %s..."
             name (plist-get spec :count)
             (if (eq cae-packages-bump-review-function
                     #'cae-packages--review-via-api)
                 (format "%s @ %s" (cae-packages--review-model)
                         (cae-packages--review-endpoint))
               cae-packages-bump-review-function))
    (funcall cae-packages-bump-review-function
             (cae-packages--bump-review-input spec)
             buffer)))

;;;###autoload
(defun cae-packages-bump-pins ()
  "Fetch all frozen packages and review/apply pin bumps in an overview buffer.
Each pinned package's current branch is fetched from its origin; those with new
upstream commits are listed in a `cae-packages-bump-mode' buffer.  There, press
RET to view a magit log of the new commits (old..new), `a' to mark a package
\(`A' marks all, `U' unmarks), and \\`C-c C-c' to write the new pins into
`cae-packages-freeze-file'.  Run `doom sync' afterwards to install them."
  (interactive)
  (require 'magit)
  (let ((specs (cae-packages--bump-collect-specs)))
    (unless specs
      (user-error "No pinned packages found in %s"
                  (file-name-nondirectory cae-packages-freeze-file)))
    ;; Warm up the local review model now so the first `r' review is fast --
    ;; it loads into VRAM while the (network-bound) fetch runs.
    (cae-packages--review-warmup)
    (message "Fetching upstream for %d frozen packages (C-g to abort)..."
             (length specs))
    (cae-packages--bump-fetch
     specs
     (lambda (outdated)
       (if (null outdated)
           (message "All frozen packages are already up to date.")
         (let ((buf (get-buffer-create "*cae package bumps*")))
           (with-current-buffer buf
             (cae-packages-bump-mode)
             (setq cae-packages--bump-specs outdated
                   cae-packages--bump-marked (make-hash-table :test #'equal)
                   header-line-format
                   " RET=log  r=AI-review  a=mark  A=mark-all  U=unmark-all  C-c C-c=apply  q=quit")
             (cae-packages--bump-render)
             (goto-char (point-min)))
           (pop-to-buffer buf)
           (message "%d package%s have upstream updates."
                    (length outdated)
                    (if (= 1 (length outdated)) "" "s"))))))))

;; Bind in evil normal/motion states too; otherwise evil shadows the single-key
;; bindings in `cae-packages-bump-mode-map'.
(map! :map cae-packages-bump-mode-map
      :nm "RET"     #'cae-packages-bump-show-log
      :nm "r"       #'cae-packages-bump-review
      :nm "a"       #'cae-packages-bump-toggle-mark
      :nm "A"       #'cae-packages-bump-mark-all
      :nm "U"       #'cae-packages-bump-unmark-all
      :nm "q"       #'quit-window
      :nm "C-c C-c" #'cae-packages-bump-apply)
