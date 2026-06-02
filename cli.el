;;; cli.el -*- lexical-binding: t; -*-

;; Build a portable-dump (pdumper) image at the end of `doom sync' so launching
;; Emacs with --dump-file=<doom.pdmp> starts from a fully-initialized heap.
;;
;; Rather than only preloading package *code* (which leaves Doom's whole init to
;; run every startup, and -- because every package feature is already in
;; `features' -- makes `after!'/`with-eval-after-load' fire EAGERLY mid-init,
;; reordering config and breaking forward references), this image runs Doom's
;; *interactive* init to completion in batch (let-binding `noninteractive' to nil
;; so the precomputed module loaders run and `after!' fires in the correct
;; order), force-loads every package, and dumps that fully-wired heap.  At
;; runtime an advice baked into the image skips re-loading the module config
;; (already resident) while still restoring `load-path'/autoloads.
;;
;; `dump-emacs-portable' only works in -batch and terminates the process the
;; instant it's called, so we can't dump from inside the sync process itself.
;; Instead we generate a script and run it in a clean child `emacs -Q'.
;;
;; KNOWN CAVEAT: config is evaluated in batch, where `display-graphic-p' is nil
;; and there is no frame.  Anything that bakes a frame/display-dependent value at
;; *config time* captures the wrong value; the rest needs case-by-case
;; pre-declares like the ones in preamble.el.

(defun cae-pdump--inputs-key (build-dir)
  "Return a fingerprint of everything baked into the pdump image.
The image bakes the user's config, all built package code, AND whatever native
`.eln' files are loadable for this Emacs, so the key hashes all three (plus
Emacs/Doom versions).  Any change -- config, packages, Doom core, this build
logic, or newly-compiled elns (e.g. after an AOT native-compile) -- invalidates
the cached image so `doom sync' rebuilds it; an unchanged sync hashes the same
and is skipped."
  (let ((parts (list emacs-version (bound-and-true-p doom-version)))
        ;; User-writable eln-cache dirs (skip the read-only system native-lisp
        ;; dir, usually the last entry) -- their elns get baked into the image,
        ;; so a fresh native-compile must trigger a rebuild.
        (eln-dirs (cl-remove-if
                   (lambda (d) (string-prefix-p "/usr" (expand-file-name d)))
                   (bound-and-true-p native-comp-eln-load-path))))
    (dolist (f (sort (append
                      ;; Config: init.el, config.el, modules, cli.el, lisp/, ...
                      (directory-files-recursively doom-user-dir "\\.el\\'")
                      ;; Built package code (straight rewrites *.elc on rebuild).
                      (directory-files-recursively build-dir "\\.elc?\\'")
                      ;; Native code that will be loaded into (and dumped in) the
                      ;; image.
                      (cl-mapcan
                       (lambda (d)
                         (setq d (expand-file-name d))
                         (and (file-directory-p d)
                              (ignore-errors
                                (directory-files-recursively d "\\.eln\\'"))))
                       eln-dirs))
                     #'string<))
      (let ((a (file-attributes f)))
        (push (format "%s|%d|%s" f
                      (or (file-attribute-size a) -1)
                      (float-time (file-attribute-modification-time a)))
              parts)))
    (md5 (mapconcat #'identity (nreverse parts) "\n"))))

(defun cae-pdump--cleanup-form ()
  "Return forms that neutralize objects pdumper can't serialize after full init.
Full init creates un-dumpable objects deep inside nested structures (e.g. the
org-roam sqlite handle lives in an emacsql connection record reachable from two
roots), so a top-level-only scan misses them.  The scrub below overwrites the
offending object IN PLACE wherever it is held (record slot, vector, cons, hash
value, or a top-level var), so it's gone regardless of how many roots reach the
container.  Every such object is a runtime handle the owning package recreates
lazily after startup."
  '(;; 1. Delete live processes and clear pending timers (baking
    ;; `doom-after-init-hook' arms idle timers -- incremental loader, gcmh -- that
    ;; are stale at runtime and may hold unserializable references; each owner
    ;; re-arms its timer at runtime as needed).
    (dolist (proc (process-list)) (ignore-errors (delete-process proc)))
    (setq timer-list nil timer-idle-list nil)
    ;; 1a. This igc/MPS pdumper CORRUPTS live buffer TEXT (stray NUL bytes + an
    ;; out-of-range char at ~position 5), and displaying a corrupt buffer aborts
    ;; tty redisplay at startup ("bidi_get_type" / Fatal error 6 -- verified by
    ;; booting `-nw`).  So leave only EMPTY buffers in the dump: ERASE every
    ;; buffer's content.  Doom's dashboard and every real buffer regenerate fresh
    ;; at runtime.  We erase rather than kill so we don't break other buffers'
    ;; pending timers/state (killing e.g. ` *elfeed-curl*' orphans its callback).
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (let ((inhibit-read-only t) (buffer-undo-list t) (inhibit-modification-hooks t))
          (when (> (buffer-size) 0) (ignore-errors (erase-buffer))))))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (remove-overlays)
        (when (fboundp 'treesit-parser-list)
          (mapc #'treesit-parser-delete (treesit-parser-list)))))
    ;; 2. Rebuild top-level hash tables with user-defined tests as `equal' tables
    ;;    ("cannot dump hash tables with user-defined tests").
    (mapatoms
     (lambda (s)
       (when (and (boundp s) (not (keywordp s)))
         (let ((v (ignore-errors (symbol-value s))))
           (when (and (hash-table-p v)
                      (not (memq (hash-table-test v) '(eq eql equal))))
             (let ((new (make-hash-table :test 'equal
                                         :size (max 1 (hash-table-count v)))))
               (maphash (lambda (k val) (puthash k val new)) v)
               (set s new)))))))
    ;; 3. In-place scrub of un-dumpable pseudovector objects.
    (let ((cae-bad '(thread mutex condition-variable treesit-parser treesit-node
                     sqlite process terminal frame window window-configuration
                     module-function xwidget xwidget-view))
          (seen (make-hash-table :test 'eq)))
      (cl-labels ((badp (o) (and (memq (type-of o) cae-bad) (not (eq o main-thread))))
                  (scrub (obj depth)
                    (when (and obj (<= depth 8) (not (gethash obj seen)))
                      (puthash obj t seen)
                      (cond
                       ((consp obj)
                        (if (badp (car obj)) (setcar obj nil) (scrub (car obj) (1+ depth)))
                        (if (badp (cdr obj)) (setcdr obj nil) (scrub (cdr obj) (1+ depth))))
                       ((or (recordp obj) (and (vectorp obj) (not (stringp obj))))
                        (dotimes (i (length obj))
                          (let ((e (aref obj i)))
                            (if (badp e) (ignore-errors (aset obj i nil))
                              (scrub e (1+ depth))))))
                       ((hash-table-p obj)
                        (let (bad)
                          (maphash (lambda (k v) (if (badp v) (push k bad) (scrub v (1+ depth)))) obj)
                          (dolist (k bad) (remhash k obj))))))))
        (mapatoms (lambda (s)
                    (when (and (boundp s) (not (keywordp s)))
                      (let ((v (ignore-errors (symbol-value s))))
                        (if (badp v) (set s nil) (scrub v 0))))
                    (when (and (fboundp s)
                               (not (special-form-p (symbol-function s)))
                               (not (subrp (symbol-function s))))
                      (ignore-errors (scrub (symbol-function s) 0)))))
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (dolist (pair (buffer-local-variables buf))
              (when (consp pair)
                (if (badp (cdr pair))
                    (ignore-errors (set (make-local-variable (car pair)) nil))
                  (ignore-errors (scrub (cdr pair) 0)))))))))))

(defun cae-pdump-build ()
  "Build the full-config pdump image (`doom.pdmp'), if it is stale.
The (re)build is skipped when `cae-pdump--inputs-key' is unchanged, so a
`doom sync' that changed no config or package costs ~0.3s instead of minutes."
  (let* ((pdmp      (expand-file-name "doom.pdmp" doom-cache-dir))
         (build-dir (expand-file-name (format "straight/build-%s" emacs-version)
                                      doom-local-dir))
         (keyfile   (concat pdmp ".key"))
         (key       (cae-pdump--inputs-key build-dir)))
    (if (and (file-exists-p pdmp)
             (file-exists-p keyfile)
             (equal key (with-temp-buffer
                          (insert-file-contents keyfile)
                          (string-trim (buffer-string)))))
        (print! (success "pdump image already current; skipping rebuild"))
      (cae-pdump--dump pdmp keyfile key build-dir))))

(defun cae-pdump--dump (pdmp keyfile key build-dir)
  "Generate the loader script, run the child Emacs to dump PDMP, record KEY.
Dumps to a temp file and renames on success, so a failed build never clobbers a
working PDMP."
  (let ((script (make-temp-file "doom-pdump" nil ".el"))
        (tmp    (concat pdmp ".new")))
    (with-temp-file script
      (insert ";;; -*- lexical-binding: t; -*-\n")
      (let ((print-level nil) (print-length nil) (print-circle nil))
        (prin1
         `(progn
            (setq debug-on-error t
                  gc-cons-threshold most-positive-fixnum
                  gc-cons-percentage 1.0)
            ;; The read-only system native-lisp dir (last entry of the default
            ;; path), captured before any suppression.  It MUST be the LAST entry
            ;; of the eln-load-path we bake into the heap: at boot
            ;; `fixup_eln_load_path' overwrites the LAST cell with the system dir
            ;; (comp.c), so any user eln dir we want the runtime eln-fallback patch
            ;; to search must come BEFORE it (else a single-element list gets
            ;; clobbered and the fallback never sees the cache).
            (defvar cae--system-eln (car (last native-comp-eln-load-path)))
            ;; Will hold the runtime eln-load-path to bake into the heap; captured
            ;; AFTER `doom-startup' so it includes the dir Doom adds
            ;; (`doom-cache-dir'/eln/) -- otherwise the patched runtime loader
            ;; can't find user-cache elns that live there (e.g. a `disp-table'
            ;; copy), and the image fails to boot ("Error using execdir").
            (defvar cae--default-eln-load-path nil)
            ;; NATIVE-in-dump.  Don't COMPILE during the build (jit off, automatic
            ;; comp inhibited) -- just LOAD the native .eln files already in the
            ;; cache, so packages bake into the heap as native-comp-units.  Two
            ;; Emacs patches make this work: pdump-symbol-with-pos.patch lets
            ;; `dump-emacs-portable' serialize the `symbol-with-pos' (pseudovector
            ;; type 6) constants those comp-units carry (dumped as bare symbols),
            ;; and pdump-user-eln-fallback.patch lets the booted image resolve a
            ;; user-cache `.eln' under `native-comp-eln-load-path' instead of the
            ;; (nonexistent) execdir-relative system path.  That fallback only
            ;; works if the cache dir survives boot-time `fixup_eln_load_path'
            ;; (which clobbers the LAST entry) -- hence the system-dir-last restore
            ;; below.
            (setq native-comp-jit-compilation nil
                  inhibit-automatic-native-compilation t
                  native-comp-async-jobs-number 1
                  native-comp-enable-subr-trampolines nil)
            ;; Bootstrap Doom core + profile.  In batch, early-init.el sets
            ;; `user-emacs-directory' from its own path and runs `doom-initialize'
            ;; in its CLI branch (which does NOT load modules -- we do that next).
            (load (expand-file-name "early-init.el" ,doom-emacs-dir)
                  nil 'nomessage nil 'must-suffix)
            (defvar cae--orig-module-load (symbol-function 'module-load))
            ;; Stub `module-load' for the whole build: dynamic modules install
            ;; module-function objects (unserializable) via fset; restored below,
            ;; reloaded lazily at runtime.
            (fset 'module-load (lambda (&rest _) nil))
            ;; Put every straight package ROOT (not subdirs) on `load-path'.
            (dolist (dir (directory-files ,build-dir t "\\`[^.]"))
              (when (file-directory-p dir) (add-to-list 'load-path dir)))
            ;; Drive Doom's INTERACTIVE init in batch using Doom's OWN functions
            ;; (not copies of their internals).  `noninteractive'=nil makes `doom!'
            ;; a no-op (the precomputed module list is used) and makes `after!'
            ;; fire in the correct order as modules load.  Reset the context first
            ;; so `doom-initialize' takes its full interactive branch -- which sets
            ;; up the session (deferred first-input/file/buffer triggers, local-var
            ;; hooks, the command-line-1 `doom-finalize' advice) and loads the
            ;; profile init.  `doom-startup' (normally invoked by the
            ;; `startup--load-user-init-file' override) then loads module config;
            ;; it must run BEFORE the force-load so its `after!' blocks are
            ;; registered when each package loads.  `doom-finalize' runs later.
            (let ((noninteractive nil))
              (setq doom-context (list t))
              (doom-initialize t)
              (doom-startup))
            ;; Doom's init just added its own dir (`doom-cache-dir'/eln/) to
            ;; `native-comp-eln-load-path'.  Capture the path for the runtime
            ;; restore, forcing the read-only system dir LAST so boot-time
            ;; `fixup_eln_load_path' overwrites the system entry, not a cache dir
            ;; the eln-fallback needs.  Leave `native-comp-eln-load-path' itself
            ;; alone so the force-load below keeps loading native elns.
            (setq cae--default-eln-load-path
                  (append (delete cae--system-eln native-comp-eln-load-path)
                          (list cae--system-eln)))
            (princ "Interactive init complete; force-loading packages...\n")
            ;; Force-load every installed package (native, from the eln cache).
            ;; ~5-10 packages that can't load standalone are reported and skipped.
            (let ((file-name-handler-alist nil)
                  (pkgs (mapcar (lambda (d)
                                  (intern (file-name-nondirectory
                                           (directory-file-name d))))
                                (seq-filter
                                 #'file-directory-p
                                 (directory-files ,build-dir t "\\`[^.]")))))
              (dolist (p pkgs)
                (condition-case err
                    (require p nil 'noerror)
                  (error (princ (format "SKIP %s: %S\n" p err))))))
            ;; Restore the real `module-load' primitive (don't leave it stubbed).
            (fset 'module-load cae--orig-module-load)
            (princ "All packages loaded; finalizing...\n")
            ;; Run Doom's OWN `doom-finalize' (normally invoked by the
            ;; command-line-1 advice at runtime).  It fires `doom-after-init-hook'
            ;; -- which binds the LEADER KEY (`doom-init-leader-keys-h') and the
            ;; rest of the late setup -- so the session is fully wired into the
            ;; image.  This matters because from a fully-dumped heap, runtime
            ;; startup re-derives `user-emacs-directory'/`process-environment' from
            ;; the stale dumped values, early-init then can't locate `lisp/doom',
            ;; and `doom-initialize'/`doom-finalize' never run -- so without this,
            ;; the booted session has no Doom keybindings.
            (let ((noninteractive nil))
              (condition-case e (doom-finalize)
                (error (princ (format "doom-finalize error: %S\n" e)))))
            ;; `doom-initialize' advised `command-line-1' to run `doom-finalize'
            ;; at runtime.  We just ran it here, and the dumped context is reset
            ;; below, so the runtime firing would only re-pop a missing `startup'
            ;; context ("Incorrect context error" at every startup).  Drop it.
            (advice-remove 'command-line-1 #'doom-finalize)
            (princ "Preparing image...\n")
            ;; Guard baked into the image.  At runtime `doom-startup' is still
            ;; called (by the `startup--load-user-init-file' override).  The heavy
            ;; part -- `doom--startup-modules', which reloads every module's
            ;; init.el/config.el -- is already resident from the build, so skip it.
            ;; But STILL run the lightweight setup: `doom--startup-vars' (load-path,
            ;; auto-mode-alist) and the loaddefs.  Emacs resets `load-path' at
            ;; startup, so without re-running these, lazily-autoloaded package files
            ;; (e.g. `vertico-repeat', loaded on first `SPC .') resolve to "no such
            ;; file" because their package dir isn't on `load-path'.
            ;; `doom/reload' (context `reload') still runs the real thing.
            (defvar cae-pdump-active nil
              "Non-nil inside a heap dumped by `cae-pdump-build'.")
            (define-advice doom-startup (:around (orig &rest args) cae-pdump-skip)
              (if (and cae-pdump-active (not (doom-context-p 'reload)))
                  (progn
                    (when (fboundp 'doom--startup-vars)             (doom--startup-vars))
                    (when (fboundp 'doom--startup-loaddefs-doom)    (doom--startup-loaddefs-doom))
                    (when (fboundp 'doom--startup-loaddefs-packages) (doom--startup-loaddefs-packages))
                    t)
                (apply orig args)))
            ;; Safety net: force-loading every package makes some `after!'/
            ;; `eval-after-load' config fire at boot that normally stays deferred
            ;; (e.g. sly-setup loading a missing contrib), and an error there would
            ;; abort the whole startup.  Wrap each `after-init-hook' and
            ;; `emacs-startup-hook' function so a single failure is logged, not
            ;; fatal.
            (dolist (hook '(after-init-hook emacs-startup-hook window-setup-hook))
              (set hook
                   (mapcar (lambda (fn)
                             (if (memq fn '(t)) fn
                               (lambda (&rest _)
                                 (condition-case e (funcall fn)
                                   (error (message "cae-pdump: %s errored: %S" fn e))))))
                           (symbol-value hook))))
            (setq cae-pdump-active t)
            ;; Keep subr trampolines OFF at runtime: full init advises ~50 C
            ;; primitives (load, define-key, message, ...).  Intercepting a subr's
            ;; C callers needs a native trampoline .eln that can't survive this
            ;; dump (see above); with this nil, advice on primitives affects Lisp
            ;; callers via the symbol function cell only -- exactly how the build
            ;; itself applied them -- so no trampoline is ever requested.
            (setq native-comp-enable-subr-trampolines nil)
            ;; Restore runtime-correct native-comp values (eln cache findable, JIT
            ;; back on) so lazily-loaded files native-compile/load as usual.
            (setq native-comp-jit-compilation t
                  inhibit-automatic-native-compilation nil
                  native-comp-eln-load-path cae--default-eln-load-path)
            ;; Reset the context stack to its pristine value.
            (setq doom-context (list t))
            ;; Keep doom.el's `with-eval-after-load 'straight' block deferred to
            ;; its proper time (it would fire mid-doom.el from the dumped image).
            (setq features (delq 'straight features))
            ;; Neutralize objects pdumper can't serialize (created by full init).
            ,@(cae-pdump--cleanup-form)
            (dump-emacs-portable ,tmp))
         (current-buffer))))
    (print! (start "Building pdump image (this runs Doom's whole init)..."))
    ;; Optional BYTECODE-fallback escape hatch (set CAE_PDUMP_MOVE_ASIDE=1): move
    ;; the eln-cache version-dir aside for the build so nothing native loads and
    ;; everything bakes as bytecode.  Native-in-dump (the default) needs the cache
    ;; PRESENT -- the patched runtime loader resolves the baked native-comp-units
    ;; from it -- so this stays OFF by default.  Always restored (unwind-protect).
    (let* ((eln-vdir (and (getenv "CAE_PDUMP_MOVE_ASIDE")
                          (bound-and-true-p comp-native-version-dir)
                          (expand-file-name
                           comp-native-version-dir
                           (expand-file-name "eln/" doom-cache-dir))))
           (eln-hidden (and eln-vdir (concat (directory-file-name eln-vdir) ".pdump-hidden"))))
      (when (and eln-vdir (file-directory-p eln-vdir))
        (ignore-errors (rename-file eln-vdir eln-hidden t)))
      (unwind-protect
          (with-temp-buffer
            (let ((status (call-process "emacs" nil t nil "--batch" "-Q" "-l" script)))
              (if (and (eq status 0) (file-exists-p tmp))
                  (progn
                    ;; Atomically replace the live image, then record the fingerprint
                    ;; so the next sync can skip the rebuild when nothing changed.
                    (rename-file tmp pdmp t)
                    (with-temp-file keyfile (insert key))
                    (print! (success "Dumped to %s") pdmp))
                (ignore-errors (delete-file tmp))
                ;; Surface the child's stdout+stderr so the failure reason is visible.
                (print! (warn "pdump build failed (exit %s) — image left unchanged:" status))
                (print! "%s" (string-trim (buffer-string))))))
        (when (and eln-hidden (file-directory-p eln-hidden))
          (ignore-errors (rename-file eln-hidden eln-vdir t)))))
    (delete-file script)))

(add-hook 'doom-after-sync-hook #'cae-pdump-build)
