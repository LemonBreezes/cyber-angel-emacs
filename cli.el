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
  (let ((parts (list emacs-version (bound-and-true-p doom-version)
                     ;; A bare dev-version string (e.g. "32.0.50") stays constant
                     ;; across rebuilds of the same branch, but a pdump is only
                     ;; loadable by the EXACT Emacs build that produced it.  Hash
                     ;; the build's commit + timestamp so ANY Emacs rebuild
                     ;; invalidates the cached image; otherwise an Emacs-only
                     ;; rebuild (no config/package/eln change) hashes the same
                     ;; key, sync SKIPS the rebuild, and the stale image fails to
                     ;; boot with "not built for this Emacs executable".
                     (bound-and-true-p emacs-repository-version)
                     (format "%S" (bound-and-true-p emacs-build-time))))
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
  '(;; 0. Undo the doom CLI's output redirection, which `doom-cli--init'
    ;; installs for the running `doom sync' command and which is LIVE when we
    ;; dump: an `:override' advice on `message' (routing every message through
    ;; `doom-print' into the now-dead ` *doom-cli stdout/stderr*' buffers) plus a
    ;; redirected `standard-output'/`doom-print-stream'.  Baked into the image,
    ;; this makes runtime `message' a silent no-op -- e.g. `+workspace/display'
    ;; runs but nothing reaches the echo area.  Strip the advice (restoring the
    ;; pristine `#<subr message>') and reset the streams to their stock defaults.
    (advice-mapc (lambda (fn _props) (advice-remove 'message fn)) 'message)
    (setq-default standard-output t)
    (when (boundp 'doom-print-stream) (setq-default doom-print-stream t))
    ;; 1. Delete live processes and clear pending timers (baking
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
                  ;; A `treesit-compiled-query' baked into the dump comes back
                  ;; with a DEAD language pointer (the grammar it points at can't
                  ;; be serialized), and `treesit-query-compile' on a compiled
                  ;; query uses the query's OWN language, not the one passed in --
                  ;; so re-validating it at runtime loads `libtree-sitter-nil' and
                  ;; errors (e.g. opening a Haskell file -> `haskell-ts-font-lock',
                  ;; whose rules are compiled eagerly in a load-time `defvar').
                  ;; Replace each with its SOURCE form -- still recoverable HERE,
                  ;; pre-dump, while the grammar is live -- so the owning mode
                  ;; recompiles it fresh against the runtime grammar.
                  (queryp (o) (and (fboundp 'treesit-compiled-query-p)
                                   (treesit-compiled-query-p o)))
                  (qsource (o) (ignore-errors (treesit-query-source o)))
                  (scrub (obj depth)
                    (when (and obj (<= depth 8) (not (gethash obj seen)))
                      (puthash obj t seen)
                      (cond
                       ((consp obj)
                        ;; Walk the list SPINE iteratively so a long list (e.g.
                        ;; `haskell-ts-font-lock', 16 settings) doesn't exhaust
                        ;; DEPTH down the cdr-chain -- only real nesting (each
                        ;; car) should cost depth, not list length.
                        (let ((cell obj))
                          (while (consp cell)
                            (let ((a (car cell)))
                              (cond ((queryp a) (setcar cell (qsource a)))
                                    ((badp a)   (setcar cell nil))
                                    (t (scrub a (1+ depth)))))
                            (let ((d (cdr cell)))
                              (cond ((queryp d) (setcdr cell (qsource d)) (setq cell nil))
                                    ((badp d)   (setcdr cell nil)         (setq cell nil))
                                    ((and (consp d) (not (gethash d seen)))
                                     (puthash d t seen)
                                     (setq cell d))
                                    ((consp d)  (setq cell nil)) ; already seen -> stop
                                    (t (scrub d (1+ depth)) (setq cell nil)))))))
                       ((or (recordp obj) (and (vectorp obj) (not (stringp obj))))
                        (dotimes (i (length obj))
                          (let ((e (aref obj i)))
                            (cond ((queryp e) (ignore-errors (aset obj i (qsource e))))
                                  ((badp e)   (ignore-errors (aset obj i nil)))
                                  (t (scrub e (1+ depth)))))))
                       ((hash-table-p obj)
                        (cond
                         ;; Leave native-comp's loaded-units registry untouched:
                         ;; it's weak-VALUE with STRING keys (sxhash-safe, can't
                         ;; crash thaw) and the eln-fixup below still enumerates it
                         ;; -- clearing it makes `dump-emacs-portable' abort with
                         ;; "trying to dump non fixed-up eln file".
                         ((eq obj (bound-and-true-p comp-loaded-comp-units-h)) nil)
                         ;; Empty every OTHER weak hash table.  They are
                         ;; GC-collectable caches the owner repopulates lazily, but
                         ;; the igc/MPS pdumper mangles some of their entries so the
                         ;; load-time `thaw_hash_tables' rehash ABORTS in `sxhash'
                         ;; (`emacs_abort' on an invalid type tag) and the booted
                         ;; image SEGFAULTS before printing anything -- observed
                         ;; after a package bump as a weak-key `equal' table whose
                         ;; one compound key (list -> list -> vector -> object) held
                         ;; a corrupted object.  The scrub never recurses into hash
                         ;; KEYS, and a cold cache is what a non-dumped Emacs has.
                         ((hash-table-weakness obj) (clrhash obj))
                         (t
                          (let (bad fixq)
                            (maphash (lambda (k v)
                                       (cond ((queryp v) (push (cons k (qsource v)) fixq))
                                             ((badp v)   (push k bad))
                                             (t (scrub v (1+ depth)))))
                                     obj)
                            (dolist (k bad) (remhash k obj))
                            (dolist (kv fixq) (puthash (car kv) (cdr kv) obj))))))))))
        (mapatoms (lambda (s)
                    (when (and (boundp s) (not (keywordp s)))
                      (let ((v (ignore-errors (symbol-value s))))
                        (cond ((queryp v) (set s (qsource v)))
                              ((badp v)   (set s nil))
                              (t (scrub v 0)))))
                    (when (and (fboundp s)
                               (not (special-form-p (symbol-function s)))
                               (not (subrp (symbol-function s))))
                      (ignore-errors (scrub (symbol-function s) 0)))))
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (dolist (pair (buffer-local-variables buf))
              (when (consp pair)
                (cond ((queryp (cdr pair))
                       (ignore-errors
                         (set (make-local-variable (car pair)) (qsource (cdr pair)))))
                      ((badp (cdr pair))
                       (ignore-errors (set (make-local-variable (car pair)) nil)))
                      (t (ignore-errors (scrub (cdr pair) 0))))))))))))

(defvar cae-pdump-native-compile-jobs (num-processors)
  "Parallel jobs for the pdump AOT native-compile pass.
Defaults to half the CPUs to bound peak RAM (each libgccjit job is heavy).")

(defvar cae-pdump-preload-themes t
  "Which themes to bake (DISABLED) into the pdump for fast runtime switching.
`load-theme' with both NO-CONFIRM and NO-ENABLE records a theme's
`theme-settings' (its face/variable specs) without enabling it; that data
survives the dump, so switching to the theme at runtime is a cheap
`enable-theme' instead of re-reading and evaluating its file.

t bakes every `custom-available-themes' (largest image, any theme switches
instantly); a list of theme symbols restricts it to those (smaller image);
nil disables the preload entirely.")

(defun cae-pdump--eln-count ()
  "Number of `.eln' files in this Emacs's user eln cache."
  (length (ignore-errors
            (directory-files-recursively
             (expand-file-name comp-native-version-dir
                               (expand-file-name "eln/" doom-cache-dir))
             "\\.eln\\'"))))

(defun cae-pdump--native-compile (build-dir)
  "AOT-native-compile the packages under BUILD-DIR that need it, before dumping.
The pdump image only bakes NATIVE code for a package whose `.eln' already exists
at build time (the build merely LOADS what's on disk), so compile the rest up
front.  Truly incremental: a source whose `.eln' is present and current is never
queued -- and `native-compile-async' has NO such up-to-date check itself (it
would queue all ~3000 files and spawn a checker subprocess for each, wasting
minutes even when nothing changed), so we pre-filter here.  Autoload/-pkg stubs,
which are never natively compiled, are excluded too.  Blocks until the queue
drains, then reports how many elns were actually produced."
  (require 'comp)
  ;; Async children inherit the parent's `load-path' (comp-run.el binds it into
  ;; their startup form), so put every package root on it -- otherwise their
  ;; macro/`require' dependencies don't resolve and compilation fails.
  (dolist (dir (directory-files build-dir t "\\`[^.]"))
    (when (file-directory-p dir) (add-to-list 'load-path dir)))
  ;; `comp-el-to-eln-filename' resolves against the first writable entry of
  ;; `native-comp-eln-load-path'; Doom puts its eln cache there, which is exactly
  ;; where these elns live, so existence == "already compiled, current".
  (let ((stale (cl-remove-if
                (lambda (el)
                  (or (string-match-p "\\(?:-autoloads\\|-pkg\\)\\.el\\'" el)
                      (let ((eln (ignore-errors (comp-el-to-eln-filename el))))
                        (and eln (file-exists-p eln)
                             (file-newer-than-file-p eln el)))))
                (directory-files-recursively build-dir "\\.el\\'"))))
    (when stale
      (print! (start "Native-compiling %d new/changed file(s) (%d jobs)...")
              (length stale) cae-pdump-native-compile-jobs)
      (let ((before (cae-pdump--eln-count))
            (native-comp-async-jobs-number cae-pdump-native-compile-jobs)
            (native-comp-async-report-warnings-errors 'silent)
            (native-comp-verbose 0)
            (inhibit-message t))
        (native-compile-async stale nil)
        (while (or (bound-and-true-p comp-files-queue)
                   (and (fboundp 'comp--async-runnings) (cl-plusp (comp--async-runnings))))
          (accept-process-output nil 1))
        (let ((delta (- (cae-pdump--eln-count) before)))
          (if (zerop delta)
              ;; The leftover stale files are test/no-native-compile sources that
              ;; never produce an eln -- nothing actually needed building.
              (print! (success "Native compilation already up to date"))
            (print! (success "Native-compiled %d new file(s)") delta)))))))

(defun cae-pdump--clean-module-elc ()
  "Delete stale `.elc' files sitting next to Doom module source files.
Doom modules MUST load as source.  A bare `(modulep! +flag)' in a module file
has TWO macroexpansions (see `modulep!'): a constant fast-path when
`doom-module-context' is bound at expansion time -- which `doom--startup-modules'
does, per-module, when loading SOURCE -- and a fragile by-path runtime fallback
otherwise.  An auto-compiler on `load' (here `compile-angel', see
lisp/cae-compile.el) byte-compiles these files STANDALONE, with NO module
context, so the fallback gets baked into the `.elc' and then fails at load with
`void-variable +flag' (observed: `+eglot' in `:tools lsp').  Because `load'
prefers `.elc' over `.el', a single such file silently breaks EVERY subsequent
module load -- the pdump child's `doom-startup' AND a normal (non-dumped) boot.
Stock Doom never compiles module configs, so these `.elc' are always spurious;
strip them before every build so modules reload from source."
  (let ((n 0))
    (dolist (dir (bound-and-true-p doom-module-load-path))
      (when (and (stringp dir) (file-directory-p dir))
        (dolist (elc (ignore-errors (directory-files-recursively dir "\\.elc\\'")))
          (when (ignore-errors (delete-file elc) t) (cl-incf n)))))
    (when (cl-plusp n)
      (print! (warn "Removed %d stale module .elc (modules must load as source)" n)))))

(defun cae-pdump-build ()
  "Build the full-config pdump image (`doom.pdmp'), if it is stale.
First runs an incremental AOT native-compile so the image bakes native code,
then (re)builds the dump -- skipped when `cae-pdump--inputs-key' is unchanged,
so a `doom sync' that changed no config or package costs only the (near-zero)
incremental compile scan instead of minutes."
  (let* ((pdmp      (expand-file-name "doom.pdmp" doom-cache-dir))
         (build-dir (expand-file-name (format "straight/build-%s" emacs-version)
                                      doom-local-dir))
         (keyfile   (concat pdmp ".key")))
    ;; Strip spurious module `.elc' first: they shadow source with a broken
    ;; `modulep!' expansion and would abort the dump child's config load (and any
    ;; non-dumped boot).  Done unconditionally -- even on a skipped rebuild -- so
    ;; the on-disk module tree is always loadable from source.
    (cae-pdump--clean-module-elc)
    ;; Native-compile BEFORE fingerprinting so KEY reflects the resulting elns
    ;; (an unchanged sync then hashes the same key and skips the dump).
    (cae-pdump--native-compile build-dir)
    (let ((key (cae-pdump--inputs-key build-dir)))
      (if (and (file-exists-p pdmp)
               (file-exists-p keyfile)
               (equal key (with-temp-buffer
                            (insert-file-contents keyfile)
                            (string-trim (buffer-string)))))
          (print! (success "pdump image already current; skipping rebuild"))
        (cae-pdump--dump pdmp keyfile key build-dir)))))

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
            ;; --- pdump oddity fixups ------------------------------------------
            ;; Some global state does not survive `dump-emacs-portable': a heap
            ;; booted from this image comes up with `global-font-lock-mode' and
            ;; `transient-mark-mode' OFF (so code shows no syntax colors and the
            ;; region is inert) even though a normally-started Emacs has them on.
            ;; Define ONE named, self-removing fixup and arrange for it to run on
            ;; the FIRST REAL FRAME, then register the installer HERE -- before the
            ;; config loads below -- so it is the baseline the rest of init builds
            ;; on.  This is the single place to collect any future "works in a
            ;; normal Emacs, broken only when booted from the dump" repairs.
            (defun cae-pdump-fix-oddities-h (&rest _)
              "Restore global state that `dump-emacs-portable' fails to carry over.
Runs once, on the first real frame of an Emacs booted from the pdump image,
then removes itself.  Add further pdump-only fixups here as discovered."
              (remove-hook 'server-after-make-frame-hook #'cae-pdump-fix-oddities-h)
              (unless global-font-lock-mode (global-font-lock-mode 1))
              (unless transient-mark-mode   (transient-mark-mode 1))
              ;; `dump-emacs-portable' drops the `defvar-local' that
              ;; `define-globalized-minor-mode' generates for evil:
              ;; `evil-mode--set-explicitly' comes back with NO default value AND
              ;; without its automatic buffer-localness.  `evil-mode-enable-in-buffer'
              ;; (on `after-change-major-mode-hook') then signals a void-variable for
              ;; every new buffer, so evil silently never turns on -- in text buffers,
              ;; EXWM buffers, everywhere.  Restore the binding, re-assert evil's
              ;; default-`major-mode' alias (evil's own `:after' advice sets this so
              ;; Fundamental buffers still trip the hook; bug#23827), then initialize
              ;; evil in the live buffers that missed it during the dumped startup.
              (when (featurep 'evil)
                (unless (default-boundp 'evil-mode--set-explicitly)
                  (set-default 'evil-mode--set-explicitly nil))
                (make-variable-buffer-local 'evil-mode--set-explicitly)
                (when (bound-and-true-p evil-mode)
                  (when (and (eq (default-value 'major-mode) 'fundamental-mode)
                             (fboundp 'evil--fundamental-mode))
                    (setq-default major-mode #'evil--fundamental-mode))
                  (dolist (buf (buffer-list))
                    (with-current-buffer buf
                      (unless (or (bound-and-true-p evil-local-mode)
                                  (minibufferp)
                                  (and (fboundp 'evil-disabled-buffer-p)
                                       (evil-disabled-buffer-p)))
                        (ignore-errors (evil-initialize))))))))
            (defun cae-pdump-install-fixups-h ()
              "Schedule `cae-pdump-fix-oddities-h' to run once, on the first frame.
The `daemonp' test must happen at RUNTIME (the dump is always built batch,
never a daemon), so this installer runs from `emacs-startup-hook'.  On a
daemon there is no frame until a client connects -- wait for
`server-after-make-frame-hook' (fires once the daemon is connected to);
otherwise (`-nw'/GUI) the initial frame already exists by now, so apply the
fixups immediately."
              (if (daemonp)
                  (add-hook 'server-after-make-frame-hook #'cae-pdump-fix-oddities-h)
                (cae-pdump-fix-oddities-h)))
            (add-hook 'emacs-startup-hook #'cae-pdump-install-fixups-h)
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
            ;; Engage `cae-after-frame!' deferral (preamble.el) for the duration
            ;; of init: the macro checks `cae-pdump--building' to decide whether
            ;; to run its body NOW or defer it to the first real frame at runtime.
            ;; Without this the deferral never triggers and display/WM/tty-
            ;; dependent config (e.g. `cae-theme', `cae-tty') loads HERE in batch,
            ;; baking wrong predicate values -- which is why the dumped theme came
            ;; up empty.  Reset to nil afterward so a runtime `doom/reload' (which
            ;; re-runs config) behaves like a normal session.
            (setq cae-pdump--building t)
            ;; --- surface the REAL config-load failure -------------------------
            ;; This child is a bare `emacs -Q', but early-init's CLI branch leaves
            ;; `debug-on-error' t with `doom-cli-debugger' installed -- so ANY error
            ;; while loading config trips that debugger, which calls `(exit! 255)'
            ;; -> `(throw 'exit ...)'.  Nothing here catches `exit', so the genuine
            ;; cause (e.g. a void autoloaded fn from an uninitialized module
            ;; submodule) gets buried under an inscrutable `no-catch exit (255)'.
            ;; Force `debug-on-error' OFF across init so a plain `condition-case'
            ;; catches the real signal; `handler-bind' grabs a backtrace at signal
            ;; time (before the stack unwinds); a `catch 'exit' backstops any stray
            ;; `exit!'.  On failure, print the UNWRAPPED root cause (Doom nests the
            ;; true error under `doom-*-error' file wrappers) and quit with a
            ;; distinct, non-255 status so the parent's report is unambiguous.
            ;; Restore `debug-on-error' for the dump phase on success.
            (let* ((cae-init-error nil)
                   (cae-init-bt nil)
                   (cae-init-outcome
                    (catch 'exit
                      (let ((noninteractive nil))
                        (setq doom-context (list t)
                              debug-on-error nil)
                        (condition-case err
                            (handler-bind
                                ((error (lambda (_e)
                                          (unless cae-init-bt
                                            (setq cae-init-bt
                                                  (with-output-to-string (backtrace)))))))
                              (doom-initialize t)
                              (setq debug-on-error nil)
                              (doom-startup))
                          (error (setq cae-init-error err))))
                      'completed)))
              (setq debug-on-error t)
              (when (or cae-init-error (not (eq cae-init-outcome 'completed)))
                (princ "\n\n=== cae-pdump: CONFIG FAILED TO LOAD -- dump aborted ===\n")
                (if (null cae-init-error)
                    (princ (format "Init aborted via `exit!' (%S); real error printed above.\n"
                                   cae-init-outcome))
                  (let ((root cae-init-error))
                    (while (and (consp root)
                                (memq (car root)
                                      '(doom-user-error doom-core-error doom-cli-error
                                        doom-profile-error doom-module-error))
                                (stringp (cadr root)) (consp (caddr root)))
                      (setq root (caddr root)))
                    (princ (format "Real error: %s\n" (error-message-string root)))
                    (princ (format "Wrapped as: %S\n" cae-init-error))
                    (when cae-init-bt
                      (princ "Backtrace (newest first):\n")
                      (princ cae-init-bt))))
                (kill-emacs 47)))
            (setq cae-pdump--building nil)
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
            ;; Force-load the autoload files Doom otherwise lazy-loads, so the
            ;; helpers they define WITHOUT a `;;;###autoload' cookie (e.g.
            ;; `+workspace--tabline', called directly by `+workspace/display' and
            ;; the which-key advice in ui.el) are baked into the heap.  Doom's
            ;; loaddefs carry only the cookied forms, so in the dump those bare
            ;; helpers stay void until the first call to a cookied sibling lazily
            ;; loads the file -- and any direct caller before that hits
            ;; `void-function'.  Mirrors the package force-load above; covers
            ;; every ENABLED module (core :doom and the user's `:cae/...' modules)
            ;; via `doom-module-list', PLUS the private config's own top-level
            ;; autoloads ($DOOMDIR/autoload.el and $DOOMDIR/autoload/*.el).
            (princ "Force-loading module + config autoloads...\n")
            (let ((file-name-handler-alist nil)
                  (roots (append (delq nil (mapcar #'doom-module-locate-path
                                                   (doom-module-list)))
                                 (list doom-user-dir))))
              (dolist (dir roots)
                (dolist (f (append
                            (let ((af (expand-file-name "autoload.el" dir)))
                              (and (file-exists-p af) (list af)))
                            (let ((ad (expand-file-name "autoload" dir)))
                              (and (file-directory-p ad)
                                   (directory-files-recursively ad "\\.el\\'")))))
                  (condition-case err
                      (load f nil 'nomessage 'nosuffix)
                    (error (princ (format "SKIP autoload %s: %S\n" f err)))))))
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
            ;; Re-derive env-dependent `custom-initialize-delay' vars at RUNTIME.
            ;; This batch build's own `command-line' already re-evaluated every
            ;; such var from the BUILD environment and then set
            ;; `custom-delayed-init-variables' to t -- both of which the dump
            ;; freezes.  A heap booted from the image reaches the same
            ;; re-evaluation block (startup.el) but, finding the variable t (not a
            ;; list), skips it -- so the build-time values survive.  For
            ;; `temporary-file-directory' that is wrong: it bakes in $TMPDIR as it
            ;; was during the build (e.g. a transient `/tmp/…' sandbox dir that no
            ;; longer exists after a reboot), so `make-temp-file' then fails with
            ;; "No such file or directory" -- which breaks `envrc-global-mode' on
            ;; `doom-first-file-hook'.  Re-list it so the booted Emacs re-derives
            ;; it from the LIVE environment; that block runs early in
            ;; `command-line', before any file is visited.  (Other delay vars stay
            ;; frozen as before -- only env-derived ones need this.)
            (setq custom-delayed-init-variables '(temporary-file-directory))
            ;; Bake theme DATA (face/variable specs) into the heap WITHOUT
            ;; enabling any: `load-theme' with NO-CONFIRM + NO-ENABLE records a
            ;; theme's `theme-settings' but leaves `custom-enabled-themes' and the
            ;; live faces untouched.  That data survives the dump, so switching to
            ;; any baked theme at runtime is a cheap `enable-theme' rather than
            ;; re-reading and evaluating its file.  (Themes are on the load path
            ;; now that every package was force-loaded above.)
            (let ((want ',cae-pdump-preload-themes))
              (when want
                (dolist (th (if (eq want t) (custom-available-themes) want))
                  (ignore-errors (load-theme th t t)))))
            ;; Neutralize objects pdumper can't serialize (created by full init).
            ,@(cae-pdump--cleanup-form)
            ;; Every loaded native comp-unit's `file' slot must be a CONS
            ;; (install-rel . build-rel) or `dump-emacs-portable' aborts with
            ;; "trying to dump non fixed-up eln file" (pdumper.c).  Upstream sets
            ;; this in `load--fixup-all-elns' (loadup.el) -- but only when Emacs
            ;; is built with --bin-dest/--eln-dest, which our `emacs -Q' batch
            ;; lacks, so every comp-unit's file is still the bare load-time
            ;; STRING.  Fix them up here.  Both halves are the `.eln's path
            ;; relative to THIS build's bindir (`invocation-directory'); since
            ;; the runtime Emacs is the SAME binary, boot-time
            ;; `emacs_execdir'+rel reconstructs the original absolute path and
            ;; the unit loads (the user-eln-fallback patch is the backstop if
            ;; the eln-cache later moves).  Comp-units loaded FROM the system
            ;; emacs.pdmp come back as strings too (pdumper resets the slot on
            ;; load), so this re-fixes them as well; ones already consed are
            ;; left alone.
            (let ((cus (make-hash-table :test 'eq))
                  (fixed 0) (already 0) (collect nil))
              (setq collect (lambda (cu) (when cu (puthash cu t cus))))
              ;; (a) the load registry, (b) every native subr reachable
              ;; through a symbol's function OR value cell -- the dumper walks
              ;; ALL reachable comp-units, so the registry alone is not enough
              ;; (a unit re-loaded under the same name evicts the old one from
              ;; the registry while it stays reachable via its subrs).
              (when (boundp 'comp-loaded-comp-units-h)
                (maphash (lambda (_ cu) (funcall collect cu))
                         comp-loaded-comp-units-h))
              (mapatoms
               (lambda (s)
                 (when (fboundp s)
                   (let ((f (symbol-function s)))
                     (when (subr-native-elisp-p f)
                       (funcall collect (subr-native-comp-unit f)))))
                 (when (boundp s)
                   (let ((v (ignore-errors (symbol-value s))))
                     (when (subr-native-elisp-p v)
                       (funcall collect (subr-native-comp-unit v)))))))
              (maphash
               (lambda (cu _)
                 (let ((f (native-comp-unit-file cu)))
                   (if (stringp f)
                       (let ((rel (file-relative-name f invocation-directory)))
                         (native-comp-unit-set-file cu (cons rel rel))
                         (setq fixed (1+ fixed)))
                     (setq already (1+ already)))))
               cus)
              (princ (format "eln fixup: %d comp-units (%d fixed, %d already consed)\n"
                             (hash-table-count cus) fixed already)))
            (dump-emacs-portable ,tmp))
         (current-buffer))))
    (print! (start "Building pdump image (this runs Doom's whole init)..."))
    (with-temp-buffer
      ;; `dump-emacs-portable' on this igc/MPS build runs single-threaded at 100%
      ;; CPU for 6-18 min and is TOTALLY SILENT -- indistinguishable from a hang,
      ;; which has repeatedly looked like a stuck sync.  The child can't heartbeat
      ;; itself (its elisp is frozen inside the C-level dump), so run it async from
      ;; HERE and emit an elapsed-time pulse every 30s.  `accept-process-output'
      ;; drives the wait without busy-looping (batch has no running timers); stderr
      ;; mixes into this buffer exactly as `call-process' did, so the failure path
      ;; still surfaces the child's output.
      ;;
      ;; CRITICAL: redirect the child's stdin from /dev/null via a shell.  Unlike
      ;; `call-process' (whose nil INFILE means /dev/null, so stdin reads hit EOF),
      ;; `make-process' wires stdin to a pipe WE never write to -- so any prompt the
      ;; dumped init reaches (a stray `y-or-n-p'/`read' during `doom-startup' with
      ;; `noninteractive' nil) blocks forever on that pipe.  `exec ... </dev/null'
      ;; restores the EOF-on-read behavior; `exec' keeps the child's exit status.
      (let* ((proc (make-process
                    :name "doom-pdump"
                    :buffer (current-buffer)
                    :command (list "sh" "-c"
                                   (concat "exec emacs --batch -Q -l "
                                           (shell-quote-argument script)
                                           " </dev/null"))
                    :connection-type 'pipe
                    :noquery t))
             (start-time (float-time))
             (next 30)
             (fmt (lambda (secs) (format "%dm%02ds" (/ secs 60) (% secs 60)))))
        (while (process-live-p proc)
          (accept-process-output proc 1)
          (let ((elapsed (round (- (float-time) start-time))))
            (when (>= elapsed next)
              (print! (item "still dumping... (%s elapsed, this can take 6-18 min)"
                            (funcall fmt elapsed)))
              (setq next (+ next 30)))))
        (let ((status (process-exit-status proc))
              (elapsed (round (- (float-time) start-time))))
          (if (and (eq status 0) (file-exists-p tmp))
              (progn
                ;; Atomically replace the live image, then record the fingerprint so
                ;; the next sync can skip the rebuild when nothing changed.
                (rename-file tmp pdmp t)
                (with-temp-file keyfile (insert key))
                (print! (success "Dumped to %s (took %s)") pdmp (funcall fmt elapsed)))
            (ignore-errors (delete-file tmp))
            ;; Surface the child's stdout+stderr so the failure reason is visible.
            (print! (warn "pdump build failed (exit %s) — image left unchanged:" status))
            (print! "%s" (string-trim (buffer-string)))))))
    (delete-file script)))

(add-hook 'doom-after-sync-hook #'cae-pdump-build)
