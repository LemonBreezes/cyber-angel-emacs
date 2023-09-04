;;; lisp/improve-shr-performance.el -*- lexical-binding: t; -*-

(after! shr
  (defun +shr-pretty-table (fun dom)
    ;; Only prettify tables which have a header, since tables are often abused for layout.
    (let ((shr-table-vertical-line
           (and (or (dom-by-tag dom 'thead) (dom-by-tag dom 'th))
                #("   " 1 2 (display (space :width (1))))))) ; face (:background "black")
      (funcall fun dom)))

  (defun +shr-indirect-call (tag-name dom &rest args)
    (let ((function (pcase (dom-tag dom)
                      ('p #'shr-tag-p)
                      ('ol #'shr-tag-ol)
                      ('ul #'shr-tag-ul)
                      ('li #'shr-tag-li)
                      ('h1 #'shr-tag-h1)
                      ('h2 #'shr-tag-h2)
                      ('h3 #'shr-tag-h3)
                      ('h4 #'shr-tag-h4)
                      ('h5 #'shr-tag-h5)
                      ('h6 #'shr-tag-h6)
                      ('div #'shr-tag-div)
                      ('em #'shr-tag-em)
                      ('code #'shr-tag-code)
                      ('table #'shr-tag-table)
                      ('a #'shr-tag-a)
                      (_ (intern (concat "shr-tag-" (symbol-name (dom-tag dom))) obarray))))
	  ;; Allow other packages to override (or provide) rendering
	  ;; of elements.
	  (external (cdr (assq tag-name shr-external-rendering-functions))))
      (cond (external
	     (apply external dom args))
	    ((fboundp function)
	     (apply function dom args))
	    (t
             (apply #'shr-generic dom args)))))

  (defun +shr-descend (dom)
    (let ((function
           (pcase (dom-tag dom)
             ('p #'shr-tag-p)
             ('ol #'shr-tag-ol)
             ('ul #'shr-tag-ul)
             ('li #'shr-tag-li)
             ('h1 #'shr-tag-h1)
             ('h2 #'shr-tag-h2)
             ('h3 #'shr-tag-h3)
             ('h4 #'shr-tag-h4)
             ('h5 #'shr-tag-h5)
             ('h6 #'shr-tag-h6)
             ('div #'shr-tag-div)
             ('em #'shr-tag-em)
             ('code #'shr-tag-code)
             ('table #'shr-tag-table)
             ('a #'shr-tag-a)
             (_ (intern (concat "shr-tag-" (symbol-name (dom-tag dom))) obarray))))
          ;; Allow other packages to override (or provide) rendering
          ;; of elements.
          (external (cdr (assq (dom-tag dom) shr-external-rendering-functions)))
	  (style (dom-attr dom 'style))
	  (shr-stylesheet shr-stylesheet)
	  (shr-depth (1+ shr-depth))
	  (start (point)))
      ;; shr uses many frames per nested node.
      (if (and (> shr-depth (/ max-specpdl-size 15))
               (not (and shr-offer-extend-specpdl
                         (y-or-n-p "Too deeply nested to render properly; increase `max-specpdl-size'?")
                         (setq max-specpdl-size (* max-specpdl-size 2)))))
          (setq shr-warning
                "Not rendering the complete page because of too-deep nesting")
        (when style
	  (if (string-match-p "color\\|display\\|border-collapse" style)
	      (setq shr-stylesheet (nconc (shr-parse-style style)
					  shr-stylesheet))
	    (setq style nil)))
        ;; If we have a display:none, then just ignore this part of the DOM.
        (unless (or (equal (cdr (assq 'display shr-stylesheet)) "none")
                    (and shr-discard-aria-hidden
                         (equal (dom-attr dom 'aria-hidden) "true")))
          ;; We don't use shr-indirect-call here, since shr-descend is
          ;; the central bit of shr.el, and should be as fast as
          ;; possible.  Having one more level of indirection with its
          ;; negative effect on performance is deemed unjustified in
          ;; this case.
          (cond (external
                 (funcall external dom))
                ((fboundp function)
                 (funcall function dom))
                (t
                 (shr-generic dom)))
          (when-let ((id (dom-attr dom 'id)))
            (push (cons id (set-marker (make-marker) start)) shr--link-targets))
	  ;; If style is set, then this node has set the color.
	  (when style
	    (shr-colorize-region
	     start (point)
	     (cdr (assq 'color shr-stylesheet))
	     (cdr (assq 'background-color shr-stylesheet))))))))

  (advice-add #'shr-tag-table :around #'+shr-pretty-table)
  (advice-add #'shr-indirect-call :override #'+shr-indirect-call)
  (advice-add #'shr-descend :override #'+shr-descend)

  ;; (native-compile #'+shr-pretty-table)
  ;; (native-compile #'+shr-indirect-call)
  ;; (native-compile #'+shr-descend)
  )
