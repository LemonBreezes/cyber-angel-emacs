;;; private/unpackaged/autoload/org-return-dwim.el -*- lexical-binding: t; -*-

(defun cae-unpackaged-org-list-insert-item-after (pos struct prevs &optional checkbox after-bullet)
  (let* ((case-fold-search t)
	 ;; Get information about list: ITEM containing POS, position
	 ;; of point with regards to item start (BEFOREP), blank lines
	 ;; number separating items (BLANK-NB), if we're allowed to
	 ;; (SPLIT-LINE-P).
	 (item
	  (catch :exit
	    (let ((i nil))
	      (pcase-dolist (`(,start ,_ ,_ ,_ ,_ ,_ ,end) struct)
		(cond
		 ((> start pos) (throw :exit i))
		 ((< end pos) nil)	;skip sub-lists before point
		 (t (setq i start))))
	      ;; If no suitable item is found, insert a sibling of the
	      ;; last item in buffer.
	      (or i (caar (reverse struct))))))
	 (item-end (org-list-get-item-end item struct))
	 (item-end-no-blank (org-list-get-item-end-before-blank item struct))
         (beforep (progn
                    (goto-char item)
                    (looking-at org-list-full-item-re)
                    (and (<= pos
                             (cond
                              ((not (match-beginning 4)) (match-end 0))
                              ;; Ignore tag in a non-descriptive list.
                              ((save-match-data (string-match "[.)]" (match-string 1)))
                               (match-beginning 4))
                              (t (save-excursion
                                   (goto-char (match-end 4))
                                   (skip-chars-forward " \t")
                                   (point)))))
                         (not (eq pos (point-at-eol))))))
	 (split-line-p (org-get-alist-option org-M-RET-may-split-line 'item))
	 (blank-nb (org-list-separating-blank-lines-number pos struct prevs))
	 ;; Build the new item to be created.  Concatenate same bullet
	 ;; as item, checkbox, text AFTER-BULLET if provided, and text
	 ;; cut from point to end of item (TEXT-CUT) to form item's
	 ;; BODY.  TEXT-CUT depends on BEFOREP and SPLIT-LINE-P.  The
	 ;; difference of size between what was cut and what was
	 ;; inserted in buffer is stored in SIZE-OFFSET.
	 (ind (org-list-get-ind item struct))
	 (ind-size (if indent-tabs-mode
		       (+ (/ ind tab-width) (mod ind tab-width))
		     ind))
	 (bullet (org-list-bullet-string (org-list-get-bullet item struct)))
	 (box (and checkbox "[ ]"))
	 (text-cut
	  (and (not beforep)
	       split-line-p
	       (progn
		 (goto-char pos)
		 ;; If POS is greater than ITEM-END, then point is in
		 ;; some white lines after the end of the list.  Those
		 ;; must be removed, or they will be left, stacking up
		 ;; after the list.
		 (when (< item-end pos)
		   (delete-region (1- item-end) (point-at-eol)))
		 (skip-chars-backward " \r\t\n")
		 ;; Cut position is after any blank on the line.
		 (save-excursion
		   (skip-chars-forward " \t")
		   (setq pos (point)))
		 (delete-and-extract-region (point) item-end-no-blank))))
	 (body
	  (concat bullet
		  (and box (concat box " "))
		  after-bullet
		  (and text-cut
		       (if (string-match "\\`[ \t]+" text-cut)
			   (replace-match "" t t text-cut)
			 text-cut))))
	 (item-sep (make-string  (1+ blank-nb) ?\n))
	 (item-size (+ ind-size (length body) (length item-sep)))
	 (size-offset (- item-size (length text-cut))))
    ;; Insert effectively item into buffer.
    (goto-char item)
    (indent-to-column ind)
    (insert body item-sep)
    ;; Add new item to STRUCT.
    (dolist (e struct)
      (let ((p (car e)) (end (nth 6 e)))
	(cond
	 ;; Before inserted item, positions don't change but an item
	 ;; ending after insertion has its end shifted by SIZE-OFFSET.
	 ((< p item)
	  (when (> end item)
	    (setcar (nthcdr 6 e) (+ end size-offset))))
	 ;; Item where insertion happens may be split in two parts.
	 ;; In this case, move start by ITEM-SIZE and end by
	 ;; SIZE-OFFSET.
	 ((and (= p item) (not beforep) split-line-p)
	  (setcar e (+ p item-size))
	  (setcar (nthcdr 6 e) (+ end size-offset)))
	 ;; Items starting after modified item fall into two
	 ;; categories.
	 ;;
	 ;; If modified item was split, and current sub-item was
	 ;; located after split point, it was moved to the new item:
	 ;; the part between body start and split point (POS) was
	 ;; removed.  So we compute the length of that part and shift
	 ;; item's positions accordingly.
	 ;;
	 ;; Otherwise, the item was simply shifted by SIZE-OFFSET.
	 ((and split-line-p (not beforep) (>= p pos) (<= p item-end-no-blank))
	  (let ((offset (- pos item ind (length bullet) (length after-bullet))))
	    (setcar e (- p offset))
	    (setcar (nthcdr 6 e) (- end offset))))
	 (t
	  (setcar e (+ p size-offset))
	  (setcar (nthcdr 6 e) (+ end size-offset))))))
    (push (list item ind bullet nil box nil (+ item item-size)) struct)
    (setq struct (sort struct #'car-less-than-car))
    ;; If not BEFOREP, new item must appear after ITEM, so exchange
    ;; ITEM with the next item in list.  Position cursor after bullet,
    ;; counter, checkbox, and label.
    (if beforep
	(goto-char item)
      (setq struct (org-list-swap-items item (+ item item-size) struct))
      (goto-char (org-list-get-next-item
		  item struct (org-list-prevs-alist struct))))
    struct))

(defun cae-unpackaged-org-element-descendant-of (type element)
  "Return non-nil if ELEMENT is a descendant of TYPE.
TYPE should be an element type, like `item' or `paragraph'.
ELEMENT should be a list like that returned by `org-element-context'."
  ;; MAYBE: Use `org-element-lineage'.
  (when-let* ((parent (org-element-property :parent element)))
    (or (eq type (car parent))
        (cae-unpackaged-org-element-descendant-of type parent))))

;;;###autoload
(defun cae-unpackaged-org-return-dwim (&optional default)
  "A helpful replacement for `org-return'.  With prefix, call `org-return'.

On headings, move point to position after entry content.  In
lists, insert a new item or end the list, with checkbox if
appropriate.  In tables, insert a new row or end the table."
  ;; Inspired by John Kitchin: http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/
  (interactive "P")
  (save-restriction
    (widen)
    (if default
        (org-return)
      (cond
       ;; Act depending on context around point.

       ;; NOTE: I prefer RET to not follow links, but by uncommenting this block, links will be
       ;; followed.

       ;; ((eq 'link (car (org-element-context)))
       ;;  ;; Link: Open it.
       ;;  (org-open-at-point-global))

       ((org-at-heading-p)
        ;; Heading: Move to position after entry content.
        ;; NOTE: This is probably the most interesting feature of this function.
        (let ((heading-start (org-entry-beginning-position)))
          (goto-char (org-entry-end-position))
          (cond ((and (org-at-heading-p)
                      (= heading-start (org-entry-beginning-position)))
                 ;; Entry ends on its heading; add newline after
                 (end-of-line)
                 (insert "\n\n"))
                (t
                 ;; Entry ends after its heading; back up
                 (forward-line -1)
                 (end-of-line)
                 (cond ((org-at-heading-p)
                        ;; At the same heading
                        (forward-line)
                        (insert "\n")
                        (forward-line -1))
                       ((looking-back (rx (zero-or-more (syntax whitespace))
                                          "[[" (one-or-more (not (or "[" "]")))
                                          (zero-or-one "][" (one-or-more (not (or "[" "]"))))
                                          "]]" (zero-or-more (syntax whitespace))))
                        (goto-char (1- (match-beginning 0)))))
                 (while (not (looking-back
                              (rx (repeat 3 (seq (optional blank) "\n")))
                              (max (point-min) (- (point) 6)) t))
                   (insert "\n"))
                 (forward-line -1)))))

       ((org-at-item-checkbox-p)
        ;; Checkbox: Insert new item with checkbox.
        (org-insert-todo-heading nil))

       ((org-in-item-p)
        ;; Plain list.  Yes, this gets a little complicated...
        (let ((context (org-element-context)))
          (if (or (eq 'plain-list (car context)) ; First item in list
                  (and (eq 'item (car context))
                       (not (eq (org-element-property :contents-begin context)
                                (org-element-property :contents-end context))))
                  (cae-unpackaged-org-element-descendant-of 'item context)) ; Element in list item, e.g. a link
              ;; Non-empty item: Add new item.
              (cl-letf (((symbol-function #'org-list-insert-item)
                         (symbol-function #'cae-unpackaged-org-list-insert-item-after)))
                (org-insert-item))
            ;; Empty item: Close the list.
            ;; TODO: Do this with org functions rather than operating on the text. Can't seem to find the right function.
            (delete-region (line-beginning-position) (line-end-position))
            (insert "\n"))))

       ((when (fboundp 'org-inlinetask-in-task-p)
          (org-inlinetask-in-task-p))
        ;; Inline task: Don't insert a new heading.
        (org-return))

       ((org-at-table-p)
        (cond ((save-excursion
                 (beginning-of-line)
                 ;; See `org-table-next-field'.
                 (cl-loop with end = (line-end-position)
                          for cell = (org-element-table-cell-parser)
                          always (equal (org-element-property :contents-begin cell)
                                        (org-element-property :contents-end cell))
                          while (re-search-forward "|" end t)))
               ;; Empty row: end the table.
               (delete-region (line-beginning-position) (line-end-position))
               (org-return))
              (t
               ;; Non-empty row: call `org-return'.
               (org-return))))
       ((save-excursion
          (skip-chars-backward "\n")
          (looking-at "\\(\n+\\)#\\+end_"))
        (if (> (- (match-end 1) (match-beginning 1)) 1)
            (progn (goto-char (1- (match-end 1)))
                   (delete-region (1+ (match-beginning 1)) (1- (match-end 1))))
          (insert-char ?\n)))
       (t
        ;; All other cases: call `org-return'.
        (org-return))))))
