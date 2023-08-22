;;; autoload/cae-corfu.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-corfu-enable-in-minibuffer-h ()
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (and (minibuffer-prompt)
                   (cl-member (minibuffer-prompt)
                              '("I-search: "
                                "Query replace "
                                "Align regexp"
                                "Expansion for ")
                              :test #'string-match-p))
              (memq this-command '(evil-ex
                                   evil-ex-search-forward
                                   evil-ex-search-backward))
              (and (featurep 'helm-core)
                   (helm--alive-p))
              (corfu-mode +1))))

;;;###autoload
(defun cae-corfu-visible-p ()
  (or (and (frame-live-p corfu--frame)
           (frame-visible-p corfu--frame))
      (and (featurep 'corfu-terminal)
           (popon-live-p corfu-terminal--popon))))
