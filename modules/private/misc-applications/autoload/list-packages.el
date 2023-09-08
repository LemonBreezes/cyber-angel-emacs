;;; private/misc-applications/autoload/list-packages.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+list-packages-hydra/body "private/misc-applications/autoload/list-packages" nil t)
(eval ;; TODO
 `(defhydra +list-packages-hydra (:color pink :hint nil)
    ("<f6>" nil "Exit" :exit t)
    ("q" quit-window nil :exit t)
    ("S" tabulated-list-sort "Sort")
    ("?" package-menu-describe-package "Describe package")
    ("H ")
    )
      t)

(defhydra +list-packages-hydra (:color pink :hint nil)
  "
  _?_ : describe-package  _H_ : hide-package    _S_ : sort
  _U_ : mark-upgrades     _b_ : report-bug      _d_ : mark-delete
  _g_ : revert-buffer     _h_ : quick-help      _i_ : mark-install
  _r_ : revert-buffer     _u_ : unmark          _w_ : browse-url
  _x_ : execute

  / / : clear-filter      / N : filter-by-name-or-description
  / a : filter-by-archive / d : filter-by-description
  / k : filter-by-keyword / m : filter-marked
  / n : filter-by-name    / s : filter-by-status
  / u : filter-upgradable / v : filter-by-version
  "
  ("<f6>" nil "Exit" :exit t)
  ("q" quit-window :exit t)
  ("?" package-menu-describe-package :column "Display")
  ("H" package-menu-hide-package :column "Display")
  ("S" tabulated-list-sort :column "Display")
  ("U" package-menu-mark-upgrades :column "Mark")
  ("b" package-report-bug :column "Act")
  ("d" package-menu-mark-delete :column "Mark")
  ("g" revert-buffer :column "Display")
  ("h" package-menu-quick-help :column :column "Display")
  ("i" package-menu-mark-install "Install" :column "Act")
  ("r" revert-buffer "Revert buffer" :column "Display")
  ("u" package-menu-mark-unmark "Unmark" :column "Mark")
  ("w" package-browse-url "Browse URL" :column "Act")
  ("x" package-menu-execute "Execute marks" :column "Act")
  ("/ /" package-menu-clear-filter "Clear filter" :column "Filter")
  ("/ N" package-menu-filter-by-name-or-description "Filter by name or description" :column "Filter")
  ("/ a" package-menu-filter-by-archive "Filter by archive" :column "Filter")
  ("/ d" package-menu-filter-by-description "Filter by description" :column "Filter")
  ("/ k" package-menu-filter-by-keyword "Filter by keyword" :column "Filter")
  ("/ m" package-menu-filter-marked "Filter by marked" :column "Filter")
  ("/ n" package-menu-filter-by-name "Filter by name" :column "Filter")
  ("/ s" package-menu-filter-by-status "Filter by status" :column "Filter")
  ("/ u" package-menu-filter-upgradable "Filter by upgradable" :column "Filter")
  ("/ v" package-menu-filter-by-version "Filter by version" :column "Filter"))
