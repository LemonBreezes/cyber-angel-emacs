((magit-diff:magit-diff-mode "--ignore-all-space" "--no-ext-diff" "--stat")
 (magit-pull "--rebase" "--autostash")
 (magit-rebase "--autosquash" "--autostash")
 (magit-revert "--autostash"))
