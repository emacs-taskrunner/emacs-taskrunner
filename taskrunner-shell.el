;; Files related to finding shell/powershell scripts and returning their names
;; for selection

(defun taskrunner--get-shell-scripts (dir)
  "Return the names of all shell scripts in directory DIR.")

(defun taskrunner--get-powershell-scripts (dir)
  "Return the names of all powershell scripts in directory DIR.")

(provide 'taskrunner-shell)
