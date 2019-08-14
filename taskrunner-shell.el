;; Files related to finding shell/powershell scripts and returning their names
;; for selection

(defun taskrunner--get-scripts (dir filetype prefix)
  "Return a list of all scripts matching regexp FILETYPE in directory DIR.
All names are prefixed with the string provided by PREFIX."
  (if (directory-name-p dir)
      (progn
        (let ((shell-scripts (directory-files dir nil filetype)))
          (map 'list (lambda (elem)
                       (concat prefix " " elem))
               shell-scripts)
          )
        )
    nil
    )
  )

(defun taskrunner--get-bash-scripts (dir)
  "Retrieve all bash scripts in directory DIR."
  (taskrunner--get-scripts dir "\\.sh$" "BASH"))

(defun taskrunner--get-powershell-scripts (dir)
  "Retrieve all powershell scripts in directory DIR."
  (taskrunner--get-scripts dir "\\.ps1$" "POWERSHELL"))

(defun taskrunner--get-zsh-scripts (dir)
  "Retrieve all zsh scripts in directory DIR"
  (taskrunner--get-scripts dir "\\.zsh$" "ZSH"))

(provide 'taskrunner-shell)
