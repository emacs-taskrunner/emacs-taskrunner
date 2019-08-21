;;; taskrunner-shell.el --- Provide functions to retrieve shell script targets -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Yavor Konstantinov

;;;; Commentary:
;; Support for:
;; - Bash
;; - Powershell
;; - Zsh

;;;; Code:

;;;; Functions

(defun taskrunner--get-scripts (DIR FILETYPE-REGEXP PREFIX)
  "Return a list of all scripts matching regexp FILETYPE-REGEXP in DIR.
All names are prefixed with the string provided by PREFIX."
  (if (directory-name-p DIR)
      (progn
        (let ((shell-scripts (directory-files DIR nil FILETYPE-REGEXP)))
          (map 'list (lambda (elem)
                       (concat PREFIX " " elem))
               shell-scripts)))
    nil))

(defun taskrunner--get-bash-scripts (DIR)
  "Retrieve all bash scripts in directory DIR."
  (taskrunner--get-scripts DIR "\\.sh$" "BASH"))

(defun taskrunner--get-powershell-scripts (DIR)
  "Retrieve all powershell scripts in directory DIR."
  (taskrunner--get-scripts DIR "\\.ps1$" "POWERSHELL"))

(defun taskrunner--get-zsh-scripts (DIR)
  "Retrieve all zsh scripts in directory DIR."
  (taskrunner--get-scripts DIR "\\.zsh$" "ZSH"))

(provide 'taskrunner-shell)
;;; taskrunner-shell.el ends here
