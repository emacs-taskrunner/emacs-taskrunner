;;; taskrunner-shell.el --- Provide functions to retrieve shell script targets -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Yavor Konstantinov

;; TODO: Find a way to reliably run tasks in the shell where they must
;; run(powershell scripts in powershell, bash in bash...)
;;;; Commentary:
;; Support for:
;; - Bash
;; - Powershell
;; - Zsh

;;;; Code:
(require 'cl-lib)

;;;; Functions

(defun taskrunner--get-scripts (DIR FILETYPE-REGEXP PREFIX)
  "Return a list of all scripts matching regexp FILETYPE-REGEXP in DIR.
All names are prefixed with the string provided by PREFIX."
  (if (directory-name-p DIR)
      (progn
        (let ((shell-scripts (directory-files DIR nil FILETYPE-REGEXP)))
          (cl-map 'list (lambda (elem)
                          (concat PREFIX " " elem))
                  shell-scripts)))))

(defun taskrunner-get-bash-scripts (DIR)
  "Retrieve the rake tasks for the project in directory DIR.
This function returns a list of the form:
\(\"BASH TASK1\" \"BASH TASK2\"...)"
  (taskrunner--get-scripts DIR "\\.sh$" "BASH"))

(defun taskrunner-get-powershell-scripts (DIR)
  "Retrieve the rake tasks for the project in directory DIR.
This function returns a list of the form:
\(\"POWERSHELL TASK1\" \"POWERSHELL TASK2\"...)"
  (taskrunner--get-scripts DIR "\\.ps1$" "POWERSHELL"))

(defun taskrunner-get-zsh-scripts (DIR)
  "Retrieve the rake tasks for the project in directory DIR.
This function returns a list of the form:
\(\"ZSH TASK1\" \"ZSH TASK2\"...)"
  (taskrunner--get-scripts DIR "\\.zsh$" "ZSH"))

(defun taskrunner-get-fish-scripts (DIR)
  "Retrieve the rake tasks for the project in directory DIR.
This function returns a list of the form:
\(\"FISH TASK1\" \"FISH TASK2\"...)"
  (taskrunner--get-scripts DIR "\\.fish$" "FISH"))

(provide 'taskrunner-shell)
;;; taskrunner-shell.el ends here
