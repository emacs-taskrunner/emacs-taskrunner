;;; taskrunner-ruby.el --- Provide functions to retrieve ruby tasks via Rake -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Yavor Konstantinov

;;;; Commentary:
;; Provide support for Rake(Ruby)

;;;; Code:
(require 'cl-lib)

;;;; Variables
(defconst taskrunner--rake-tasks-command '("rake" "-AT")
  "Command used to retrieve the tasks from rake.")

;;;; Functions
(defun taskrunner-get-rake-tasks (DIR)
  "Retrieve the rake tasks for the project in directory DIR.
This function returns a list of the form:
\(\"RAKE TASK1\" \"RAKE TASK2\"...)"
  (let ((default-directory DIR))
    (cl-map 'list (lambda (elem)
                    (concat "RAKE" " " (cadr (split-string elem " "))))
            (split-string (shell-command-to-string taskrunner--rake-tasks-command) "\n"))))

(provide 'taskrunner-ruby)
;;; taskrunner-ruby.el ends here
