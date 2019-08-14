;; Functions related to ruby's rake
(defconst taskrunner--rake-tasks-command '("rake" "-AT")
  "Command used to retrieve the tasks from rake.")

(defun taskrunner--rake-tasks (dir)
  "Retrieve tasks from the rake build system for the project in directory DIR."
  (let ((default-directory dir)
        (task-list '()))
    (map 'list (lambda (elem)
                 (concat "RAKE" " " (cadr (split-string elem " "))))
         (split-string (shell-command-to-string taskrunner--rake-tasks-command) "\n"))
    )
  )

(provide 'taskrunner-ruby)
