;; Emacs taskrunner

(require 'projectile)

(defconst taskrunner--js-gulp-tasks-command "gulp --tasks-json"
  "Command used to retrieve the tasks for 'gulp' in json form.")

(defun taskrunner--js-get-package-tasks ()
  "Open and extract the tasks from package.json.
This command returns a list containing the names of the tasks as strings."
  (interactive)
  (let* ((package-path (concat (projectile-project-root) "package.json"))
         (package-json-contents (assoc 'scripts (json-read-file package-path)))
         (package-tasks '())
         )
    (dolist (el (cdr package-json-contents))
      (setq package-tasks (push (symbol-name (car el)) package-tasks)))
    (message "%s" package-tasks)
    )
  )

(defun taskrunner--js-get-gulp-tasks ()
  "Retrieve tasks for gulp if the file is found.
If no file exists, return an empty list."
  (interactive)
  (let ((default-directory (projectile-project-root))
        (gulp-json-tasks (cdr (cadr (json-read-from-string (shell-command-to-string taskrunner--js-gulp-tasks-command)))))
        )
    (message "%s" gulp-json-tasks)
    )
  )
