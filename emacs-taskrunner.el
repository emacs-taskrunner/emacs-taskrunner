;; Emacs taskrunner

(require 'projectile)

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
