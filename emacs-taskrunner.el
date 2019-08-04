;; Emacs taskrunner

(require 'projectile)

(defconst taskrunner--js-gulp-tasks-command "gulp --tasks-json"
  "Command used to retrieve the tasks for 'gulp' in json form.")

(defconst taskrunner--rake-tasks-command "rake --tasks")

(defconst taskrunner--make-phony-regexp "\.PHONY[[:space:]]+:[[:space:]]+"
  "Regular expression used to locate all PHONY targets in makefile.")

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

(defun taskrunner--js-get-gulp-tasks (&optional path)
  "Retrieve tasks for gulp if the file is found.
If no file exists, return an empty list."
  (interactive)
  (let ((default-directory
          (or
           path
           (projectile-project-root)))
        (gulp-json-tasks (cdr (cadr (json-read-from-string (shell-command-to-string taskrunner--js-gulp-tasks-command)))))
        )
    (message "%s" path)
    (message "%s" gulp-json-tasks)
    )
  )

(defun taskrunner--ruby-get-rake-tasks (&optional path)
  (interactive)
  (let ((default-directory (or
                            path
                            (projectile-project-root)))
        (buff (get-buffer-create "*taskrunner-rake-tasks*"))
        (rake-tasks '()))
    (message (concat "Default dir: " default-directory))
    ;; (shell-command "rake --tasks" buff)
    (call-process "rake" nil buff nil "--tasks")
    (with-temp-buffer
      (set-buffer buff)
      (goto-line 1)
      (while (re-search-forward "rake " nil t)
        (setq rake-tasks (push (symbol-name (form-at-point)) rake-tasks))))
    ;; (kill-current-buffer))
    rake-tasks
    )
  )

(defun taskrunner--make-get-phony-tasks (&optional path)
  "Retrieve all 'PHONY' tasks from a makefile. If PATH is nil then project root
is used."
  (interactive)
  (let ((make-path (or
                    path
                    (concat (projectile-project-root "Makefile"))))
        (buff (get-buffer-create "*taskrunner-makefile*"))
        (phony-tasks '())
        )
    (with-temp-buffer
      (set-buffer buff)
      (goto-line 1)
      (insert-file-contents make-path)
      (while (re-search-forward taskrunner--make-phony-regexp nil t)
        (setq phony-tasks (push (symbol-name (symbol-at-point)) phony-tasks)))
      (kill-current-buffer))
    phony-tasks
    )
  )

(defun taskrunner--get-grunt-tasks ()
  "Retrieve the tasks from the grunt taskrunner. It uses grunt --help to
retrieve them."
  (let ((beg (re-search-forward "Available tasks.+\n" nil t))
        ;; The end of the region is simply an empty line
        (end (re-search-forward "^$" nil t))
        (splits))
    (when beg
      (narrow-to-region beg end)
      (setq splits (split-string (buffer-string) "\n"))
      (widen))
    (dolist (el splits)
      (message "%s" (car (split-string (string-trim el) " "))))
    )
  )
