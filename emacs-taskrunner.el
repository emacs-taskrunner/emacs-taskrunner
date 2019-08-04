;; Emacs taskrunner

(require 'projectile)

(defconst taskrunner--js-gulp-tasks-command "gulp --tasks-simple"
  "Command used to retrieve the tasks for 'gulp' in json form.")

(defconst taskrunner--rake-tasks-command '("rake" "-T")
  "Command used to retrieve the tasks from rake.")

(defconst taskrunner--make-phony-regexp "\.PHONY[[:space:]]+:[[:space:]]+"
  "Regular expression used to locate all PHONY targets in makefile.")

(defvar taskrunner-tasks-cache '()
  "A cache used to store the tasks retrieved.
It is an alist of the form (project-root . list-of-tasks)")

(defun taskrunner--yarn-or-npm (dir)
  "Attempt to decide if the current project in directory DIR uses yarn or npm.
If the file 'yarn.lock' is not found then the default is 'npm'."
  (let ((dir-files  (directory-files dir)))
    (if (member "yarn.lock" dir-files)
        "YARN"
      "NPM")
    )
  )

(defun taskrunner--js-get-package-tasks (dir)
  "Open and extract the tasks from package.json located in directory DIR.
This command returns a list containing the names of the tasks as strings."
  (let* ((package-path (concat dir "package.json"))
         (package-json-contents (assoc 'scripts (json-read-file package-path)))
         (task-prefix (taskrunner--yarn-or-npm dir))
         (package-tasks '())
         )
    (dolist (el (cdr package-json-contents))
      (setq package-tasks (push (concat task-prefix " " (symbol-name (car el))) package-tasks)))
    package-tasks
    )
  )

(defun taskrunner--load-tasks-in-cache (dir)
  "Locate all task files and load them into the cache for the project."
  (interactive)
  (let ((work-dir-files (directory-files dir)))
    (if (member "package.json" work-dir-files)
        (taskrunner--js-get-package-tasks dir)
      )
    (if (or (member "gulpfile.js" work-dir-files) (member "Gulpfile.js" work-dir-files))
        (taskrunner--js-get-gulp-tasks dir)
      )
    )
  )

(defun taskrunner--js-get-gulp-tasks (dir)
  "Retrieve tasks for gulp if the file is found."
  (interactive)
  (let ((default-directory dir))
    (split-string (shell-command-to-string taskrunner--js-gulp-tasks-command) "\n")
    )
  )

(defun taskrunner--ruby-get-rake-tasks (dir)
  "Retrieve tasks from the rake build system for the project in directory DIR."
  (let ((default-directory dir)
        (task-list '()))
    (dolist  (el (split-string (shell-command-to-string "rake -T") "\n"))
      (push (concat "RAKE" " " (cadr (split-string el " "))) task-list)
      )
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

(defun taskrunner--get-grunt-tasks-from-buffer ()
  "Retrieve the tasks from the grunt taskrunner. It uses grunt --help to
retrieve them."
  (goto-line 1)
  (let ((beg (re-search-forward "Available tasks.+\n" nil t))
        ;; The end of the region is simply an empty line
        (end (re-search-forward "^$" nil t))
        (splits))
    (when beg
      (narrow-to-region beg end)
      (setq splits (split-string (buffer-string) "\n"))
      (widen))
    (dolist (el splits)
      (message "Called from splits")
      (message "%s" (car (split-string (string-trim el) " "))))
    )
  )

(defun taskrunner--grunt-process-sentinel (process event)
  "Sentinel used to retrieve the grunt tasks from an async process."
  (cond
   ((string-match-p "finished" event)
    (message" Done!")
    (with-temp-buffer
      (set-buffer (process-buffer process))
      (taskrunner--get-grunt-tasks-from-buffer))
    )
   (t
    (message "Failure to retrieve tasks from grunt! Error produced was %s" event)))
  )

(defun taskrunner--create-process (dir commands run-in-compile &optional
                                       buff-name sentinel process-name)
  "Run the command COMMAND in the directory DIR. If RUN-IN-COMPILE is t
then run the command in compilation mode, otherwise run an async process."
  (let ((default-directory dir))
    (if (not run-in-compile)
        (progn
          (make-process
           :name process-name
           :buffer buff-name
           :command commands
           :sentinel sentinel))
      )
    )
  )

(provide 'emacs-taskrunner)
;;; emacs-taskrunner.el ends here
