;; Emacs taskrunner

(require 'projectile)
(require 'taskrunner-clang)

(defconst taskrunner--js-gulp-tasks-command "gulp --tasks-simple"
  "Command used to retrieve the tasks for 'gulp' in json form.")

(defconst taskrunner--rake-tasks-command '("rake" "-AT")
  "Command used to retrieve the tasks from rake.")

(defvar taskrunner-tasks-cache '()
  "A cache used to store the tasks retrieved.
It is an alist of the form (project-root . list-of-tasks)")


(defun taskrunner--yarn-or-npm (dir)
  "Attempt to decide if the current project uses in directory DIR yarn or npm.
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

(defun taskrunner--js-get-gulp-tasks (dir)
  "Retrieve tasks for gulp if there is a gulp taskfile in directory DIR."
  (interactive)
  (let ((default-directory dir))
    (map 'list (lambda (elem)
                 (concat "GULP" " " elem)) (split-string (shell-command-to-string taskrunner--js-gulp-tasks-command) "\n"))
    )
  )

(defun taskrunner--gradle-get-heading-tasks (heading)
  "Retrieve the gradle tasks below the heading HEADING and return as list."
  (goto-line 1)
  (narrow-to-region (re-search-forward heading nil t)
                    (progn 
                      (re-search-forward "^$" nil t)
                      (previous-line 1)
                      (line-end-position)))

  (map 'list (lambda (elem)
               (message (concat "GRADLE" " " (car (split-string elem " ")))))
       ;; (message "%s" (split-string elem " ")))
       (split-string (buffer-string) "\n"))
  (widen))

(defun taskrunner--gradle-tasks (dir)
  "Retrieve the gradle tasks in for the project in directory DIR."
  (let ((default-directory dir)
        (buff (get-buffer-create "*taskrunner-gradle-tasks*"))
        )
    (call-process "gradle"  nil "*taskrunner-gradle-tasks*"  nil "tasks" "--all")
    (with-temp-buffer
      (set-buffer buff)
      (message "Get build")
      (taskrunner--gradle-get-heading-tasks "Build tasks\n-+\n")
      (taskrunner--gradle-get-heading-tasks "Help tasks\n-+\n")
      (taskrunner--gradle-get-heading-tasks "Verification tasks\n-+\n")
      (taskrunner--gradle-get-heading-tasks "Build Setup tasks\n-+\n")
      (taskrunner--gradle-get-heading-tasks "Documentation tasks\n-+\n")
      (taskrunner--gradle-get-heading-tasks "Other tasks\n-+\n")
      (kill-buffer buff)
      )
    )
  )

(defun taskrunner--rake-tasks (dir)
  "Retrieve tasks from the rake build system for the project in directory DIR."
  (let ((default-directory dir)
        (task-list '()))
    (map 'list (lambda (elem)
                 (concat "RAKE" " " (cadr (split-string elem " "))))
         (split-string (shell-command-to-string taskrunner--rake-tasks-command) "\n"))
    )
  )

(defun taskrunner--get-tasks (dir)
  "Locate and extract all tasks for the project in directory DIR.
Returns a list containing all possible tasks.  Each element is of the form
'TASK-RUNNER-PROGRAM TASK-NAME'.  This is done for the purpose of working with
projects which might use multiple task runners."
  (let ((work-dir-files (directory-files dir))
        (tasks '()))
    (if (member "package.json" work-dir-files)
        (append tasks (taskrunner--js-get-package-tasks dir))
      )
    (if (or (member "gulpfile.js" work-dir-files)
            (member "Gulpfile.js" work-dir-files))
        (append tasks (taskrunner--js-get-gulp-tasks dir))
      )
    (if (or (member "rakefile" work-dir-files)
            (member "Rakefile" work-dir-files)
            (member "rakefile.rb" work-dir-files)
            (member "Rakefile.rb" work-dir-files))
        (append tasks (taskrunner--ruby-get-rake-tasks dir))
      )

    tasks
    )
  )

(defun taskrunner-refresh-cache ()
  "Retrieve all tasks for the current project and load them in the cache.
If there were tasks previously loaded then remove them and retrieve all tasks
again."
  (interactive)
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
