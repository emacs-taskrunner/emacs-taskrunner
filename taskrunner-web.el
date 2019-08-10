;; Functions/warnings/varibles related to retrieving and working with
;; tasksrunners which are usually used in web projects.
;; Included here are:
;; - yarn/npm
;; - gulp
;; - grunt
;; - TODO: jake
;; - TODO: webpack?

(defcustom taskrunner-preferred-js-package-manager nil
  "The preferred package manager to be used for tasks from package.json.
The package manager name should be a string which can be either `npm' or `yarn'.
If its value is nil then the package manager to be used is determined by the presence
of either a `yarn.lock' or `package-lock.json'.  If none are present and this
variable is nil then `npm' is used as default."
  :type 'string
  :options '("npm" "yarn")
  :group 'emacs-taskrunner)

(defconst taskrunner--js-gulp-tasks-command "gulp --tasks-simple"
  "Command used to retrieve the tasks for 'gulp' in json form.")

(defun taskrunner--yarn-or-npm (dir)
  "Detect if the current project in directory DIR is using `yarn' or `npm'.
If `taskrunner-preferred-js-package-manager' is not nil then its value is used.
Otherwise,  if `yarn.lock' is present then yarn is used.  If `package-lock.json'
is present then NPM is used.  If none are present and
`taskrunner-preferred-js-package-manager' is nil then the default is `npm'."
  (let ((dir-files  (directory-files dir)))
    (cond
     ((stringp taskrunner-preferred-js-package-manager)
      (upcase taskrunner-preferred-js-package-manager))
     ((member "yarn.lock" dir-files)
      "YARN")
     ((member "package-lock.json" dir-files)
      "NPM")
     (t
      "NPM"))
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

(provide 'taskrunner-web)
