;; Functions/warnings/varibles related to retrieving and working with
;; tasksrunners which are usually used in web projects.
;; Included here are:
;; - yarn/npm
;; - gulp
;; - grunt
;; - jake

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
  "Command used to retrieve the tasks for Gulp.")

(defconst taskrunner--jake-tasks-command "jake -T"
  "Command used to retrieve tasks from the Jake taskrunner.")

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
     ;; Default if no files are present and the preferred package manager is
     ;; not set
     (t
      "NPM"))
    )
  )

(defun taskrunner--js-get-package-tasks (dir)
  "Open and extract the tasks from package.json located in directory DIR.
This command returns a list containing the names of the tasks as strings."
  (let* ((package-path (concat dir "package.json"))
         (package-json-scripts (assoc 'scripts (json-read-file package-path)))
         (task-prefix (taskrunner--yarn-or-npm dir))
         (package-tasks '())
         )
    (message "package-json-scripts")
    (map 'list (lambda (elem)
                 (concat task-prefix " " (symbol-name (car elem))))
         (cdr package-json-scripts))
    )
  )

(defun taskrunner--js-get-gulp-tasks (dir)
  "Retrieve gulp tasks for the project in directory DIR."
  (let ((default-directory dir))
    (map 'list (lambda (elem)
                 (concat "GULP" " " elem))
         (split-string (shell-command-to-string taskrunner--js-gulp-tasks-command) "\n"))
    )
  )

(defun taskrunner--get-grunt-tasks-from-buffer ()
  "Retrieve the grunt tasks from the current buffer and return them as a list.
This function is not meant to be used externally.  Use `taskrunner--get-grunt-tasks'
instead."
  (goto-line 1)
  (let ((beg (re-search-forward "Available tasks.+\n" nil t))
        ;; The end of the region is simply an empty line
        (end (re-search-forward "^$" nil t))
        (tasks '()))
    (when beg
      (narrow-to-region beg end)
      (map 'list (lambda (elem)
                   (concat "GRUNT" " " (car (split-string (string-trim elem) " "))))
           (split-string (buffer-string) "\n")))
    )
  )

(defun taskrunner--get-grunt-tasks (dir)
  "Retrieve all grunt tasks from the project in directory DIR."
  (let ((default-directory dir)
        (buff (get-buffer-create "*taskrunner-grunt-tasks*"))
        (tasks))
    (call-process "grunt" nil "*taskrunner-grunt-tasks*" nil "--help")
    (with-temp-buffer
      (set-buffer buff)
      (setq tasks (taskrunner--get-grunt-tasks-from-buffer))
      (kill-current-buffer))
    tasks
    )
  )

(defun taskrunner--get-jake-tasks (dir)
  "Get all Jake tasks from the project in directory DIR."
  (let ((default-directory dir))
    (map 'list (lambda (elem)
                 (concat "JAKE" " " (cadr (split-string elem " "))))
         ;; Splitting the Jake tasks on \n leads to one element being empty so it must be removed
         (remove "" (split-string (shell-command-to-string taskrunner--jake-tasks-command) "\n")))
    )
  )

;; This will be used when I being work on making task retrieval async!
;; (defun taskrunner--grunt-process-sentinel (process event)
;;   "Sentinel used to retrieve the grunt tasks from an async process."
;;   (cond
;;    ((string-match-p "finished" event)
;;     (message" Done!")
;;     (with-temp-buffer
;;       (set-buffer (process-buffer process))
;;       (taskrunner--get-grunt-tasks-from-buffer))
;;     )
;;    (t
;;     (message "Failure to retrieve tasks from grunt! Error produced was %s" event)))
;;   )

(provide 'taskrunner-web)
