;;; taskrunner-web.el --- Provide functions to retrieve tasks for web build systems-*- lexical-binding: t; -*-
;; Copyright (C) 2019 Yavor Konstantinov

;;; Commentary:
;; Support included for:
;; - yarn/npm
;; - gulp
;; - grunt
;; - jake

;;; Code:

;;;; Required

(require 'json)
(require 'subr-x)

;;;; Variables

(defcustom taskrunner-preferred-js-package-manager nil
  "The preferred package manager to be used for tasks from package.json.
The package manager name should be a string which can be either `npm' or `yarn'.
If its value is nil then the package manager to be used is determined by the presence
of either a `yarn.lock' or `package-lock.json'.  If none are present and this
variable is nil then `npm' is used as default."
  :type 'string
  :options '("npm" "yarn")
  :group 'taskrunner)

(defcustom taskrunner-grunt-buffer-name "*taskrunner-grunt-tasks*"
  "Buffer name used to retrieve grunt tasks."
  :type 'string
  :group 'taskrunner)

(defconst taskrunner--js-gulp-tasks-command "gulp --tasks-simple"
  "Command used to retrieve the tasks for Gulp.")

(defconst taskrunner--jake-tasks-command "jake -T"
  "Command used to retrieve tasks from the Jake taskrunner.")

;;;; Functions

(defun taskrunner--yarn-or-npm (DIR)
  "Detect if the current project in directory DIR is using `yarn' or `npm'.
If `taskrunner-preferred-js-package-manager' is not nil then its value is used.
Otherwise,  if `yarn.lock' is present then yarn is used.  If `package-lock.json'
is present then NPM is used.  If none are present and
`taskrunner-preferred-js-package-manager' is nil then the default is `npm'."
  (let ((dir-files  (directory-files DIR)))
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
      "NPM"))))

(defun taskrunner-get-package-json-tasks (DIR)
  "Retrieve all tasks in the scripts section of the package.json file in DIR.
This function returns a list of the form:
\(\PM TASK1\" \"PM TASK2\"...)
where PM is the package manager used."
  (let* ((package-path (expand-file-name "package.json" DIR))
         (package-json-scripts (assoc 'scripts (json-read-file package-path)))
         (task-prefix (taskrunner--yarn-or-npm DIR)))
    (map 'list (lambda (elem)
                 (concat task-prefix " " (symbol-name (car elem))))
         (cdr package-json-scripts))
    )
  )

(defun taskrunner-get-gulp-tasks (DIR)
  "Retrieve the gulp tasks for the project in directory DIR.
This function returns a list of the form:
\(\"GULP TASK1\" \"GULP TASK2\"...)"
  (let ((default-directory DIR))
    (butlast (map 'list (lambda (elem)
                          (concat "GULP" " " elem))
                  (split-string (shell-command-to-string taskrunner--js-gulp-tasks-command) "\n")))))

(defun taskrunner--get-grunt-tasks-from-buffer ()
  "Retrieve the grunt tasks from the current buffer and return them as a list.
This function is not meant to be used externally.  Use
`taskrunner--get-grunt-tasks' instead."
  (goto-char (point-min))
  (let ((beg (re-search-forward "Available tasks.+\n" nil t))
        ;; The end of the region is simply an empty line
        (end (re-search-forward "^$" nil t)))
    (when beg
      (narrow-to-region beg end)
      (map 'list (lambda (elem)
                   (concat "GRUNT" " " (car (split-string (string-trim elem) " "))))
           (split-string (buffer-string) "\n")))
    )
  )

(defun taskrunner-get-grunt-tasks (DIR)
  "Retrieve the grunt tasks for the project in directory DIR.
This function returns a list of the form:
\(\"GRUNT TASK1\" \"GRUNT TASK2\"...)"
  (let ((default-directory DIR)
        (buff (get-buffer-create taskrunner-grunt-buffer-name))
        (tasks))
    (call-process "grunt" nil taskrunner-grunt-buffer-name nil "--help")
    (with-temp-buffer
      (set-buffer buff)
      (setq tasks (taskrunner--get-grunt-tasks-from-buffer))
      (kill-current-buffer))
    (butlast tasks)))

(defun taskrunner-get-jake-tasks (DIR)
  "Retrieve the jake tasks for the project in directory DIR.
This function returns a list of the form:
\(\"JAKE TASK1\" \"JAKE TASK2\"...)"
  (let ((default-directory DIR))
    (map 'list (lambda (elem)
                 (concat "JAKE" " " (cadr (split-string elem " "))))
         ;; Splitting the Jake tasks on \n leads to one element being empty so it must be removed
         (remove "" (split-string (shell-command-to-string taskrunner--jake-tasks-command) "\n")))))

(provide 'taskrunner-web)
;;; taskrunner-web.el ends here
