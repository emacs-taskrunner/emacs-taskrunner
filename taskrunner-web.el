;;; taskrunner-web.el --- Provide functions to retrieve tasks for web build systems-*- lexical-binding: t; -*-
;; Copyright (C) 2019 Yavor Konstantinov

;;; Commentary:
;; This file adds support for taskrunners which are mainly used for
;; javascript/web projects.
;; Support included for:
;; - yarn/npm
;; - gulp
;; - grunt
;; - jake

;;; Code:

;;;; Required

(require 'json)
(require 'subr-x)
(require 'cl-lib)

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

(defconst taskrunner--js-gulp-tasks-command "gulp --tasks-simple"
  "Command used to retrieve the tasks for Gulp.")

;;;; Functions

(defun taskrunner--yarn-or-npm (DIR)
  "Detect if the current project in directory DIR is using `yarn' or `npm'.
If `taskrunner-preferred-js-package-manager' is not nil then its value is used.
Otherwise,  if `yarn.lock' is present then yarn is used.  If `package-lock.json'
is present then NPM is used.  If none are present and
`taskrunner-preferred-js-package-manager' is nil then the default is `npm'."
  (let ((dir-files  (directory-files DIR)))
    (cond
     ((member "yarn.lock" dir-files)
      "YARN")
     ((member "package-lock.json" dir-files)
      "NPM")
     ((stringp taskrunner-preferred-js-package-manager)
      (upcase taskrunner-preferred-js-package-manager))
     ;; Default if no files are present and the preferred package manager is
     ;; not set
     (t
      "NPM"))))

(defun taskrunner-get-package-json-tasks (DIR)
  "Retrieve all tasks in the scripts section of the package.json file in DIR.
This function returns a list of the form:
\(\PM TASK1\" \"PM TASK2\"...)
where PM is the package manager used(yarn or npm)."
  (let* ((package-path (expand-file-name "package.json" DIR))
         (package-json-scripts (alist-get 'scripts (json-read-file package-path)))
         (task-prefix (taskrunner--yarn-or-npm DIR))
         (tasks))
    (when package-json-scripts
      (setq tasks (cl-map 'list (lambda (elem)
                                  (concat task-prefix " " (symbol-name (car elem))))
                          package-json-scripts)))
    tasks))

(defun taskrunner-get-gulp-tasks (DIR)
  "Retrieve the gulp tasks for the project in directory DIR.
This function returns a list of the form:
\(\"GULP TASK1\" \"GULP TASK2\"...)"
  (let ((default-directory DIR))
    (butlast (cl-map 'list (lambda (elem)
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
      (cl-map 'list (lambda (elem)
                      (concat "GRUNT" " " (car (split-string (string-trim elem) " "))))
              (split-string (buffer-string) "\n")))))

(defun taskrunner-get-grunt-tasks (DIR)
  "Retrieve the grunt tasks for the project in directory DIR.
This function returns a list of the form:
\(\"GRUNT TASK1\" \"GRUNT TASK2\"...)"
  (let ((default-directory DIR)
        (tasks))
    (call-process "grunt" nil (taskrunner--make-task-buff-name "grunt") nil "--help")
    (with-temp-buffer
      (set-buffer (taskrunner--make-task-buff-name "grunt"))
      (setq tasks (taskrunner--get-grunt-tasks-from-buffer))
      (kill-current-buffer))
    (butlast tasks)))

(defun taskrunner--get-jake-tasks-from-buffer ()
  "Retrieve the rake tasks from the current buffer.
This function returns a list of the form:
\(\"JAKE TASK1\" \"JAKE TASK2\"...)"
  (goto-char (point-min))
  (taskrunner--narrow-to-line)
  (if (not (string-match-p ".+aborted\." (buffer-string)))
      (progn
        (widen)
        (goto-char (point-min))
        (let ((jake-tasks '()))
          (while (search-forward-regexp "^jake[[:space:]]+" nil t)
            (taskrunner--narrow-to-line)
            (push (concat "JAKE" " " (cadr (split-string (buffer-string) " " t))) jake-tasks)
            (widen))
          (kill-current-buffer)
          jake-tasks))
    nil))

(defun taskrunner-get-jake-tasks (DIR)
  "Retrieve the ant tasks for the project in directory DIR.
This function returns a list of the form:
\(\"JAKE TASK1\" \"JAKE TASK2\"...)"
  (let ((default-directory DIR))
    (call-process "jake"  nil (taskrunner--make-task-buff-name "jake")  nil "-T")
    (with-temp-buffer
      (set-buffer (taskrunner--make-task-buff-name "jake"))
      (taskrunner--get-jake-tasks-from-buffer))))

(provide 'taskrunner-web)
;;; taskrunner-web.el ends here
