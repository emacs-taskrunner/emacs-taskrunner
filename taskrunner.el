;; Emacs taskrunner

(require 'projectile)
(require 'taskrunner-clang)
(require 'taskrunner-web)
(require 'taskrunner-java)
(require 'taskrunner-ruby)
(require 'taskrunner-static-targets)
(require 'taskrunner-mix)
(require 'taskrunner-leiningen)

(defgroup taskrunner nil
  "A taskrunner for emacs which covers several build systems and lets the user select and run targets interactively.")

(defvar taskrunner-last-command-cache '()
  "A cache used to store the last executed command for each project.")

(defvar taskrunner-tasks-cache '()
  "A cache used to store the tasks retrieved.
It is an alist of the form (project-root . list-of-tasks)")

(defun taskrunner-get-last-command-ran (&optional dir)
  "Retrieve the last task ran in currently visited project or in directory DIR.
If the project does not exist, return nil."
  (let ((proj-dir (if dir
                      (intern dir)
                    (intern (projectile-project-root)))))
    (alist-get proj-dir taskrunner-last-command-cache nil)
    )
  )

(defun taskrunner-set-last-command-ran (dir command)
  "Set the command COMMAND to be the last ran for project in directory DIR."
  ;; Remove the the previous command if it exists. Assoc-delete-all does not
  ;; throw an error so it is safe
  (let ((new-command-cache (assoc-delete-all (intern dir)
                                             taskrunner-last-command-cache)))
    ;; Reset the cache with new command added
    (setq taskrunner-last-command-cache (push (cons (intern dir) command) new-command-cache))
    )
  )

(defun taskrunner-delete-tasks-cache ()
  "Delete the entire task cache."
  (setq taskrunner-tasks-cache '()))

(defun taskrunner-delete-last-command-cache ()
  "Delete the entire last command cache."
  (setq taskrunner-last-command-cache '()))

(defun taskrunner-collect-tasks (dir)
  "Locate and extract all tasks for the project in directory DIR.
Returns a list containing all possible tasks.  Each element is of the form
'TASK-RUNNER-PROGRAM TASK-NAME'.  This is done for the purpose of working with
projects which might use multiple task runners.

Use this function if you want to retrieve the tasks from a project without
updating the cache."
  (let ((work-dir-files (directory-files dir))
        (tasks '()))
    (if (member "package.json" work-dir-files)
        (setq tasks (append tasks (taskrunner--js-get-package-tasks dir))))
    (if (or (member "gulpfile.js" work-dir-files)
            (member "Gulpfile.js" work-dir-files))
        (setq tasks (append tasks (taskrunner--js-get-gulp-tasks dir))))
    (if (or (member "Gruntfile.js" work-dir-files)
            (member "Gruntfile.coffee" work-dir-files))
        (setq tasks (append tasks (taskrunner--get-grunt-tasks dir))))
    (if (or (member "rakefile" work-dir-files)
            (member "Rakefile" work-dir-files)
            (member "rakefile.rb" work-dir-files)
            (member "Rakefile.rb" work-dir-files))
        (setq tasks (append tasks (taskrunner--get-rake-tasks dir))))
    (if (or (member "gradlew" work-dir-files)
            (member "gradlew.bat" work-dir-files)
            (member "build.gradle" work-dir-files))
        (setq tasks (append tasks (taskrunner--get-gradle-tasks dir))))
    (if (member "mix.exs" work-dir-files)
        (setq tasks (append tasks (taskrunner--start-elixir-task-process dir))))
    (if (member "project.clj" work-dir-files)
        (setq tasks (append tasks (taskrunner--start-leiningen-task-process dir))))
    (if (member "Cargo.toml" work-dir-files)
        (setq tasks (append tasks taskrunner--rust-targets)))
    (if (or (member "go.mod" work-dir-files)
            (member "go.sum" work-dir-files))
        (setq tasks (append tasks taskrunner--golang-targets)))
    tasks
    )
  )

(defun taskrunner-get-tasks-from-cache (&optional dir)
  "Retrieve the cached tasks from the directory DIR or the current project.
If the project does not have any tasks cached then collect all tasks and update
the cache.  If the tasks exist then simply return them.  The tasks returned are
in a list of strings.  Each string has the form TASKRUNNER-PROGRAM TASK-NAME."
  (let* ((proj-root (if dir
                        dir
                      (projectile-project-root)))
         (proj-tasks (alist-get (intern proj-root) taskrunner-tasks-cache)))
    ;; If the tasks do not exist, retrieve them first and then add to cache
    (if (not proj-tasks)
        (progn
          (setq proj-tasks (taskrunner-collect-tasks proj-root))
          ;; Add the project to the list. Use a symbol for faster comparison
          (push (cons (intern proj-root)  proj-tasks) taskrunner-tasks-cache)
          )
      (message "Did not retrieve tasks again" ))
    ;; Return the tasks
    proj-tasks
    )
  )

(defun taskrunner-project-cached-p (&optional dir)
  "Check if either the current project or the one in directory DIR are cached.
Return t or nil."
  (let ((proj-root (if dir
                       (intern dir)
                     (intern (projectile-project-root)))))
    ;; Cannot simply return the cache contents to the caller so use this to
    ;; make sure that either t or nil is returned.
    (if (assoc proj-root taskrunner-tasks-cache)
        t
      nil)
    )
  )

(defun taskrunner-refresh-cache (&optional dir)
  "Retrieve all tasks for project in DIR or the current project and set cache.
If there were tasks previously loaded then remove them, retrieve all tasks
again and set the corresponding project to the new list.  Return a list
containing the new tasks."
  (let* ((proj-root (if dir
                        dir
                      (projectile-project-root)))
         (proj-tasks (taskrunner-collect-tasks proj-root)))
    ;; remove old tasks if they exist
    (assoc-delete-all (intern proj-root) taskrunner-tasks-cache)
    ;; Add new tasks
    (push (cons (intern proj-root)  proj-tasks) taskrunner-tasks-cache)
    ;; Return the tasks
    proj-tasks
    )
  )

(defun taskrunner-run-task (task &optional dir)
  "Run command TASK in project root or directory DIR if provided."
  (let* ((default-directory (if dir
                                dir
                              (projectile-project-root)))
         (taskrunner-program (downcase (car (split-string task " "))))
         )
    (compile
     ;; Downcase the first word which indicates the taskrunner
     (concat taskrunner-program " " (mapconcat 'identity (cdr (split-string task " ")) " "))
     t)
    )
  )

(provide 'taskrunner)
;;; emacs-taskrunner.el ends here
