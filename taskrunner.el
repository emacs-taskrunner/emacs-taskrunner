;;; taskrunner.el --- Retrieve build system/taskrunner tasks  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Yavor Konstantinov

;; Author: Yavor Konstantinov <ykonstantinov1 AT gmail DOT com>
;; URL: https://github.com/emacs-taskrunner/emacs-taskrunner
;; Version: 0.5
;; Package-Requires: ((emacs "24"))
;; Keywords: build-system taskrunner build task-runner tasks

;; This file is not part of GNU Emacs.

;;; Commentary:
;; This package aims to provide a library which can be used to retrieve tasks
;; from several build systems and task runners.  The output produced can then be
;; leveraged to create interactive user interfaces(helm/ivy for example) which
;; will let the user select a task to be ran.

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, then make sure to also install one of the
;; available frontends for this.  They are:
;; - ivy-taskrunner <- Uses ivy as a frontend
;; - helm-taskrunner <- Uses helm as a frontend
;; - ido-taskrunner <- Uses Ido as a frontend

;;;;; Manual

;; Install these required packages:

;; projectile

;; Then put this folder in your load-path, and put this in your init:

;; (require 'taskrunner)

;;;; Usage

;; Please see README for more details on the interfaces and customizable options
;; available.  This package is not meant to be used itself unless you are
;; developing a new frontend

;;;; Credits

;; This package would not have been possible without the following
;; packages:
;; grunt.el[1] which helped me retrieve the tasks from grunt
;; gulp-task-runner[2] which helped me retrieve the tasks from gulp
;; helm-make[3] which helped me figure out the regexps needed to retrieve
;;              makefile targets
;;
;;  [1] https://github.com/gempesaw/grunt.el
;;  [2] https://github.com/NicolasPetton/gulp-task-runner/tree/877990e956b1d71e2d9c7c3e5a129ad199b9debb
;;  [3] https://github.com/abo-abo/helm-make

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

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

;; Variables:

(defvar taskrunner-last-command-cache '()
  "A cache used to store the last executed command for each project.")

(defvar taskrunner-tasks-cache '()
  "A cache used to store the tasks retrieved.
It is an alist of the form (project-root . list-of-tasks)")

;; Functions:

(defun taskrunner--narrow-to-line ()
  "Narrow to the line entire line that the point lies on."
  (narrow-to-region (point-at-bol)
                    (point-at-eol))
  )

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


;; TODO: Add support for:
;; CMake
;; Parsers for them are already created

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

    (if (or (member "Jakefile.js" work-dir-files)
            (member "Jakefile" work-dir-files)
            (member "Jakefile.coffee" work-dir-files))
        (setq tasks (append tasks (taskrunner--get-jake-tasks dir))))

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

    (if (member "Cask" work-dir-files)
        (setq tasks (append tasks taskrunner--cast-targets)))

    ;; Cmake project. If it is an insource build then nothing is done
    ;; and the makefile contents are extracted in the statements below
    ;; otherwise, try to look for a build folder. If none is found
    (if (member "CMakeLists.txt" work-dir-files)
        )

    ;; Handle general makefiles at project root
    ;; TODO: If the makefile to be parsed is open in a buffer, it will be closed.
    ;;       Need to find a way to make it stay open if the buffer already exists
    ;; TODO: This is a bit repetitive/verbose. Could it be shortened?
    (cond
     ((member "Makefile" work-dir-files)
      (with-temp-buffer
        (find-file-read-only
         (concat dir "Makefile"))
        (setq tasks (append tasks (taskrunner-get-make-targets taskrunner-retrieve-all-make-targets)))
        )
      )
     ((member "makefile" work-dir-files)
      (with-temp-buffer
        (find-file-read-only
         (concat dir "makefile"))
        (setq tasks (append tasks (taskrunner-get-make-targets taskrunner-retrieve-all-make-targets)))
        )
      )
     ((member "GNUmakefile" work-dir-files)
      (with-temp-buffer
        (find-file-read-only
         (concat dir "GNumakefile"))
        (setq tasks (append tasks (taskrunner-get-make-targets taskrunner-retrieve-all-make-targets)))
        )
      )
     )

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
     ;; Downcase the first word which indicates the taskrunner and join
     ;; the rest of the arguments as a single string
     (concat taskrunner-program " " (mapconcat 'identity
                                               (cdr (split-string task " ")) " "))
     t)
    )
  )

;;;; Footer

(provide 'taskrunner)
;;; taskrunner.el ends here
