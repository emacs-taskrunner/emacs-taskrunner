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
;; And one or more of these:
;; - ivy-taskrunner <- Uses ivy as a frontend
;; - helm-taskrunner <- Uses helm as a frontend
;; - ido-taskrunner <- Uses Ido as a frontend

;; Then put this folder in your load-path, and put this in your init:

;; (require 'taskrunner)

;;;; Usage

;; Please see README for more details on the interfaces and customizable options
;; available.  This package is not meant to be used itself unless you are
;; developing a new frontend or would like to retrieve information about the
;; available tasks in a project/directory.

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

(defcustom taskrunner-no-previous-command-ran-warning
  "No previous command has been ran in this project!"
  "Warning used to indicate that there is no cached previously run command."
  :group 'taskrunner
  :type 'string)

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

(defun taskrunner-get-last-command-ran (&optional ROOT)
  "Retrieve the last task ran in currently visited project or in directory ROOT.
If the project does not exist, return nil."
  (let ((proj-dir (if ROOT
                      (intern ROOT)
                    (intern (projectile-project-root)))))
    (alist-get proj-dir taskrunner-last-command-cache nil)
    )
  )

(defun taskrunner-set-last-command-ran (ROOT DIR COMMAND)
  "Set the COMMAND ran in DIR to be the last command ran for project in ROOT."
  ;; Remove the the previous command if it exists. Assoc-delete-all does not
  ;; throw an error so it is safe
  (let ((new-command-cache (assoc-delete-all (intern ROOT)
                                             taskrunner-last-command-cache)))
    ;; Reset the cache with new command added
    (setq taskrunner-last-command-cache (push (list (intern ROOT) DIR COMMAND) new-command-cache))
    )
  )

(defun taskrunner-invalidate-tasks-cache ()
  "Delete the entire task cache."
  (setq taskrunner-tasks-cache '()))

(defun taskrunner-invalidate-last-command-cache ()
  "Delete the entire last command cache."
  (setq taskrunner-last-command-cache '()))


(defun taskrunner-collect-tasks (DIR)
  "Locate and extract all tasks for the project in directory DIR.
Returns a list containing all possible tasks.  Each element is of the form
'TASK-RUNNER-PROGRAM TASK-NAME'.  This is done for the purpose of working with
projects which might use multiple task runners.

Use this function if you want to retrieve the tasks from a project without
updating the cache."
  (let ((work-dir-files (directory-files DIR))
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

    (if (member "stack.yaml" work-dir-files)
        (setq tasks (append tasks taskrunner--stack-targets)))


    ;; Cmake project. If it is an insource build then nothing is done
    ;; and the makefile contents are extracted in the statements below
    ;; otherwise, try to look for a build folder. If none is found
    (if (member "CMakeLists.txt" work-dir-files)
        (cond
         ;; Check for a build folder
         ((or (member "build" work-dir-files)
              (member "Build" work-dir-files))
          )
         ;; Check if there are NO makefiles in the main folder.
         ;; If there are not then prompt user to select a build folder for the makefile
         ;; This build folder is then added to the CMake build folder cache
         ((not (or (member "Makefile" work-dir-files)
                   (member "makefile" work-dir-files)
                   (member "GNUmakefile" work-dir-files)))
          ;; Prompt and use that
          )
         )
      )

    ;; Handle general makefiles at project root
    ;; TODO: If the makefile to be parsed is open in a buffer, it will be closed.
    ;; Need to find a way to make it stay open if the buffer already exists
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

(defun taskrunner-get-tasks-from-cache (&optional DIR)
  "Retrieve the cached tasks from the directory DIR or the current project.
If the project does not have any tasks cached then collect all tasks and update
the cache.  If the tasks exist then simply return them.  The tasks returned are
in a list of strings.  Each string has the form TASKRUNNER-PROGRAM TASK-NAME."
  (let* ((proj-root (if DIR
                        DIR
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

(defun taskrunner-project-cached-p (&optional DIR)
  "Check if either the current project or the one in directory DIR are cached.
Return t or nil."
  (let ((proj-root (if DIR
                       (intern DIR)
                     (intern (projectile-project-root)))))
    ;; Cannot simply return the cache contents to the caller so use this to
    ;; make sure that either t or nil is returned.
    (if (assoc proj-root taskrunner-tasks-cache)
        t
      nil)
    )
  )

(defun taskrunner-refresh-cache (&optional DIR)
  "Retrieve all tasks for project in DIR or the current project and set cache.
If there were tasks previously loaded then remove them, retrieve all tasks
again and set the corresponding project to the new list.  Return a list
containing the new tasks."
  (let* ((proj-root (if DIR
                        DIR
                      (projectile-project-root)))
         (proj-tasks (taskrunner-collect-tasks proj-root)))
    ;; remove old tasks if they exist
    (assoc-delete-all (intern proj-root) taskrunner-tasks-cache)
    ;; Add new tasks
    (push (cons (intern proj-root) proj-tasks) taskrunner-tasks-cache)
    ;; Return the tasks
    proj-tasks
    )
  )

;; TODO: Make this use a custom buffer name
(defun taskrunner-run-task (TASK &optional DIR ASK)
  "Run command TASK in project root or directory DIR if provided.
If ASK is non-nil then ask the user to supply extra arguments to the task to
be ran."
  (let* ((default-directory (if DIR
                                DIR
                              (projectile-project-root)))
         (taskrunner-program (downcase (car (split-string TASK " "))))
         (command (cadr (split-string TASK " ")))
         )
    (when ASK
      (setq command
            (read-string (concat "Args/Flags to pass to " taskrunner-program ": ")
                         command)))
    ;; Extra handling for npm/yarn which require the run keyword
    (if (or (string-equal taskrunner-program "npm")
            (string-equal taskrunner-program "yarn"))
        (progn
          ;; (intern (projectile-project-root))
          (taskrunner-set-last-command-ran (projectile-project-root)
                                           default-directory
                                           (concat taskrunner-program " " command))
          (compile (concat taskrunner-program " " "run" " " command) t))
      (progn
        (taskrunner-set-last-command-ran  (projectile-project-root)
                                          default-directory
                                          (concat taskrunner-program " " command))
        (compile (concat taskrunner-program " " command) t))
      )
    )
  )

(defun taskrunner-rerun-last-task (DIR)
  "Rerun the last task which was ran for the project in DIR."
  (let ((last-ran-command (taskrunner-get-last-command-ran DIR)))
    (if last-ran-command
        (taskrunner-run-task (cadr last-ran-command) (car last-ran-command))
      (message taskrunner-no-previous-command-ran-warning))
    )
  )

;;;; Footer

(provide 'taskrunner)
;;; taskrunner.el ends here
