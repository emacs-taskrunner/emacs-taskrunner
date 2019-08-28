;;; taskrunner.el --- Retrieve build system/taskrunner tasks  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Yavor Konstantinov

;; Author: Yavor Konstantinov <ykonstantinov1 AT gmail DOT com>
;; URL: https://github.com/emacs-taskrunner/emacs-taskrunner
;; Version: 0.6
;; Package-Requires: ((emacs "25.1"))
;; Keywords: build-system taskrunner build task-runner tasks convenience

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
;; async
;; cl-lib
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
(require 'async)
(require 'cl-lib)
(require 'taskrunner-clang)
(require 'taskrunner-web)
(require 'taskrunner-java)
(require 'taskrunner-ruby)
(require 'taskrunner-static-targets)
(require 'taskrunner-mix)
(require 'taskrunner-leiningen)
(require 'taskrunner-general)

(defgroup taskrunner nil
  "A taskrunner for emacs which covers several build systems and lets the user select and run targets interactively."
  :prefix "taskrunner-"
  :group 'convenience)

;; Variables:

(defcustom taskrunner-no-previous-command-ran-warning
  "No previous command has been ran in this project!"
  "Warning used to indicate that there is no cached previously run command."
  :group 'taskrunner
  :type 'string)

(defvar taskrunner--cache-file-read nil
  "Indicates whether or not the cache file has been read.
Do not edit unless you want to reread the cache.")

(defvar taskrunner--async-process-dir nil
  "Used to hold the working directory argument for usage in the async package.
Do not edit this manually!")

(defconst taskrunner--cache-file-header-warning
  ";;This file is generated automatically. Please do not edit by hand!\n"
  "Warning inserted at the top of the tasks cache file to indicate not to edit it.")

(defconst taskrunner--buffer-name-regexp
  "\*taskrunner-.+*"
  "Regexp used to find all buffers running tasks.")

;; Caches used to store data related to tasks/commands

(defvar taskrunner-last-command-cache (make-hash-table :test 'eq :weakness nil)
  "A cache used to store the last executed command for each project.
It is a hashmap where each member is of the form (project-root command)")

(defvar taskrunner-build-cache (make-hash-table :test 'eq :weakness nil)
  "A cache used to store project build folders for retrieval.
It is a hashmap where each member is of the form (project-root build-folder)")

(defvar taskrunner-tasks-cache (make-hash-table :test 'eq :weakness nil)
  "A cache used to store the tasks retrieved.
It is a hashmap where each member is of the form (project-root list-of-tasks)")

(defvar taskrunner-command-history-cache (make-hash-table :test 'eq :weakness nil)
  "A cache used to store the command history for a project.
It is a hashmap where each member is of the form (project-root list-of-commands)")

(defvar taskrunner-custom-command-cache (make-hash-table :test 'eq :weakness nil)
  "A cache used to store custom commands for each project.
It is a hashmap where each member is of the form (project-root list-of-commands)")

(defvar taskrunner-command-history-size 10
  "The maximum number of commands stored in the command cache for each project.")

;; Functions:

;; Helper/Utility Functions
(defun taskrunner--narrow-to-line ()
  "Narrow to the line entire line that the point lies on."
  (narrow-to-region (point-at-bol)
                    (point-at-eol)))

(defmacro taskrunner--make-task-buff-name (TASKRUNNER)
  "Create a buffer name used to retrieve the tasks for TASKRUNNER."
  `(concat "*taskrunner-" ,TASKRUNNER "-tasks-buffer*"))

;; Getters and setters for caches
(defun taskrunner-get-last-command-ran (&optional DIR)
  "Retrieve the last command ran for the project.
If DIR is non-nil then return the command for for that directory.  Otherwise,
use the project root for the currently visited buffer."
  (let ((proj-dir (if DIR
                      DIR
                    (projectile-project-root))))
    (gethash (intern proj-dir) taskrunner-last-command-cache)))

(defun taskrunner-set-last-command-ran (ROOT DIR COMMAND)
  "Set the COMMAND ran in DIR to be the last command ran for project in ROOT."
  (puthash (intern ROOT) (list DIR COMMAND) taskrunner-last-command-cache))

(defun taskrunner-add-to-tasks-cache (ROOT TASKS)
  "Add TASKS for project in directory ROOT to the tasks cache.
TASKS should a list of strings where each string is of the form
\"TASKRUNNER-PROGRAM COMMAND\". The cache for ROOT is always overwritten if it
exists!"
  (puthash (intern ROOT) TASKS taskrunner-tasks-cache))

(defun taskrunner-get-tasks-from-cache (&optional ROOT)
  "Retrieve all tasks for project in ROOT if any exist.
Return nil if none have been previously added."
  (let ((proj-dir (if ROOT
                      ROOT
                    (projectile-project-root))))
    (gethash (intern proj-dir) taskrunner-tasks-cache)))

(defun taskrunner-add-to-build-cache (ROOT BUILD-DIR)
  "Add BUILD-DIR as the build directory for make in ROOT."
  (puthash (intern ROOT) BUILD-DIR taskrunner-build-cache))

(defun taskrunner-get-build-cache (&optional ROOT)
  "Retrieve the build folder for ROOT.  Return nil if it does not exist."
  (let ((proj-dir (if ROOT
                      ROOT
                    (projectile-project-root))))
    (gethash (intern proj-dir) taskrunner-build-cache)))

(defun taskrunner-add-command-to-history (ROOT COMMAND)
  "Add COMMAND to the history cache for project in ROOT."
  (let ((history-cache (gethash (intern ROOT) taskrunner-command-history-cache)))
    (if history-cache
        (if (< (length history-cache) taskrunner-command-history-size)
            (puthash (intern ROOT) (cons COMMAND history-cache)
                     taskrunner-command-history-cache)
          (progn
            (push COMMAND history-cache)
            (puthash (intern ROOT) (butlast history-cache)
                     taskrunner-command-history-cache)))
      (puthash (intern ROOT) (list COMMAND) taskrunner-command-history-cache))))

(defun taskrunner-get-commands-from-history (&optional ROOT)
  "Retrieve command history list from cache if possible.
If ROOT is non-nil then retrieve the command history for project
from that directory.  Otherwise, use the project root as per the
command `projectile-project-root'"
  (let ((proj-dir (if ROOT
                      ROOT
                    (projectile-project-root))))
    (gethash (intern proj-dir) taskrunner-command-history-cache)))

(defun taskrunner-add-custom-command (ROOT COMMAND &optional NO-OVERWRITE)
  "Add a custom command COMMAND to the cache for project in ROOT.
If NO-OVERWRITE is non-nil then do not overwrite the cache file used for storage."
  (let ((comm-list (gethash (intern ROOT) taskrunner-custom-command-cache)))
    ;; If the list is not empty then simply append the new command
    (if comm-list
        (puthash (intern ROOT) (cons COMMAND comm-list) taskrunner-custom-command-cache)
      (puthash (intern ROOT) (list COMMAND) taskrunner-custom-command-cache))
    (unless NO-OVERWRITE
      ;; Write to the cache file to make custom commands persist
      (taskrunner-write-cache-file))))

(defun taskrunner-get-custom-commands (&optional DIR)
  "Retrieve the list of custom commands for the currently visited project.
If DIR is non-nil then retrieve commands for project in that root
folder.  Otherwise, use command `projectile-project-root'.

This function will return a list of strings of the form:
\(\"TASKRUNNER CUSTOM-COMMAND1\" \"TASKRUNNER CUSTOM-COMMAND2\"...)"
  (let ((proj-root (if DIR
                       DIR
                     (projectile-project-root))))
    (gethash (intern proj-root) taskrunner-custom-command-cache)))

(defun taskrunner-delete-custom-command (ROOT COMMAND &optional NO-OVERWRITE)
  "Delete a custom command COMMAND for the project in directory ROOT.
If NO-OVERWRITE is non-nil then do not overwrite the cache file."
  (let ((command-list (gethash (intern ROOT) taskrunner-custom-command-cache)))
    (when command-list
      (setq command-list (remove COMMAND command-list))
      ;; Overwrite the custom commands
      (puthash (intern ROOT) command-list taskrunner-custom-command-cache)
      (unless NO-OVERWRITE
        (taskrunner-write-cache-file)))))

(defun taskrunner-delete-all-custom-commands (&optional DIR NO-OVERWRITE)
  "Delete all custom tasks for a project.
If DIR is non-nil then delete the tasks for the project with root
DIR.  Otherwise, use the output of command `projectile-project-root'.
If NO-OVERWRITE is non-nil then do not overwrite the cache file."
  (let ((proj-root (if DIR
                       DIR
                     (projectile-project-root))))
    (remhash (intern proj-root) taskrunner-custom-command-cache)
    (unless NO-OVERWRITE
      (taskrunner-write-cache-file))))

;; Invalidation functions for caches. These "reset" them
(defun taskrunner-invalidate-build-cache ()
  "Invalidate the entire build cache."
  (clrhash taskrunner-build-cache))

(defun taskrunner-invalidate-tasks-cache ()
  "Invalidate the entire task cache."
  (clrhash taskrunner-tasks-cache))

(defun taskrunner-invalidate-last-command-cache ()
  "Invalidate the entire last command cache."
  (clrhash taskrunner-last-command-cache))

(defun taskrunner-invalidate-command-history-cache ()
  "Invalidate the entire command history cache."
  (clrhash taskrunner-command-history-cache))

(defun taskrunner-invalidate-custom-command-cache ()
  "Invalidate the entire custom command cache."
  (clrhash taskrunner-custom-command-cache))

;; Saving and reading the cache file
(defun taskrunner-read-cache-file ()
  "Read the task cache file and initialize the task caches with its contents."
  (with-temp-buffer
    (let ((taskrunner-cache-filepath (expand-file-name "taskrunner-tasks.eld" user-emacs-directory))
          (file-tasks))
      (when (file-exists-p taskrunner-cache-filepath)
        (with-temp-buffer
          (insert-file-contents taskrunner-cache-filepath)
          (setq file-tasks (car (read-from-string (buffer-string))))
          ;; Load all the caches with the retrieved info
          (setq taskrunner-tasks-cache (nth 0 file-tasks))
          (setq taskrunner-last-command-cache(nth 1 file-tasks))
          (setq taskrunner-build-cache (nth 2 file-tasks))
          (setq taskrunner-command-history-cache (nth 3 file-tasks))
          ;; Length is checked for backwards compatibility.  The cache file will
          ;; be overwritten soon but if the user installed this package before
          ;; the new cache was added, trying to read in the new command cache
          ;; will throw an error
          (when (= (length file-tasks) 5)
            (setq taskrunner-custom-command-cache (nth 4 file-tasks))))))))

(defun taskrunner-write-cache-file ()
  "Save all tasks in the cache to the cache file in Emacs user directory."
  (let ((taskrunner-cache-filepath (expand-file-name "taskrunner-tasks.eld" user-emacs-directory)))
    (write-region (format "%s%s\n" taskrunner--cache-file-header-warning
                          (list (prin1-to-string taskrunner-tasks-cache)
                                (prin1-to-string taskrunner-last-command-cache)
                                (prin1-to-string taskrunner-build-cache)
                                (prin1-to-string taskrunner-command-history-cache)
                                (prin1-to-string taskrunner-custom-command-cache)))
                  nil
                  taskrunner-cache-filepath)))

(defun taskrunner-delete-cache-file ()
  "Delete the cache file used for persistence between Emacs sessions.
The user will be asked to confirm this action before deleting the file."
  (if (y-or-n-p "Are you sure you want to delete the cache file? ")
      (delete-file (expand-file-name "taskrunner-tasks.eld" user-emacs-directory))))

;; Functions/Macros related to finding files which signal what type of build
;; system/taskrunner is used
(defmacro taskrunner-files-matching-regexp (REGEXP DIRECTORY FILE-LIST KEY MATCH-LIST)
  "Create a list containing all file names in FILE-LIST which match REGEXP.
If there are any matches then the list of matching names is added
to alist MATCH-LIST with key KEY.  Each list element has the form:
FILENAME ABSOLUTE-FILE-PATH
and the absolute file path is created by concatenating DIRECTORY with filename."
  `(let ((match-list '()))
     (dolist (elem ,FILE-LIST)
       (if (and (string-match-p ,REGEXP elem)
                (not (file-directory-p (expand-file-name elem ,DIRECTORY))))
           (push (list (intern elem) (expand-file-name elem ,DIRECTORY)) match-list)))
     (when match-list
       (push (list ,KEY match-list) ,MATCH-LIST))))

(defmacro taskrunner-file-in-source-folder-p (ROOT ROOT-FILES FILE-NAME)
  "Look for FILE-NAME within the source folder of a project in directory ROOT.
The source folder is located from ROOT-FILES which is a list containing all of
the files inside of the project's root folder."
  `(let ((src-folder-files)
         (src-folder-path)
         (found-src-flag nil)
         (found-file-p nil)
         (i 0))
     (while (and
             (not found-src-flag)
             (<= i (length taskrunner-build-dir-list)))

       (when (member (elt taskrunner-source-dir-list i) ,ROOT-FILES)
         (setq src-folder-path (expand-file-name (elt taskrunner-source-dir-list i) ,ROOT))
         (setq found-src-flag t))
       (setq i (1+ i)))

     (when found-src-flag
       (setq src-folder-files (directory-files src-folder-path))
       (when (member ,FILE-NAME src-folder-files)
         (setq found-file-p t)))

     found-file-p))

(defun taskrunner-collect-taskrunner-files (DIR)
  "Collect the main taskrunner/build system files in DIR.

This function returns an alist of the form:
\((SYSTEM_1 LOCATION_1) (SYSTEM_2 LOCATION_2)... (SYSTEM_N LOCATION_N))
where LOCATION_1, LOCATION_2...LOCATION_N can either be an alist of the form:
\(FILE_NAME FILE_PATH) or it can be a single string containing the file path
to a single file."
  (let ((proj-root-files (directory-files DIR))
        (files '()))

    (if (member "package.json" proj-root-files)
        (push (list (intern (taskrunner--yarn-or-npm DIR)) (expand-file-name "package.json" DIR)) files))

    (cond
     ((member "gulpfile.js" proj-root-files)
      (push (list 'GULP (expand-file-name "gulpfile.js" DIR)) files))
     ((member "Gulpfile.js" proj-root-files)
      (push (list 'GULP (expand-file-name "Gulpfile.js" DIR)) files)))

    (cond
     ((member "Gruntfile.js" proj-root-files)
      (push (list 'GRUNT (expand-file-name "Gruntfile.js" DIR)) files))
     ((member "Gruntfile.coffee" proj-root-files)
      (push (list 'GRUNT (expand-file-name "Gruntfile.coffee" DIR)) files)))

    (cond
     ((member "Jakefile.js" proj-root-files)
      (push (list 'JAKE (expand-file-name "Jakefile.js" DIR)) files))
     ((member "Jakefile.coffee" proj-root-files)
      (push (list 'JAKE (expand-file-name "Jakefile.coffee" DIR)) files))
     ((member "Jakefile" proj-root-files)
      (push (list 'JAKE (expand-file-name "Jakefile" DIR)) files)))

    (cond
     ((member "rakefile" proj-root-files)
      (push (list 'RAKE (expand-file-name "rakefile" DIR)) files))
     ((member "Rakefile" proj-root-files)
      (push (list 'RAKE (expand-file-name "Rakefile" DIR)) files))
     ((member "rakefile.rb" proj-root-files)
      (push (list 'RAKE (expand-file-name "rakefile.rb" DIR)) files))
     ((member "Rakefile.rb" proj-root-files)
      (push (list 'RAKE (expand-file-name "Rakefile.rb" DIR)) files)))

    (if (member "Cask" proj-root-files)
        (push (list 'CASK (expand-file-name "Cask" DIR)) files))

    (if (member "mix.exs" proj-root-files)
        (push (list 'MIX (expand-file-name "mix.exs" DIR)) files))

    (if (member "project.clj" proj-root-files)
        (push (list 'LEIN (expand-file-name "project.clj" DIR)) files))

    (if (member "Cargo.toml" proj-root-files)
        (push (list 'CARGO (expand-file-name "Cargo.toml" DIR)) files))

    (if (member "stack.yaml" proj-root-files)
        (push (list 'STACK (expand-file-name "stack.yaml" DIR)) files))

    (if (member "CMakeLists.txt" proj-root-files)
        (push (list 'CMAKE (expand-file-name "CMakeLists.txt" DIR)) files))

    (if (member "build.xml" proj-root-files)
        (push (list 'ANT (expand-file-name "build.xml" DIR)) files))

    (if (member "Taskfile.yml" proj-root-files)
        (push (list 'GO-TASK (expand-file-name "Taskfile.yml" DIR)) files))

    (if (member "dodo.py" proj-root-files)
        (push (list 'DOIT (expand-file-name "dodo.py" DIR)) files))

    (if (member "magefile.go" proj-root-files)
        (push (list 'MAGE (expand-file-name "magefile.go" DIR)) files))

    (if (member "maskfile.md" proj-root-files)
        (push (list 'MASK (expand-file-name "maskfile.md" DIR)) files))

    (if (member "Makefile.yaml" proj-root-files)
        (push (list 'CARGO-MAKE (expand-file-name "Makefile.yaml" DIR)) files))

    (if (member "tusk.yml" proj-root-files)
        (push (list 'TUSK (expand-file-name "tusk.yml" DIR)) files))

    (if (member "buidler.config.js" proj-root-files)
        (push (list 'BUIDLER (expand-file-name "buidler.config.js" DIR)) files))

    (if (member "dobi.yml" proj-root-files)
        (push (list 'DOBI (expand-file-name "dobi.yml" DIR)) files))

    ;; Justfile names are case insensitive. Will need to add support for that
    ;; but this will be at a later time. For now, add support for the(what I
    ;; think are) most common names
    (cond
     ((member "justfile" proj-root-files)
      (push (list 'JUST (expand-file-name "justfile" DIR)) files))
     ((member "Justfile" proj-root-files)
      (push (list 'JUST (expand-file-name "Justfile" DIR)) files))
     ((member "JUSTFILE" proj-root-files)
      (push (list 'JUST (expand-file-name "JUSTFILE" DIR)) files)))

    (cond
     ((member "Makefile" proj-root-files)
      (push (list 'MAKE (expand-file-name "Makefile" DIR)) files))
     ((member "makefile" proj-root-files)
      (push (list 'MAKE (expand-file-name "makefile" DIR)) files))
     ((member "GNUmakefile" proj-root-files)
      (push (list 'MAKE (expand-file-name "GNUmakefile" DIR)) files)))

    (taskrunner-files-matching-regexp ".*gradle.*" DIR proj-root-files 'GRADLE files)

    (taskrunner-files-matching-regexp ".*cabal.*" DIR proj-root-files 'CABAL files)

    (taskrunner-files-matching-regexp "go\\.\\(mod\\|sum\\)" DIR proj-root-files 'GO files)

    files))

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
        (setq tasks (append tasks (taskrunner-get-package-json-tasks DIR))))

    (if (or (member "gulpfile.js" work-dir-files)
            (member "Gulpfile.js" work-dir-files))
        (setq tasks (append tasks (taskrunner-get-gulp-tasks DIR))))

    (if (or (member "Gruntfile.js" work-dir-files)
            (member "Gruntfile.coffee" work-dir-files))
        (setq tasks (append tasks (taskrunner-get-grunt-tasks DIR))))

    (if (or (member "Jakefile.js" work-dir-files)
            (member "Jakefile" work-dir-files)
            (member "Jakefile.coffee" work-dir-files))
        (setq tasks (append tasks (taskrunner-get-jake-tasks DIR))))

    (if (or (member "rakefile" work-dir-files)
            (member "Rakefile" work-dir-files)
            (member "rakefile.rb" work-dir-files)
            (member "Rakefile.rb" work-dir-files))
        (setq tasks (append tasks (taskrunner-get-rake-tasks DIR))))

    (if (or (member "gradlew" work-dir-files)
            (member "gradlew.bat" work-dir-files)
            (member "build.gradle" work-dir-files))
        (setq tasks (append tasks (taskrunner-get-gradle-tasks DIR))))

    (if (member "build.xml" work-dir-files)
        (setq tasks (append tasks (taskrunner-get-ant-tasks DIR))))

    (if (member "mix.exs" work-dir-files)
        (setq tasks (append tasks (taskrunner-get-mix-tasks DIR))))

    (if (member "project.clj" work-dir-files)
        (setq tasks (append tasks (taskrunner-get-leiningen-tasks DIR))))

    (if (member "Taskfile.yml" work-dir-files)
        (setq tasks (append tasks (taskrunner-get-go-task-tasks DIR))))

    (if (member "dodo.py" work-dir-files)
        (setq tasks (append tasks (taskrunner-get-doit-tasks DIR))))

    (if (member "magefile.go" work-dir-files)
        (setq tasks (append tasks (taskrunner-get-mage-tasks DIR))))

    (if (member "maskfile.md" work-dir-files)
        (setq tasks (append tasks (taskrunner-get-mask-tasks DIR))))

    (if (member "tusk.yml" work-dir-files)
        (setq tasks (append tasks (taskrunner-get-tusk-tasks DIR))))

    (if (member "buidler.config.js" work-dir-files)
        (setq tasks (append tasks (taskrunner-get-buidler-tasks DIR))))

    (if (member "dobi.yml" work-dir-files)
        (setq tasks (append tasks (taskrunner-get-dobi-tasks DIR))))

    (if (or (member "justfile" work-dir-files)
            (member "Justfile" work-dir-files)
            (member "JUSTFILE" work-dir-files))
        (setq tasks (append tasks (taskrunner-get-just-tasks DIR))))

    (if (member "Makefile.yaml" work-dir-files)
        (setq tasks (append tasks (taskrunner-get-cargo-make-tasks DIR))))

    (cond ((member "CMakeLists.txt" work-dir-files)
           (setq tasks (append tasks (taskrunner-get-cmake-tasks DIR))))
          ((taskrunner-file-in-source-folder-p DIR work-dir-files "CMakeLists.txt")
           (setq tasks (append tasks (taskrunner-get-cmake-tasks DIR)))))

    (cond ((member "meson.build" work-dir-files)
           (setq tasks (append tasks (taskrunner-get-meson-tasks DIR))))
          ((taskrunner-file-in-source-folder-p DIR work-dir-files "meson.build")
           (setq tasks (append tasks (taskrunner-get-meson-tasks DIR)))))

    ;; There should only be one makefile in the directory only look for one type
    ;; of name.
    (cond
     ((member "Makefile" work-dir-files)
      (setq tasks (append tasks (taskrunner-get-make-targets
                                 DIR "Makefile" taskrunner-retrieve-all-make-targets))))
     ((member "makefile" work-dir-files)
      (setq tasks (append tasks (taskrunner-get-make-targets
                                 DIR "makefile" taskrunner-retrieve-all-make-targets))))
     ((member "GNUmakefile" work-dir-files)
      (setq tasks (append tasks (taskrunner-get-make-targets
                                 DIR "GNUmakefile" taskrunner-retrieve-all-make-targets)))))

    ;;; Static targets. These will never change and are hardcoded
    (if (member "Cargo.toml" work-dir-files)
        (setq tasks (append tasks taskrunner--rust-targets)))

    (if (or (member "go.mod" work-dir-files)
            (member "go.sum" work-dir-files))
        (setq tasks (append tasks taskrunner--golang-targets)))

    (if (member "Cask" work-dir-files)
        (setq tasks (append tasks taskrunner--cask-targets)))

    (if (member "stack.yaml" work-dir-files)
        (setq tasks (append tasks taskrunner--stack-targets)))

    ;; Use built in projectile function for cabal. No need to reinvent the wheel
    (if (projectile-cabal-project-p)
        (setq tasks (append taskrunner--cabal-targets)))

    ;;; Return the tasks collected
    tasks))

(defun taskrunner-get-tasks-sync (&optional DIR)
  "Retrieve the cached tasks from the directory DIR or the current project.
If the project does not have any tasks cached then collect all tasks and update
the cache.  If the tasks exist then simply return them.  The tasks returned are
in a list of strings.  Each string has the form TASKRUNNER-PROGRAM TASK-NAME.

Warning: This function runs synchronously and will block Emacs!"
  ;; Read the cache file if it exists.
  ;; This is done only once at startup
  (unless taskrunner--cache-file-read
    (taskrunner-read-cache-file)
    (setq taskrunner--cache-file-read t))

  ;; Retrieve the tasks from cache if possible
  (let* ((proj-root (if DIR
                        DIR
                      (projectile-project-root)))
         (proj-tasks (taskrunner-get-tasks-from-cache proj-root))
         (custom-tasks (taskrunner-get-custom-commands proj-root)))
    ;; If the tasks do not exist, retrieve them first and then add to cache.
    (unless proj-tasks
      (setq proj-tasks (taskrunner-collect-tasks proj-root))
      (taskrunner-add-to-tasks-cache proj-root proj-tasks)
      (taskrunner-write-cache-file))
    ;; Return the tasks
    (append custom-tasks proj-tasks)))

(defun taskrunner--start-async-task-process (FUNC &optional DIR)
  "Run `emacs-async' to retrieve the tasks for the currently visited project.
The resulting list of tasks which may be empty is then passed to
the function FUNC.  This function must accept only one argument
which will be a list of strings consisting of taskrunner/build
systems and target name.

Example:
\(\"MAKE target1\" \"MAKE target2\"...)

If DIR is non-nil then tasks are gathered from that directory."
  ;; Variable used so that the async call can use the DIR argument
  (setq taskrunner--async-process-dir DIR)
  (async-start
   `(lambda ()
      ;; inject the load path so we can find taskrunner
      ,(async-inject-variables "\\`load-path\\'")
      ;; Inject all variables from the taskrunner package
      ,(async-inject-variables "taskrunner-.*")
      ;; For cl-map's
      (require 'cl-lib)
      ;; Used to enable the use of package-installed-p in taskrunner
      (require 'package)
      ;; Main package
      (require 'taskrunner)
      (let* ((proj-root (if taskrunner--async-process-dir
                            taskrunner--async-process-dir
                          (projectile-project-root)))
             (proj-tasks (taskrunner-collect-tasks proj-root)))
        (list proj-root proj-tasks taskrunner-build-cache)))
   (lambda (TARGETS)
     (let* ((proj-dir (nth 0 TARGETS))
            (proj-tasks (nth 1 TARGETS))
            (build-cache (nth 2 TARGETS))
            (custom-tasks (taskrunner-get-custom-commands proj-dir)))
       (taskrunner-add-to-tasks-cache proj-dir proj-tasks)
       ;; Overwrite the build cache. It might or might not have been updated
       ;; with more directories
       (setq taskrunner-build-cache build-cache)
       (taskrunner-write-cache-file)
       ;; This is to prevent erros occurring when C-g is used from whatever is
       ;; present in FUNC
       (with-local-quit
         ;; Add custom tasks to output here
         (funcall FUNC (append custom-tasks proj-tasks)))))))

(defun taskrunner-get-tasks-async (FUNC &optional DIR)
  "Retrieve the tasks for the currently visited project asynchronously.
The resulting list of tasks (which may be empty) is then passed to
the function FUNC.  This function must accept only one argument
which will be a list of strings consisting of taskrunner/build
systems and target name.

Example:
\(\"MAKE target1\" \"MAKE target2\"...)

If the tasks exist in the cache then they are retrieved right away. Otherwise,
an `emacs-async' process is started to collect them in the background.  This
means that FUNC might be called almost instantaneously or at a later time which
can usually range between 2-10 seconds depending on how many tasks need to be
collected from different systems.

If DIR is non-nil then tasks are gathered from that directory."
  ;; Read the cache file if it exists.
  ;; This is done only once at startup
  (unless taskrunner--cache-file-read
    (taskrunner-read-cache-file)
    (setq taskrunner--cache-file-read t))

  (let* ((proj-root (if DIR
                        DIR
                      (projectile-project-root)))
         (proj-tasks (taskrunner-get-tasks-from-cache proj-root))
         (custom-tasks (taskrunner-get-custom-commands proj-root)))
    ;; Attempt to avoid spawning a process. On Linux/MacOS this should not be
    ;; too much of a problem but it can be quite slow on Windows
    (if proj-tasks
        (with-local-quit
          ;; Add custom tasks here is the tasks do not need to be gathered
          ;; The will appear at the "top" of the output if the fronend does not
          ;; do any sorting
          (funcall FUNC (append custom-tasks proj-tasks)))
      (taskrunner--start-async-task-process FUNC proj-root))))

(defun taskrunner-project-cached-p (&optional DIR)
  "Check if either the current project or the one in directory DIR are cached.
Return t or nil."
  (let ((proj-root (if DIR
                       DIR
                     (projectile-project-root))))
    (if (taskrunner-get-tasks-from-cache proj-root)
        t
      nil)))

(defun taskrunner-refresh-cache-sync (&optional DIR)
  "Retrieve all tasks for a project and update the tasks cache.
If DIR is non-nil then the tasks are gathered from that folder,
otherwise the project root is used.  This function is synchronous
so using it will block Emacs unless its ran on a thread."
  (let* ((proj-root (if DIR
                        DIR
                      (projectile-project-root))))
    ;; Set the current value for the project root to nil in order to force the
    ;; tasks to be collected again if they do exist.
    (taskrunner-add-to-tasks-cache proj-root nil)
    (taskrunner-get-tasks-sync proj-root)))

(defun taskrunner-refresh-cache-async (FUNC &optional DIR)
  "Retrieve all tasks asynchronously and pass them to FUNC.
If DIR is non-nil then refresh the tasks for the project in that directory.
If there were tasks previously loaded then remove them, retrieve all tasks
again and set the corresponding project to the new list.  Return a list
containing the new tasks."
  (let* ((proj-root (if DIR
                        DIR
                      (projectile-project-root))))
    ;; Set the current value for the project root to nil in order to force the
    ;; tasks to be collected again if they do exist.
    (taskrunner-add-to-tasks-cache proj-root nil)
    (taskrunner-get-tasks-async FUNC proj-root)))

(defun taskrunner--generate-compilation-buffer-name (TASKRUNNER TASK)
  "Generate a buffer name for compilation of TASK with TASKRUNNER program."
  ;; The compilation-start function requires a function which accepts only 1
  ;; argument, the mode. It is necessary to return a lambda function so we can
  ;; use the taskrunner/task combo in the name.
  (lambda (mode)
    ;; This is just so the bytecompiler does not complain.
    (intern mode)
    (concat "*taskrunner-" TASKRUNNER "-" TASK "*" )))

(defun taskrunner-run-task (TASK &optional DIR ASK USE-BUILD-CACHE)
  "Run command TASK in project root or directory DIR if provided.
If ASK is non-nil then ask the user to supply extra arguments to
the task to be ran.  If USE-BUILD-CACHE is non-nil then attempt
to use the build directory for the project which is retrieved
from the build cache."
  (let* ((default-directory (if DIR
                                DIR
                              (projectile-project-root)))
         (taskrunner-program (downcase (car (split-string TASK " "))))
         ;; Concat the arguments since we might be rerunning a command with arguments from history
         (task-name (mapconcat 'identity
                               (cdr (split-string TASK " ")) " "))
         (command)
         ;; Set the exec path to include all binaries so the taskrunners can be found
         ;; This should not produce a problem if the binaries/folders do not exist
         (exec-path (append exec-path (list taskrunner-go-task-bin-path
                                            taskrunner-mage-bin-path
                                            taskrunner-tusk-bin-path
                                            taskrunner-doit-bin-path
                                            taskrunner-dobi-bin-path))))
    (when ASK
      (setq task-name (read-string (concat "Arguments to add to command: ")
                                   task-name)))

    ;; Add the commands to history and set the new last command ran The command
    ;; is concatenated again so any arguments provided(if there are any) are saved
    (taskrunner-set-last-command-ran (projectile-project-root) default-directory
                                     (concat (upcase taskrunner-program) " " task-name))
    (taskrunner-add-command-to-history (projectile-project-root)
                                       (concat (upcase taskrunner-program) " " task-name))

    ;; Command to be ran is built here.  Some taskrunners/build systems require
    ;; special handling(cache lookups/prepending/appending some extra command to
    ;; run the task...) and all of this is done here
    (cond ((string-equal "ninja" taskrunner-program)
           (when (and USE-BUILD-CACHE
                      (taskrunner-get-build-cache default-directory))
             (setq default-directory (taskrunner-get-build-cache default-directory)))
           (setq command (concat taskrunner-program " " task-name)))
          ((string-equal "make" taskrunner-program)
           (when (and USE-BUILD-CACHE
                      (taskrunner-get-build-cache default-directory))
             (setq default-directory (taskrunner-get-build-cache default-directory)))
           (setq command (concat taskrunner-program " " task-name)))
          ((string-equal "npm" taskrunner-program)
           (setq command (concat taskrunner-program " " "run" " " task-name)))
          ((string-equal "yarn" taskrunner-program)
           (setq command (concat taskrunner-program " " "run" " " task-name)))
          ((string-equal "buidler" taskrunner-program)
           (setq command (concat "npx" " " taskrunner-program " " task-name)))
          ((string-equal "dobi" taskrunner-program)
           (setq command (concat taskrunner-dobi-bin-name " " task-name)))
          (t
           (setq command (concat taskrunner-program " " task-name))))

    (taskrunner-write-cache-file)
    (compilation-start command t (taskrunner--generate-compilation-buffer-name taskrunner-program task-name) t)))

(defun taskrunner-rerun-last-task (DIR)
  "Rerun the last task which was ran for the project in DIR."
  (let ((last-ran-command (taskrunner-get-last-command-ran DIR)))
    (if last-ran-command
        (taskrunner-run-task (cadr last-ran-command) (car last-ran-command) nil t)
      (message taskrunner-no-previous-command-ran-warning))))

(defun taskrunner-get-compilation-buffers ()
  "Return a list of the names of all taskrunner compilation buffers."
  (let ((taskrunner-buffers '()))
    (dolist (buff (buffer-list))
      (if (string-match taskrunner--buffer-name-regexp (buffer-name buff))
          (push (buffer-name buff) taskrunner-buffers)))
    taskrunner-buffers))

(defun taskrunner-kill-compilation-buffers ()
  "Kill all taskrunner compilation buffers."
  (let ((taskrunner-buffers (taskrunner-get-compilation-buffers)))
    (when taskrunner-buffers
      (dolist (buff taskrunner-buffers)
        (kill-buffer buff)))))

(defun taskrunner-clean-up-projects (&optional NO-OVERWRITE)
  "Remove all projects which do not exist anymore from all caches.
If NO-OVERWRITE is non-nil then do not overwrite the cache file.  Otherwise,
overwrite it with the new cache contents."
  (let ((proj-paths '()))
    (maphash (lambda (key _)
               (when (not (file-exists-p (symbol-name key)))
                 (push key proj-paths)))
             taskrunner-tasks-cache)

    ;; Remove all projects whose paths are not accessible anymore. If the
    ;; project path does not exist in one of the caches then remhash will not
    ;; throw an error. This means that we can safely iterate over each cache and
    ;; remove the elements even if they might not even exist within it.
    (dolist (path proj-paths)
      (remhash path taskrunner-tasks-cache)
      (remhash path taskrunner-command-history-cache)
      (remhash path taskrunner-last-command-cache)
      (remhash path taskrunner-build-cache))
    (unless NO-OVERWRITE
      (taskrunner-write-cache-file))))

;; Debugging utilities
(defmacro taskrunner--insert-hashmap-contents (HASHMAP-NAME)
  "Insert the elements of the hashmap with HASHMAP-NAME in current buffer."
  `(maphash (lambda (key elem)
              (insert (symbol-name key) " " (format "%s" elem) "\n"))
            ,HASHMAP-NAME))

(defun taskrunner--debug-show-cache-contents ()
  "Debugging function used to show the cache contents in a new temp buffer.
This is not meant to be used for anything seen by the user."
  (interactive)
  (let ((buff (generate-new-buffer "*taskrunner-debug-cache-contents*")))
    (set-buffer buff)
    (insert "Task cache contents\n")
    (taskrunner--insert-hashmap-contents taskrunner-tasks-cache)
    (insert "\nLast command cache contents\n")
    (taskrunner--insert-hashmap-contents taskrunner-last-command-cache)
    (insert "\nBuild cache contents\n")
    (taskrunner--insert-hashmap-contents taskrunner-build-cache)
    (insert "\nCommand history cache contents\n")
    (taskrunner--insert-hashmap-contents taskrunner-command-history-cache)
    (insert "\nCustom commands cache contents\n")
    (taskrunner--insert-hashmap-contents taskrunner-custom-command-cache)
    (switch-to-buffer buff)))

;; Check if the notification library is installed and as an extra step check if
;; Emacs is compiled with "NOTIFY". If those are present then load the functions.
;; TODO: Will this work with windows?

;; Thanks to:
;; https://stackoverflow.com/questions/7790382/how-to-determine-whether-a-package-is-installed-in-elisp
;; for the tip about 'noerror in require
(when (and (require 'notifications nil 'noerror)
           (string-match-p "NOTIFY" system-configuration-features))
  (require 'notifications)
  (defun taskrunner--show-notification (BUFF _)
    "Show a desktop notification when compilation/comint mode is finished running"
    (when (or (fboundp 'notifications-notify)
              (fboundp 'w32-notifications-notify))
      (let ((buff-name (buffer-name BUFF))
            (program-name)
            (task-name)
            (display-string))
        ;; Create a notification only when its a taskrunner buffer
        (when (string-match-p taskrunner--buffer-name-regexp buff-name)
          (setq program-name (cadr (split-string buff-name "-")))
          (setq task-name (car (split-string
                                (caddr (split-string buff-name "-")) "*")))
          (setq display-string (concat "The command \""  program-name " "
                                       task-name "\" "
                                       "has finished!"))
          ;; Decide the system type
          (cond
           ((or (equal system-type 'darwin)
                (equal system-type 'gnu/linux))
            ;; Silence the byte compiler on windows
            (if (fboundp 'notifications-notify)
                (notifications-notify
                 :title "Emacs Taskrunner"
                 :body display-string
                 :urgency 'low)))
           ((equal system-type 'windows-nt)
            ;; Silence the byte compiler on linux/macos platforms
            (if (fboundp 'w32-notification-notify)
                (w32-notification-notify
                 :title "Emacs Taskrunner"
                 :body display-string
                 :level 'info))))))))

  (defun taskrunner-notification-on ()
    "Turn on notifications which are shown when a task ran with taskrunner is finished.."
    (unless (member 'taskrunner--show-notification compilation-finish-functions)
      (push 'taskrunner--show-notification compilation-finish-functions)))

  (defun taskrunner-notification-off ()
    "Turn off notifications which are shown when a task ran with taskrunner is finished.."
    (if (member 'taskrunner--show-notification compilation-finish-functions)
        (setq compilation-finish-functions (remove 'taskrunner--show-notification
                                                   compilation-finish-functions)))))

;;;; Footer

(provide 'taskrunner)
;;; taskrunner.el ends here
