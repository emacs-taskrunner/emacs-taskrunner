;;; taskrunner.el --- Retrieve build system/taskrunner tasks  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Yavor Konstantinov

;; Author: Yavor Konstantinov <ykonstantinov1 AT gmail DOT com>
;; URL: https://github.com/emacs-taskrunner/emacs-taskrunner
;; Version: 0.5
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
  :group 'convenience)

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

(defvar taskrunner-build-cache '()
  "A cache used to store project build folders for retrieval.
It is an alist of the form (project-root . build-folder)")

(defvar taskrunner--cache-file-read nil
  "Indicates whether or not the cache file has been read.
Do not edit unless you want to reread the cache.")

(defvar taskrunner--tempdir nil
  "Used to hold the working directory argument for usage in the async package.
Do not edit this manually!")

(defconst taskrunner--cache-file-header-warning
  ";;This file is generated automatically. Please do not edit by hand!\n"
  "Warning inserted at the top of the tasks cache file to indicate not to edit it.")

(defconst taskrunner--buffer-name-regexp
  "\*taskrunner-.+*"
  "Regexp used to find all buffers running tasks.")

;; Functions:

(defmacro taskrunner--make-task-buff-name (TASKRUNNER)
  "Create a buffer name used to retrieve the tasks for TASKRUNNER."
  `(concat "*taskrunner-" ,TASKRUNNER "-tasks-buffer*"))

(defun taskrunner--narrow-to-line ()
  "Narrow to the line entire line that the point lies on."
  (narrow-to-region (point-at-bol)
                    (point-at-eol)))

(defun taskrunner-get-last-command-ran (&optional DIR)
  "Retrieve the last command ran for the project.
If DIR is non-nil then return the command for for that directory.  Otherwise,
use the project root for the currently visited buffer."
  (let ((proj-dir (if DIR
                      (intern DIR)
                    (intern (projectile-project-root)))))
    (alist-get proj-dir taskrunner-last-command-cache)))

(defun taskrunner-set-last-command-ran (ROOT DIR COMMAND)
  "Set the COMMAND ran in DIR to be the last command ran for project in ROOT."
  ;; Remove the the previous command if it exists. Assoc-delete-all does not
  ;; throw an error so it is safe
  (let ((new-command-cache (assq-delete-all (intern ROOT)
                                            taskrunner-last-command-cache)))
    ;; Reset the cache with new command added
    (setq taskrunner-last-command-cache (push (list (intern ROOT) DIR COMMAND) new-command-cache))))

(defun taskrunner-add-to-tasks-cache (DIR TASKS)
  "Add TASKS for project in directory DIR to the tasks cache."
  (setq taskrunner-tasks-cache (assq-delete-all (intern DIR)
                                                taskrunner-tasks-cache))
  (push (cons (intern DIR) TASKS) taskrunner-tasks-cache))

(defun taskrunner-add-to-build-cache (PROJ-ROOT BUILD-DIR)
  "Add BUILD-DIR as the build directory for make in PROJ-ROOT."
  (setq taskrunner-build-cache (assq-delete-all (intern PROJ-ROOT) taskrunner-build-cache))
  (push (list (intern PROJ-ROOT) BUILD-DIR) taskrunner-build-cache))

(defun taskrunner-get-build-cache (PROJ-ROOT)
  "Retrieve the build folder for PROJ-ROOT.  Return nil if it does not exist."
  (car-safe (alist-get (intern PROJ-ROOT) taskrunner-build-cache)))

(defun taskrunner-invalidate-build-cache ()
  "Invalidate the entire build cache."
  (setq taskrunner-build-cache '()))

(defun taskrunner-invalidate-tasks-cache ()
  "Invalidate the entire task cache."
  (setq taskrunner-tasks-cache nil))

(defun taskrunner-invalidate-last-command-cache ()
  "Invalidate the entire last command cache."
  (setq taskrunner-last-command-cache nil))

(defun taskrunner--read-cache-file ()
  "Read the task cache file and initialize the task caches with its contents."
  (with-temp-buffer
    (let ((cache-file-path (expand-file-name "taskrunner-tasks.eld" user-emacs-directory))
          (file-tasks))
      (when (file-exists-p cache-file-path)
        (with-temp-buffer
          (insert-file-contents cache-file-path)
          (setq file-tasks (car (read-from-string (buffer-string))))
          ;; Load all the caches with the retrieved info
          (setq taskrunner-tasks-cache (nth 0 file-tasks))
          (setq taskrunner-last-command-cache(nth 1 file-tasks))
          (setq taskrunner-build-cache (nth 2 file-tasks)))))))

(defun taskrunner--format-list-for-write (LIST)
  "Format the alist LIST for writing.
LIST should have the form of (SYMBOL STRING STRING...STRING).
This function will surround each STRING with another set of
double quotes.  This is done so that when the file is read back,
the strings are still surrounded with double quotes."
  (if LIST
      (let ((formatted-list '()))
        (dolist (elem LIST)
          (push  (cl-map 'list (lambda (elem)
                                 (if (stringp elem)
                                     (concat "\"" elem "\"")
                                   elem))
                         elem)
                 formatted-list))
        formatted-list)
    '()))

(defun taskrunner--write-to-cache-file (TASKS)
  "Write TASKS to the taskrunner tasks cache file."
  (let ((content-string (format "%s%s\n" taskrunner--cache-file-header-warning TASKS))
        (cache-filepath (expand-file-name "taskrunner-tasks.eld" user-emacs-directory)))
    (write-region content-string nil cache-filepath)))

(defun taskrunner--save-tasks-to-cache-file ()
  "Save all tasks in the cache to the cache file in Emacs user directory."
  (taskrunner--write-to-cache-file (list (taskrunner--format-list-for-write taskrunner-tasks-cache)
                                         (taskrunner--format-list-for-write taskrunner-last-command-cache)
                                         (taskrunner--format-list-for-write taskrunner-build-cache))))

(defmacro taskrunner-buffer-matching-regexp (REGEXP DIRECTORY FILE-LIST KEY MATCH-LIST)
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
        (push (list "NPM" (expand-file-name "package.json" DIR)) files))

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
      (push (list "JAKE" (expand-file-name "Jakefile.js" DIR)) files))
     ((member "Jakefile.coffee" proj-root-files)
      (push (list "JAKE" (expand-file-name "Jakefile.coffee" DIR)) files))
     ((member "Jakefile" proj-root-files)
      (push (list "JAKE" (expand-file-name "Jakefile" DIR)) files)))

    (cond
     ((member "rakefile" proj-root-files)
      (push (list "RAKE" (expand-file-name "rakefile" DIR)) files))
     ((member "Rakefile" proj-root-files)
      (push (list "RAKE" (expand-file-name "Rakefile" DIR)) files))
     ((member "rakefile.rb" proj-root-files)
      (push (list "RAKE" (expand-file-name "rakefile.rb" DIR)) files))
     ((member "Rakefile.rb" proj-root-files)
      (push (list "RAKE" (expand-file-name "Rakefile.rb" DIR)) files)))

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

    (taskrunner-buffer-matching-regexp ".*gradle.*" DIR proj-root-files 'GRADLE files)

    (taskrunner-buffer-matching-regexp ".*cabal.*" DIR proj-root-files 'CABAL files)

    (taskrunner-buffer-matching-regexp "go\\.\\(mod\\|sum\\)" DIR proj-root-files 'GO files)

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
    (taskrunner--read-cache-file)
    (setq taskrunner--cache-file-read t))

  ;; Retrieve the tasks from cache if possible
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
          ;; Write to the cache file when a new set of tasks is found.
          ;; This will overwrite anything
          (taskrunner--save-tasks-to-cache-file)))
    ;; Return the tasks
    proj-tasks))


(defun taskrunner-get-tasks-async (FUNC &optional DIR)
  "Retrieve the tasks from the currently visited project asynchronously.
The resulting list of tasks which may be empty is then passed to
the function FUNC.  This function must accept only one argument
which will be a list of strings consisting of taskrunner/build
systems and target name.  Example: \(\"MAKE target1\" \"MAKE
target2\"...)

If DIR is non-nil then tasks are gathered from that directory."
  ;; Read the cache file if it exists.
  ;; This is done only once at startup
  (unless taskrunner--cache-file-read
    (message "READ FILE")
    (taskrunner--read-cache-file)
    (setq taskrunner--cache-file-read t))

  ;; Variable used so that the async call can use the DIR argument
  (setq taskrunner--tempdir DIR)
  (async-start
   `(lambda ()
      ;; inject the load path so we can find taskrunner
      ,(async-inject-variables "\\`load-path\\'")
      ;; Inject all variables from the taskrunner package
      ,(async-inject-variables "taskrunner-.*")
      (require 'cl-lib)
      (require 'taskrunner)
      (let* ((proj-root (if taskrunner--tempdir
                            taskrunner--tempdir
                          (projectile-project-root)))
             (proj-tasks (alist-get (intern proj-root) taskrunner-tasks-cache))
             (cache-status nil))
        ;; If the tasks do not exist then collect them and set cache-status.
        ;; cache-status can be t if the project is already cached or nil if it had
        ;; to be retrieved.
        (if (null proj-tasks)
            (progn (setq proj-tasks (taskrunner-collect-tasks proj-root))
                   (setq cache-status nil))
          (setq cache-status t))
        (list cache-status proj-root proj-tasks taskrunner-build-cache))
      )
   (lambda (TARGETS)
     (let ((cache-status (car TARGETS))
           (proj-dir (cadr TARGETS))
           (proj-tasks (caddr TARGETS))
           (build-cache (cadddr TARGETS)))
       ;; If the tasks are not cached then add them to the cache and write it to the file.
       (unless cache-status
         (taskrunner-add-to-tasks-cache proj-dir proj-tasks)
         (setq taskrunner-build-cache build-cache)
         (taskrunner--save-tasks-to-cache-file))
       (funcall FUNC proj-tasks)))))

(defun taskrunner-project-cached-p (&optional DIR)
  "Check if either the current project or the one in directory DIR are cached.
Return t or nil."
  (let ((proj-root (if DIR
                       (intern DIR)
                     (intern (projectile-project-root)))))
    (if (alist-get proj-root taskrunner-tasks-cache)
        t
      nil)))

(defun taskrunner-refresh-cache-sync (&optional DIR)
  "Retrieve all tasks for project in DIR or the current project and set cache.
If there were tasks previously loaded then remove them, retrieve all tasks
again and set the corresponding project to the new list.  Return a list
containing the new tasks."
  (let* ((proj-root (if DIR
                        DIR
                      (projectile-project-root)))
         (proj-tasks (taskrunner-collect-tasks proj-root)))
    ;; remove old tasks if they exist
    (assq-delete-all (intern proj-root) taskrunner-tasks-cache)
    ;; Add new tasks
    (push (cons (intern proj-root) proj-tasks) taskrunner-tasks-cache)
    ;; Write to the cache file when a new set of tasks is found
    (taskrunner--save-tasks-to-cache-file)))

(defun taskrunner-refresh-cache-async (FUNC &optional DIR)
  "Retrieve all tasks asynchronously and pass them to FUNC.
If DIR is non-nil then refresh the tasks for the project in that directory.
If there were tasks previously loaded then remove them, retrieve all tasks
again and set the corresponding project to the new list.  Return a list
containing the new tasks."
  (let* ((proj-root (if DIR
                        DIR
                      (projectile-project-root))))
    ;; remove old tasks if they exist
    (setq taskrunner-tasks-cache (assq-delete-all (intern proj-root) taskrunner-tasks-cache))
    (taskrunner-get-tasks-async FUNC)
    ))

(defun taskrunner--generate-buffer-name (TASKRUNNER TASK)
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
         (task-name (cadr (split-string TASK " ")))
         (command))
    (when ASK
      (setq task-name (read-string (concat "Arguments to add to command: ")
                                   task-name)))
    ;; Special case handling for commands which use the build cache or which need
    ;; extra arguments provided to run a specific task
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
          (t
           (setq command (concat taskrunner-program " " task-name))))

    ;; Add to last command cache(overwrites previous command) Need to add
    ;; special handling for yarn/npm since they require the run keyword inserted
    ;; between taskname and npm/yarn
    (if (or (string-equal taskrunner-program "npm")
            (string-equal taskrunner-program "yarn"))
        (taskrunner-set-last-command-ran (projectile-project-root) default-directory (concat taskrunner-program " " task-name))
      (taskrunner-set-last-command-ran (projectile-project-root) default-directory command))

    (compilation-start command t (taskrunner--generate-buffer-name taskrunner-program task-name) t)))

(defun taskrunner-rerun-last-task (DIR)
  "Rerun the last task which was ran for the project in DIR."
  (let ((last-ran-command (taskrunner-get-last-command-ran DIR)))
    (if last-ran-command
        (taskrunner-run-task (cadr last-ran-command) (car last-ran-command))
      (message taskrunner-no-previous-command-ran-warning))))


(defun taskrunner--debug-show-cache-contents ()
  "Debugging function used to show the cache contents in a new temp buffer.
This is not meant to be used for anything seen by the user."
  (interactive)
  (let ((buff (generate-new-buffer "*taskrunner-debug-cache-contents*")))
    (set-buffer buff)
    (insert "Task cache contents\n")
    (dolist (el taskrunner-tasks-cache)
      (insert (format "%s\n" el)))
    (insert "\nLast command cache contents\n")
    (dolist (el taskrunner-last-command-cache)
      (insert (format "%s\n" el)))
    (insert "\nBuild cache contents\n")
    (dolist (el taskrunner-build-cache)
      (insert (format "%s\n" el)))
    (switch-to-buffer buff)))


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

(defun taskrunner-clean-up-projects ()
  "Remove all projects which do not exist anymore from all caches.
Update all caches and the cache file after this is performed."
  (let ((new-task-cache '())
        (new-command-cache '())
        (new-build-cache '()))

    (dolist (task taskrunner-tasks-cache)
      (if (file-directory-p (symbol-name (car task)))
          (push task new-task-cache)))

    (dolist (command taskrunner-last-command-cache)
      (if (file-directory-p (symbol-name(car command)))
          (push command new-command-cache)))

    (dolist (build-folder taskrunner-build-cache)
      (if (file-directory-p (symbol-name (car build-folder)))
          (push build-folder new-build-cache)))

    (setq taskrunner-tasks-cache new-task-cache)
    (setq taskrunner-last-command-cache new-command-cache)
    (setq taskrunner-build-cache new-build-cache)

    (taskrunner--save-tasks-to-cache-file)))

;;;; Footer

(provide 'taskrunner)
;;; taskrunner.el ends here
