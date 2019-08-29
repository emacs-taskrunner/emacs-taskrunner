;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'taskrunner)

(defcustom taskrunner-rg-bin-path (executable-find "rg")
  "Path to ripgrep(rg) binary.")

(defcustom taskrunner-ag-bin-path (executable-find "ag")
  "Path to the silver searcher(ag) binary.")

(defcustom taskrunner-find-bin-path (executable-find "find")
  "Path to the find binary.")

(defconst taskrunner--rg-filename-command "rg --files -g "
  "Command line arguments for ripgrep.")

(defconst taskrunner--ag-filename-command "ag -g "
  "Command line arguments for silver searcher(ag).")

(defconst taskrunner--find-filename-command " -type f -regextype posix-egrep -regex "
  "Command line arguments used for the find utility.")

(defsubst taskrunner-searchers-installed-p ()
  "Check if search programs are installed."
  (or (file-executable-p taskrunner-rg-bin-path)
      (file-executable-p taskrunner-ag-bin-path)
      (file-executable-p taskrunner-grep-bin-path)))

(defsubst taskrunner-find-relative-path (PATH &optional DIR)
  "Find the relative filename for PATH and return it.
If DIR is non-nil then use that as the absolute path name for
which to find the relative path.  Otherwise, use the command
`projectile-project-root'."
  (let ((proj-root (if DIR
                       DIR
                     (projectile-project-root))))
    (file-relative-name PATH proj-root)))

(defsubst taskrunner-find-absolute-path (PATH &optional DIR)
  "Return the absolute path for PATH.
If DIR is non-nil then use that as a basepath, otherwise use the
  output of the command `projectile-project-root'"
  (let ((base-path (if DIR
                       DIR
                     (projectile-project-root))))
    (expand-file-name PATH base-path)))

(defsubst taskrunner-locate-filename (FILENAME-REGEXP ROOT &optional RG-GLOB)
  "Look for files whose names match FILENAME-REGEXP in directory ROOT.
RG-GLOB"
  (let ((search-output))
    (cond
     ((and (file-executable-p taskrunner-rg-bin-path)
           RG-GLOB)
      (let ((rg-matches '()))
        (message "Using RG")
        ;; It seems like all wildcard searches for ripgrep using --files -g have
        ;; to surround the regexp with *
        (dolist (glob RG-GLOB)
          (setq rg-matches (append (split-string (shell-command-to-string
                                                  (concat taskrunner--rg-filename-command " *"  glob " " ROOT ))
                                                 "\n" t)
                                   rg-matches))
          )
        rg-matches
        )
      )
     ((file-executable-p taskrunner-ag-bin-path)
      (message "Using AG")
      (split-string (shell-command-to-string
                     (concat taskrunner--ag-filename-command " "  FILENAME-REGEXP " " ROOT )) "\n" t)
      )
     ((file-executable-p taskrunner-grep-bin-path)
      (message "Using find")
      (split-string (shell-command-to-string
                     (concat "find " ROOT "-type f -regextype posix-egrep -regex " FILENAME-REGEXP)) "\n" t))
     )
    )
  )

(defun taskrunner-get-all-tasks-dir (FILENAME-REGEXP ROOT FUNC &optional RG-GLOB)
  "Find all filenames matching FILENAME-REGEXP in directory ROOT.
The containing directory of each file which matches FILENAME-REGEXP is passed
to FUNC.  If RG-GLOB is non-nil, it should be a list of git style globs which
will be passed to ripgrep."
  (let ((search-result (taskrunner-locate-filename FILENAME-REGEXP ROOT RG-GLOB))
        (tasks-found '()))
    (when search-result
      (dolist (file-path search-result)
        (when (file-exists-p file-path)
          ;; (message "FILE EXISTS %s" file-path)
          (setq tasks-found (append tasks-found (funcall FUNC (file-name-directory file-path))))
          )
        )
      )
    tasks-found
    )
  )

(defun taskrunner-get-all-tasks-file (FILENAME-REGEXP ROOT FUNC &optional RG-GLOB)
  "Find all filenames matching FILENAME-REGEXP in directory ROOT.
Each resulting filepath(absolute) which matches FILENAME-REGEXP
is passed to FUNC.  If RG-GLOB is non-nil, it should be a list of
git style globs which will be passed to ripgrep."
  (let ((search-result (taskrunner-locate-filename FILENAME-REGEXP ROOT RG-GLOB))
        (tasks-found '()))
    (when search-result
      (dolist (file-path search-result)
        (when (file-exists-p file-path)
          ;; (message "FILE EXISTS %s" file-path)
          (setq tasks-found (append tasks-found (funcall FUNC file-path)))
          ;; (append tasks-found (funcall FUNC (file-name-directory file-path)))
          )
        )
      )
    tasks-found
    )
  )


(defun taskrunner-collect-tasks-full-search (DIR)
  "Locate and extract all tasks for the project in directory DIR.
Returns a list containing all possible tasks.  Each element is of the form
'TASK-RUNNER-PROGRAM TASK-NAME'.  This is done for the purpose of working with
projects which might use multiple task runners.

Use this function if you want to retrieve the tasks from a project without
updating the cache."
  (let ((work-dir-files (directory-files DIR))
        (tasks '()))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "package\.json$" DIR
                                                  'taskrunner-get-package-json-tasks
                                                  '("package\.json"))))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "[Gg]ulpfile\.js$" DIR
                                                  'taskrunner-get-gulp-tasks
                                                  '("[Gg]ulpfile\.js"))))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*Gruntfile\.(js|coffee)$\"" DIR
                                                  'taskrunner-get-grunt-tasks
                                                  '("Gruntfile\.*"))))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*Jakefile(\.js|\.coffee)?$\"" DIR
                                                  'taskrunner-get-jake-tasks '("Jakefile\.*"))))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\"[Rr]akefile(\.rb)?$\"" DIR
                                                  'taskrunner-get-rake-tasks
                                                  '("[Rr]akefile" "[Rr]akefile\.rb"))))


    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*mix\.exs$\"" DIR
                                                  'taskrunner-get-mix-tasks
                                                  "mix\.exs")))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*project\.clj$\"" DIR
                                                  'taskrunner-get-leiningen-tasks
                                                  "project\.clj")))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*Taskfile\.yml$\"" DIR
                                                  'taskrunner-get-go-task-tasks
                                                  "Taskfile\.yml")))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*dodo\.py$\"" DIR
                                                  'taskrunner-get-doit-tasks
                                                  "dodo\.py")))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*magefile\.go$\"" DIR
                                                  'taskrunner-get-mage-tasks
                                                  "magefile\.go")))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*maskfile\.md$\"" DIR
                                                  'taskrunner-get-mask-tasks
                                                  "maskfile\.md")))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*tusk\.yml$\"" DIR
                                                  'taskrunner-get-tusk-tasks
                                                  "tusk\.yml")))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*buidler\.config\.js$\"" DIR
                                                  'taskrunner-get-buidler-tasks
                                                  "buidler\.config\.js")))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*dobi\.yml$\"" DIR
                                                  'taskrunner-get-dobi-tasks
                                                  "dobi\.yml")))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*(Justfile|justfile|JUSTFILE)$\"" DIR
                                                  'taskrunner-get-just-tasks
                                                  '("Justfile" "justfile" "JUSTFILE"))))

    ;; (cond ((member "meson.build" work-dir-files)
    ;;        (setq tasks (append tasks (taskrunner-get-meson-tasks DIR))))
    ;;       ((taskrunner-file-in-source-folder-p DIR work-dir-files "meson.build")
    ;;        (setq tasks (append tasks (taskrunner-get-meson-tasks DIR)))))

    ;; (if (taskrunner-locate-filename "\".*meson\.build$\"" DIR '("meson\.build")))

    ;; This will handle projects which use either regular Makefiles or CMake Makefiles
    (setq tasks (append tasks
                        (taskrunner-get-all-tasks-file "\".*(M|m|GNUm)akefile$\"" DIR
                                                       (lambda (FILE)
                                                         (taskrunner-get-make-targets FILE
                                                                                      taskrunner-retrieve-all-make-targets))
                                                       '("[Mm]akefile"))))

    ;; These are searched for in project root only
    (if (or (member "gradlew" work-dir-files)
            (member "gradlew.bat" work-dir-files)
            (member "build.gradle" work-dir-files))
        (setq tasks (append tasks (taskrunner-get-gradle-tasks DIR))))

    (if (member "build.xml" work-dir-files)
        (setq tasks (append tasks (taskrunner-get-ant-tasks DIR))))

    ;; Static targets. These will never change and are hardcoded
    (if (member "Cargo.toml" work-dir-files)
        (setq tasks (append tasks taskrunner--rust-targets)))

    (if (taskrunner-locate-filename "\".*go\.(mod|sum)$\"" DIR '("go\.mod" "go\.sum"))
        (setq tasks (append tasks taskrunner--golang-targets)))

    (if (member "Cask" work-dir-files)
        (setq tasks (append tasks taskrunner--cask-targets)))

    (if (member "stack.yaml" work-dir-files)
        (setq tasks (append tasks taskrunner--stack-targets)))

    ;; Cabal can have multiple files but they all end with .cabal extension
    (if (taskrunner-locate-filename "\".*\.cabal$\"" DIR '("\.cabal"))
        (setq tasks (append taskrunner--cabal-targets)))

    ;;; Return the tasks collected
    tasks))

(provide 'taskrunner-locate)
;;; taskrunner-locate.el ends here
