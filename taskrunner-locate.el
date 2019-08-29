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

(defsubst taskrunner-locate-filename (FILENAME-REGEXP ROOT &optional RG-GLOB)
  "Look for files whose names match FILENAME-REGEXP in directory ROOT.
RG-GLOB"
  (let ((search-output))
    (cond
     ((and (file-executable-p taskrunner-rg-bin-path)
           RG-GLOB)
      (message "Using RG")
      ;; It seems like all wildcard searches for ripgrep using --files -g have
      ;; to surround the regexp with *
      (split-string (shell-command-to-string
                     (concat taskrunner--rg-filename-command " *"  FILENAME-REGEXP "* " ROOT )) "\n" t)
      )
     ((file-executable-p taskrunner-ag-bin-path)
      (message "Using AG")
      (split-string (shell-command-to-string
                     (concat taskrunner--ag-filename-command " "  FILENAME-REGEXP " " ROOT )) "\n" t)
      )
     ((file-executable-p taskrunner-grep-bin-path)
      (message "Using find")
      (split-string (shell-command-to-string
                     (concat "find " ROOT " -regextype posix-egrep -regex " FILENAME-REGEXP)) "\n" t))
     (t
      (message "Finish this as a fallback!"))
     )
    )
  )


(defun taskrunner-get-all-tasks-dir (FILENAME-REGEXP ROOT FUNC &optional RG-GLOB)
  "Find all file names matching FILENAME-REGEXP in project with dir at ROOT.
Call FUNC for each of the filepaths for each file.
RG-GLOB"
  (let ((search-result (taskrunner-locate-filename FILENAME-REGEXP ROOT RG-GLOB))
        (tasks-found '()))
    (when search-result
      (dolist (file-path search-result)
        (when (file-exists-p file-path)
          (message "FILE EXISTS %s" file-path)
          (setq tasks-found (append tasks-found (funcall FUNC (file-name-directory file-path))))
          ;; (append tasks-found (funcall FUNC (file-name-directory file-path)))
          )
        )
      )
    tasks-found
    )
  )

(defun taskrunner-get-all-tasks-file (FILENAME-REGEXP ROOT FUNC &optional RG-GLOB)
  "Find all file names matching FILENAME-REGEXP in project with dir at ROOT.
Call FUNC for each of the filepaths for each file.
RG-GLOB"
  (let ((search-result (taskrunner-locate-filename FILENAME-REGEXP ROOT RG-GLOB))
        (tasks-found '()))
    (when search-result
      (dolist (file-path search-result)
        (when (file-exists-p file-path)
          (message "FILE EXISTS %s" file-path)
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
                        (taskrunner-get-all-tasks "package\.json$" DIR 'taskrunner-get-package-json-tasks)))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "[Gg]ulpfile\.js$" DIR 'taskrunner-get-package-json-tasks)))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*Gruntfile\.(js|coffee)$\"" DIR 'taskrunner-get-grunt-tasks)))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*Jakefile(\.js|\.coffee)?$\"" DIR 'taskrunner-get-grunt-tasks)))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\"[Rr]akefile(\.rb)?$\"" DIR 'taskrunner-get-grunt-tasks)))


    (if (member "build.xml" work-dir-files)
        (setq tasks (append tasks (taskrunner-get-ant-tasks DIR))))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*mix\.exs$\"" DIR 'taskrunner-get-grunt-tasks)))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*project\.clj$\"" DIR 'taskrunner-get-grunt-tasks)))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*Taskfile\.yml$\"" DIR 'taskrunner-get-grunt-tasks)))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*dodo\.py$\"" DIR 'taskrunner-get-grunt-tasks)))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*magefile\.go$\"" DIR 'taskrunner-get-grunt-tasks)))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*maskfile\.md$\"" DIR 'taskrunner-get-grunt-tasks)))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*tusk\.yml$\"" DIR 'taskrunner-get-grunt-tasks)))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*buidler\.config\.js$\"" DIR 'taskrunner-get-grunt-tasks)))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*dobi\.yml$\"" DIR 'taskrunner-get-grunt-tasks)))

    ;; (if (or (member "justfile" work-dir-files)
    ;;         (member "Justfile" work-dir-files)
    ;;         (member "JUSTFILE" work-dir-files))
    ;;     (setq tasks (append tasks (taskrunner-get-just-tasks DIR))))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks "\".*\.yml$\"" DIR 'taskrunner-get-grunt-tasks)))

    ;; (cond ((member "CMakeLists.txt" work-dir-files)
    ;;        (setq tasks (append tasks (taskrunner-get-cmake-tasks DIR))))
    ;;       ((taskrunner-file-in-source-folder-p DIR work-dir-files "CMakeLists.txt")
    ;;        (setq tasks (append tasks (taskrunner-get-cmake-tasks DIR)))))

    ;; (cond ((member "meson.build" work-dir-files)
    ;;        (setq tasks (append tasks (taskrunner-get-meson-tasks DIR))))
    ;;       ((taskrunner-file-in-source-folder-p DIR work-dir-files "meson.build")
    ;;        (setq tasks (append tasks (taskrunner-get-meson-tasks DIR)))))

    ;; There should only be one makefile in the directory only look for one type
    ;; of name.
    ;; (cond
    ;;  ((member "Makefile" work-dir-files)
    ;;   (setq tasks (append tasks (taskrunner-get-make-targets
    ;;                              DIR "Makefile" taskrunner-retrieve-all-make-targets))))
    ;;  ((member "makefile" work-dir-files)
    ;;   (setq tasks (append tasks (taskrunner-get-make-targets
    ;;                              DIR "makefile" taskrunner-retrieve-all-make-targets))))
    ;;  ((member "GNUmakefile" work-dir-files)
    ;;   (setq tasks (append tasks (taskrunner-get-make-targets
    ;;                              DIR "GNUmakefile" taskrunner-retrieve-all-make-targets)))))

    (setq tasks (append tasks
                        (taskrunner-get-all-tasks-file "\".*(M|m|GNUm)akefile$\"" DIR
                                                       (lambda (FILE)
                                                         (taskrunner-get-make-targets FILE
                                                                                      taskrunner-retrieve-all-make-targets)))))

    ;; Keep this at the project root. Usually, gradle files are not nested
    (if (or (member "gradlew" work-dir-files)
            (member "gradlew.bat" work-dir-files)
            (member "build.gradle" work-dir-files))
        (setq tasks (append tasks (taskrunner-get-gradle-tasks DIR))))

    ;; Static targets. These will never change and are hardcoded
    ;; These should also always be found at the root
    (if (member "Cargo.toml" work-dir-files)
        (setq tasks (append tasks taskrunner--rust-targets)))

    (if (or (member "go.mod" work-dir-files)
            (member "go.sum" work-dir-files))
        (setq tasks (append tasks taskrunner--golang-targets)))

    (if (member "Cask" work-dir-files)
        (setq tasks (append tasks taskrunner--cask-targets)))

    (if (member "stack.yaml" work-dir-files)
        (setq tasks (append tasks taskrunner--stack-targets)))

    ;; Cabal can have multiple files but they all end with .cabal extension
    (if (taskrunner-locate-filename "\.cabal$" DIR)
        (setq tasks (append taskrunner--cabal-targets)))

    ;;; Return the tasks collected
    tasks))

;; (taskrunner-get-all-tasks "[Mm]akefile$" "~/clones/esqulino/" '(lambda (filepath)
;;                                                                  (taskrunner-get-make-targets filepath "Makefile" t)))
;; (message "%s"
;;          (taskrunner-get-all-tasks-file "\".*(M|m|GNUm)akefile$\"" "~/clones/esqulino/"
;;                                         (lambda (FILE)
;;                                           (taskrunner-get-make-targets FILE
;;                                                                        taskrunner-retrieve-all-make-targets))))

;; (message "%s" (taskrunner-get-all-tasks "package\.json$" "~/clones/vscode-node-debug/" 'taskrunner-get-package-json-tasks))
;; (message "REX %s" (taskrunner-get-all-tasks "Gruntfile\.js$" "~/clones/grunt-demo/" 'taskrunner-get-grunt-tasks))

;; (taskrunner-locate-filename "package\.json$" "~/clones/vscode-node-debug")
;; (taskrunner-locate-filename "Gruntfile\.js$" "~/clones/vscode-node-debug")

;; (taskrunner-locate-filename "Cargo\.toml" "~/clones/bat")
;; (message "tASKS %s" (taskrunner-collect-tasks-full-search "~/clones/grunt-demo/"))
;; (message "%s" (taskrunner-collect-tasks-full-search "~/linux/"))

;; (taskrunner-locate-filename "\".*Gruntfile\.(js|coffee)$\"" "~/clones/regex-test/")
;; (taskrunner-locate-filename "\".*Jakefile(\.js|\.coffee)?$\"" "~/clones/regex-test/")
;; (taskrunner-locate-filename "\".*(M|m|GNUm)akefile$\"" "~/clones/regex-test/")
;; (taskrunner-locate-filename "\".*(M|m|GNUm)akefile$\"" "~/clones/esqulino/")

(provide 'taskrunner-locate)
;;; taskrunner-locate.el ends here
