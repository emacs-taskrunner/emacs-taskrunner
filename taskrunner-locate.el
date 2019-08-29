;;; package --- Summary
;;; Commentary:
;;; Code:

(provide 'taskrunner-locate)
;;; taskrunner-locate.el ends here

(defcustom taskrunner-rg-bin-path (executable-find "rg")
  "Path to ripgrep(rg) binary.")

(defcustom taskrunner-ag-bin-path (executable-find "ag")
  "Path to the silver searcher(ag) binary.")

(defcustom taskrunner-grep-bin-path (executable-find "grep")
  "Path to the grep binary.")

(defconst taskrunner--rg-filename-command "rg --files -g "
  "Command line arguments for ripgrep.")

(defconst taskrunner--ag-filename-command "ag -g "
  "Command line arguments for silver searcher(ag).")

(defmacro taskrunner-locate-filename (FILENAME-REGEXP ROOT)
  "Look for files whose names match FILENAME-REGEXP in directory ROOT."
  `(let ((search-output))
     (cond
      ((file-executable-p taskrunner-rg-bin-path)
       (message "Using RG")
       ;; It seems like all wildcard searches for ripgrep using --files -g have
       ;; to surround the regexp with *
       (split-string (shell-command-to-string
                      (concat taskrunner--rg-filename-command " *"  ,FILENAME-REGEXP "* " ,ROOT )) "\n" t)
       )
      ((file-executable-p taskrunner-ag-bin-path)
       (message "Using AG")
       (split-string (shell-command-to-string
                      (concat taskrunner--ag-filename-command " "  ,FILENAME-REGEXP " " ,ROOT )) "\n" t)
       )
      ((file-executable-p taskrunner-grep-bin-path))
      (t
       (message "Finish this as a fallback!")))
     )
  )

(defun taskrunner-get-all-tasks (FILENAME-REGEXP ROOT FUNC)
  "Find all file names matching FILENAME-REGEXP in project with dir at ROOT.
Call FUNC for each of the filepaths for each file."
  (let ((search-result (taskrunner-locate-filename FILENAME-REGEXP ROOT))
        (tasks-found '()))
    (when search-result
      (dolist (file-path search-result)
        (if (file-exists-p file-path)
            ;; (message "H: %s" (funcall FUNC (file-name-directory file-path)))
            (setq tasks-found (append tasks-found (funcall FUNC (file-name-directory file-path))))
          )
        )
      )
    tasks-found
    )
  )

(taskrunner-get-all-tasks "[Mm]akefile$" "~/clones/esqulino/" '(lambda (filepath)
                                                                 (taskrunner-get-make-targets filepath "Makefile" t))
                          )

