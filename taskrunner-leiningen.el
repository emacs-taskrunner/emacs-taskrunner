;; Leiningen task retrieval

(defcustom taskrunner-leiningen-buffer-name "*taskrunner-leiningen-tasks*"
  "Name of the buffer temporarily created to be used for retrieving leiningen tasks."
  :group 'taskrunner)

(defcustom taskrunner-leiningen-task-section-header-regexp
  "Several tasks are available:\n"
  "Regexp used to match the header of the task section in the leiningen help
command. This is used to retrieve all of the tasks.")

(defun taskrunner--get-leiningen-tasks-from-buffer ()
  "Retrieve all leiningen tasks from the current buffer."
  (let ((beg (search-forward-regexp "Several tasks are available:\n" nil t)))
    (when beg
      (narrow-to-region beg
                        (search-forward-regexp "^$"))
      (map 'list (lambda (elem)
                   (concat "LEIN" " " (car (split-string elem " "))))
           (split-string (buffer-string) "\n"))
      )
    )
  )

(defun taskrunner--start-leiningen-task-process (dir)
  "Start the mix help process for project in DIR and retrieve mix tasks."
  (let ((default-directory dir)
        (buff (get-buffer-create taskrunner-leiningen-buffer-name))
        (lein-tasks) ;; Store the elixir tasks retrieved
        )
    (call-process "lein" nil taskrunner-leiningen-buffer-name nil "-h")
    (with-temp-buffer
      (set-buffer buff)
      (goto-line 1)
      (setq lein-tasks (taskrunner--get-leiningen-tasks-from-buffer))
      (kill-current-buffer)
      )
    ;; Return the tasks explicitly. Cannot do it any other way since the buffer
    ;; used must be killed before returning from this function
    (butlast lein-tasks)
    )
  )

(provide 'taskrunner-leiningen)
