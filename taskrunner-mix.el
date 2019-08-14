;; Retrieve tasks for mix(Elixir)


(defcustom taskrunner-mix-buffer-name "*taskrunner-elixir-tasks*"
  "Name of the buffer temporarily created to be used for retrieving mix tasks."
  :group 'taskrunner)

(defun taskrunner--get-elixir-tasks-from-buffer ()
  "Retrieve all mix tasks from the currently visited buffer."
  (map 'list (lambda (elem)
               (let ((split-task-line (split-string elem " ")))
                 (cond
                  ;; Match the last line of mix 'iex -S mix'
                  ((string-match "^iex" elem)
                   (concat "MIX" " " (car (split-string elem "#"))))
                  ;; Match the first line 'mix #some_comment'
                  ;; This is presented as MIX mix in the output
                  ((string-match "^mix[[:space:]]+#" elem)
                   (concat "MIX" " " "mix"))
                  ;; Match all other regular lines
                  (t
                   (concat "MIX" " " (cadr split-task-line)))
                  )
                 )
               )
       (split-string (buffer-string) "\n"))
  )


(defun taskrunner--start-elixir-task-process (dir)
  "Start the mix help process for project in DIR and retrieve mix tasks."
  (let ((default-directory dir)
        (buff (get-buffer-create taskrunner-mix-buffer-name))
        (elixir-tasks) ;; Store the elixir tasks retrieved
        )
    (call-process "mix" nil taskrunner-mix-buffer-name nil "help")
    (with-temp-buffer
      (set-buffer buff)
      (goto-line 1)
      ;; Use regexp to find start of region since 'mix help' can sometimes show
      ;; errors in the output and we do not want to match those
      (search-forward-regexp "^mix[[:space:]]+" nil t)
      (narrow-to-region (progn
                          (beginning-of-line)
                          (point))
                        ;; Find last line
                        (progn
                          (goto-char (point-max))
                          (beginning-of-line)
                          (point)))
      (setq elixir-tasks (taskrunner--get-elixir-tasks-from-buffer))
      (kill-current-buffer)
      )
    ;; Return the tasks explicitly. Cannot do it any other way since the buffer
    ;; used must be killed before returning from this function
    elixir-tasks
    )
  )

(provide 'taskrunner-mix)
