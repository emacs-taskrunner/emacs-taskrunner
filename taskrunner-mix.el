;; Retrieve tasks for mix(Elixir)


(defun taskrunner--get-elixir-tasks-from-buffer ()
  "Retrieve all mix tasks from the currently visited buffer."
  (map 'list (lambda (elem)
               (let ((split-task-line (split-string elem " ")))
                 (cond
                  ((string-match "^iex" elem)
                   (concat "MIX" " " (car (split-string elem "#"))))
                  ((string-match "^mix[[:space:]]+#" elem)
                   (concat "MIX" " " "mix"))
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
        (buff (get-buffer-create "*taskrunner-elixir-tasks*"))
        (elixir-tasks )
        )
    (call-process "mix"  nil "*taskrunner-elixir-tasks*"  nil "help")
    (with-temp-buffer
      (set-buffer buff)
      (goto-line 1)
      (search-forward-regexp "^mix[[:space:]]+" nil t)
      (narrow-to-region (progn
                          (beginning-of-line)
                          (point))
                        (progn
                          (goto-char (point-max))
                          (beginning-of-line)
                          (point)))
      (setq elixir-tasks (taskrunner--get-elixir-tasks-from-buffer))
      (kill-current-buffer)
      )
    elixir-tasks
    )
  )
