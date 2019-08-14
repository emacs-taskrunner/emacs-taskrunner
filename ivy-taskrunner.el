;; Test interface for taskrunner

(require 'ivy)
(require 'taskrunner)

(defvar ivy-taskrunner-extra-actions-list
  '(("a" 'ivy-taskrunner-run-task-with-args "Run task and pass args")
    ("c" 'ivy-taskrunner-run-task-with-args "Run task in current folder without args")
    ("C" 'ivy-taskrunner-run-task-with-args "Run task in current folder with args")
    )
  "A list of extra actions to be used when running a task selected through ivy.")

(defun ivy-taskrunner--run-task-no-args (task)
  "Run the task TASK chosen through the ivy interface."
  (message task))

(defun ivy-taskrunner-run-task-with-args (task)
  "Run the task TASK and ask the user to supply extra arguments."
  (let ((extra-args (read-string "Arguments/Flags to pass to task: ")))
    (if extra-args
        (taskrunner-run-task (concat task " " extra-args))
      (taskrunner-run-task task)
      )
    )
  )

(defun ivy-taskrunner ()
  "Launch ivy to select a task to run in the current project."
  (interactive)
  (let ((in-project-p (projectile-project-p)))
    ;; If we are not in a project, ask the user to switch to one
    (if (not in-project-p)
        ;; If counsel is intalled, use that, otherwise use the default
        ;; projectile-switch-project interface
        (if (package-installed-p 'counsel)
            (setq in-project-p
                  (progn 
                    (require 'counsel)
                    (counsel-projectile-switch-project)))
          (setq in-project-p (projectile-switch-project))))

    ;; Run ivy interface only if the current buffer is in a project, otherwise
    ;; do nothing
    (when in-project-p
      ;; Add extra actions
      (ivy-set-actions
       'ivy-taskrunner
       ivy-taskrunner-extra-actions-list)
      
      ;; Run ivy
      (ivy-read "Task to run: "
                (taskrunner-get-tasks-from-cache)
                :require-match t
                :action 'ivy-taskrunner--main-action))
    )
  )
