;; Test interface for taskrunner

(require 'ivy)
(require 'taskrunner)

(defun ivy-taskrunner--main-action (task)
  "Run the task TASK chosen through the ivy interface."
  (message task))

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
      (ivy-read "Task to run: "
                (taskrunner-get-tasks-from-cache)
                :require-match t
                :action 'ivy-taskrunner--main-action))
    )
  )
