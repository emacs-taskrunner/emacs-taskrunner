;;; taskrunner-general.el --- Provide functions to access general taskrunners not tied to any language -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Yavor Konstantinov

;;; Commentary:
;; Support included for:
;; Golang's Task

;;; Code:

;;;; Required
(require 'projectile)

;;;; Variables
(defcustom go-task-bin-path "~/go/bin/"
  "Path used to locate the `task' golang binary."
  :group 'taskrunner
  :type 'string)

(defcustom go-task-buffer-name "*taskrunner-go-task*"
  "Temporary buffer name used to collect all targets for go task.
The process output of the command `task -l' is loaded in here."
  :group 'taskrunner
  :type 'string)

;;;; Functions
(defun taskrunner--get-go-tasks-from-buffer ()
  "Retrieve all go tasks from the currently visited buffer.
The tasks are returned in the form:
\(\"TASK TASK-NAME\" ...)"
  (interactive)
  (let ((targets '()))
    (goto-char (point-min))
    (while (search-forward-regexp "^\*" nil t)
      (taskrunner--narrow-to-line)
      (push (car (split-string (cadr (split-string (buffer-string) " ")) ":")) targets)
      (widen))
    (kill-current-buffer)
    (if targets
        (map 'list (lambda (elem)
                     (concat "TASK" " " elem))
             targets)
      targets)))

(defun taskrunner-get-go-task-tasks (DIR)
  "Retrieve the golang Task tasks for the project in directory DIR.
This function returns a list of the form:
\(\"TASK TASK1\" \"TASK TASK2\"...)"
  (let ((default-directory DIR)
        (exec-path (cons go-task-bin-path exec-path)))
    (call-process "task" nil go-task-buffer-name nil "-l")
    (with-temp-buffer
      (set-buffer go-task-buffer-name)
      (taskrunner--get-go-tasks-from-buffer))))

(provide taskrunner-general)
;;; taskrunner-general.el ends here
