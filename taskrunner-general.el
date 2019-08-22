;;; taskrunner-general.el --- Provide functions to access general taskrunners not tied to any language -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Yavor Konstantinov

;;; Commentary:
;; Support included for:
;; Golang's Task

;;; Code:

;;;; Required
(require 'projectile)

;;;; Variables
(defcustom taskrunner-go-task-bin-path "~/go/bin/"
  "Path used to locate the `task' taskrunner binary."
  :group 'taskrunner
  :type 'string)

(defcustom taskrunner-mage-bin-path "~/go/bin/"
  "Path used to locate the `mage' taskrunner binary."
  :group 'taskrunner
  :type 'string)

(defcustom taskrunner-go-task-buffer-name "*taskrunner-go-task*"
  "Temporary buffer name used to collect all targets for go task.
The process output of the command `task -l' is loaded in here."
  :group 'taskrunner
  :type 'string)

(defcustom taskrunner-mage-task-buffer-name "*taskrunner-mage*"
  "Temporary buffer name used to collect all targets for mage.
The process output of the command `mage -l' is loaded in here."
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
        (exec-path (cons taskrunner-go-task-bin-path exec-path)))
    (call-process "task" nil taskrunner-go-task-buffer-name nil "-l")
    (with-temp-buffer
      (set-buffer taskrunner-go-task-buffer-name)
      (taskrunner--get-go-tasks-from-buffer))))

(defun taskrunner--get-mage-tasks-from-buffer ()
  "Retrieve all mage tasks from the currently visited buffer."
  (let ((targets '())
        (beg nil))
    (goto-char (point-min))
    (setq beg (search-forward-regexp "Targets:\n" nil t))
    (when beg
      (narrow-to-region (point-at-bol) (point-max))
      (dolist (elem (split-string (buffer-string) "\n"))
        (push (car (split-string elem " " t)) targets)
        (if (null (car targets))
            (pop targets))))
    (kill-current-buffer)
    (map 'list (lambda (elem)
                 (concat "MAGE" " " elem))
         targets)))

(defun taskrunner-get-mage-tasks (DIR)
  "Retrieve the mage tasks for the project in directory DIR.
This function returns a list of the form:
\(\"MAGE TASK1\" \"MAGE TASK2\"...)"
  (let ((default-directory DIR)
        (exec-path (cons taskrunner-mage-bin-path exec-path)))
    (call-process "mage" nil taskrunner-mage-task-buffer-name nil "-l")
    (with-temp-buffer
      (set-buffer taskrunner-mage-task-buffer-name)
      (taskrunner--get-mage-tasks-from-buffer))))

(provide 'taskrunner-general)
;;; taskrunner-general.el ends here
