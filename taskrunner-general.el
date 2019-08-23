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

(defcustom taskrunner-doit-bin-path "~/.local/bin/"
  "Path used to locate the `doit' taskrunner binary."
  :group 'taskrunner
  :type 'string)

(defcustom taskrunner-go-task-buffer-name "*taskrunner-go-task-tasks*"
  "Temporary buffer name used to collect all targets for go task.
The process output of the command `task -l' is loaded in here."
  :group 'taskrunner
  :type 'string)

(defcustom taskrunner-mage-task-buffer-name "*taskrunner-mage-tasks*"
  "Temporary buffer name used to collect all targets for mage.
The process output of the command `mage -l' is loaded in here."
  :group 'taskrunner
  :type 'string)

(defcustom taskrunner-doit-task-buffer-name "*taskrunner-doit-tasks*"
  "Temporary buffer name used to collect all targets for doit.
The process output of the command `doit list' is loaded in here."
  :group 'taskrunner
  :type 'string)

(defcustom taskrunner-just-task-buffer-name "*taskrunner-just-task-tasks*"
  "Temporary buffer name used to collect all targets for just.
The process output of the command `just --list' is loaded in here."
  :group 'taskrunner
  :type 'string)

(defcustom taskrunner-mask-task-buffer-name "*taskrunner-mask-tasks*"
  "Temporary buffer name used to collect all targets for mask.
The process output of the command `mast --help' is loaded in here."
  :group 'taskrunner
  :type 'string)

(defcustom taskrunner-cargo-make-task-buffer-name "*taskrunner-cargo-make-tasks*"
  "Temporary buffer name used to collect all targets for cargo-make.
The process output of the command `cargo make --list-all-steps'
is loaded in here."
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

(defun taskrunner--get-doit-tasks-from-buffer ()
  "Retrieve all doit tasks from the current buffer."
  (goto-char (point-min))
  (let ((targets '()))
    (dolist (elem (split-string (buffer-string) "\n"))
      (push (concat "DOIT" " "(car (split-string elem " "))) targets)
      )
    (kill-current-buffer)
    ;; Remove the first target since it is null due to double newlines at the
    ;; end of buffer
    (if (not (null targets))
        (pop targets))
    targets))

(defun taskrunner-get-doit-tasks (DIR)
  "Retrieve the mage tasks for the project in directory DIR.
This function returns a list of the form:
\(\"DOIT TASK1\" \"DOIT TASK2\"...)"
  (let ((default-directory DIR)
        (exec-path (cons taskrunner-doit-bin-path exec-path)))
    (call-process "doit" nil taskrunner-doit-task-buffer-name nil "list")
    (with-temp-buffer
      (set-buffer taskrunner-doit-task-buffer-name)
      (taskrunner--get-doit-tasks-from-buffer))))


(defun taskrunner--get-just-tasks-from-buffer ()
  "Retrieve all just tasks from the current buffer."
  (goto-char (point-min))
  (let ((targets '()))
    (when (search-forward-regexp "Available recipes:\n" nil t)
      (narrow-to-region (point-at-bol) (point-max))
      (setq targets (map 'list (lambda (elem)
                                 (concat "JUST" " " (car (split-string elem " " t))))
                         (split-string (buffer-string) "\n")))
      (kill-current-buffer)
      (if targets
          (butlast targets)
        nil))))

(defun taskrunner-get-just-tasks (DIR)
  "Retrieve the mage tasks for the project in directory DIR.
This function returns a list of the form:
\(\"JUST TASK1\" \"JUST TASK2\"...)"
  (let ((default-directory DIR))
    (call-process "just" nil taskrunner-just-task-buffer-name nil "--list")
    (with-temp-buffer
      (set-buffer taskrunner-just-task-buffer-name)
      (taskrunner--get-just-tasks-from-buffer))))

(defun taskrunner--get-mask-tasks-from-buffer ()
  "Retrieve all mask tasks from the current buffer."
  (goto-char (point-min))
  (let ((targets '()))
    (when (search-forward-regexp "SUBCOMMANDS:\n" nil t)
      (narrow-to-region (point-at-bol) (point-max))
      (setq targets (map 'list
                         (lambda (elem)
                           (concat "MASK" " " (car (split-string elem " " t))))
                         (split-string (buffer-string) "\n")))
      (kill-current-buffer)
      (butlast targets))))

(defun taskrunner-get-mask-tasks (DIR)
  "Retrieve the mage tasks for the project in directory DIR.
This function returns a list of the form:
\(\"MASK TASK1\" \"MASK TASK2\"...)"
  (let ((default-directory DIR))
    (call-process "mask" nil taskrunner-mask-task-buffer-name nil "--help")
    (with-temp-buffer
      (set-buffer taskrunner-just-mask-buffer-name)
      (taskrunner--get-mask-tasks-from-buffer))))

(defun taskrunner--get-cargo-make-tasks-from-buffer ()
  "Retrieve all cargo-make tasks from the current buffer."
  (goto-char (point-min))
  (let ((targets '())
        (beg)
        (end))
    (while (search-forward-regexp "^-+\n" nil t)
      (setq beg (point-at-bol))
      (search-forward-regexp "^$" nil t)
      (forward-line -1)
      (setq end (point-at-eol))
      (narrow-to-region beg end)
      (dolist (el (split-string (buffer-string) "\n"))
        (push (concat "CARGO-MAKE" " " (car (split-string el " "))) targets))
      (widen))
    (kill-current-buffer)
    targets))

(defun taskrunner-get-cargo-make-tasks (DIR)
  "Retrieve the mage tasks for the project in directory DIR.
This function returns a list of the form:
\(\"CARGO-MAKE TASK1\" \"CARGO-MAKE TASK2\"...)"
  (let ((default-directory DIR))
    (call-process "cargo" nil taskrunner-cargo-make-task-buffer-name nil "make" "--list-all-steps")
    (with-temp-buffer
      (set-buffer taskrunner-cargo-make-task-buffer-name)
      (taskrunner--get-cargo-make-tasks-from-buffer))))

(provide 'taskrunner-general)
;;; taskrunner-general.el ends here
