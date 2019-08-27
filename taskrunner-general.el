;;; taskrunner-general.el --- Provide functions to access general taskrunners not tied to any language -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Yavor Konstantinov

;;; Commentary:
;; This file provides support for general taskrunners which are not tied to
;; a specific language.
;; Support included for:
;; Golang's Task
;; Mage
;; cargo-make
;; mask
;; just
;; doit

;;; Code:

;;;; Required
(require 'projectile)
(require 'cl-lib)

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

(defcustom taskrunner-tusk-bin-path "~/clones/tusk-test/"
  "Path to the Tusk taskrunner binary."
  :group 'taskrunner
  :type 'string)

(defcustom taskrunner-dobi-bin-path "~/"
  "Path to the folder containing the Dobi taskrunner binary."
  :group 'taskrunner
  :type 'string)

(defcustom taskrunner-dobi-bin-name "dobi-linux"
  "Name of the Dobi taskrunner binary."
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
        (cl-map 'list (lambda (elem)
                        (concat "TASK" " " elem))
                targets)
      targets)))

(defun taskrunner-get-go-task-tasks (DIR)
  "Retrieve the golang Task tasks for the project in directory DIR.
This function returns a list of the form:
\(\"TASK TASK1\" \"TASK TASK2\"...)"
  (let ((default-directory DIR)
        (exec-path (cons taskrunner-go-task-bin-path exec-path)))
    (call-process "task" nil (taskrunner--make-task-buff-name "go-task") nil "-l")
    (with-temp-buffer
      (set-buffer (taskrunner--make-task-buff-name "go-task"))
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
    (cl-map 'list (lambda (elem)
                    (concat "MAGE" " " elem))
            targets)))

(defun taskrunner-get-mage-tasks (DIR)
  "Retrieve the mage tasks for the project in directory DIR.
This function returns a list of the form:
\(\"MAGE TASK1\" \"MAGE TASK2\"...)"
  (let ((default-directory DIR)
        (exec-path (cons taskrunner-mage-bin-path exec-path)))
    (call-process "mage" nil (taskrunner--make-task-buff-name "mage") nil "-l")
    (with-temp-buffer
      (set-buffer (taskrunner--make-task-buff-name "mage"))
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
    (call-process "doit" nil (taskrunner--make-task-buff-name "doit") nil "list")
    (with-temp-buffer
      (set-buffer (taskrunner--make-task-buff-name "doit"))
      (taskrunner--get-doit-tasks-from-buffer))))


(defun taskrunner--get-just-tasks-from-buffer ()
  "Retrieve all just tasks from the current buffer."
  (goto-char (point-min))
  (let ((targets '()))
    (when (search-forward-regexp "Available recipes:\n" nil t)
      (narrow-to-region (point-at-bol) (point-max))
      (setq targets (cl-map 'list (lambda (elem)
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
    (call-process "just" nil (taskrunner--make-task-buff-name "just") nil "--list")
    (with-temp-buffer
      (set-buffer (taskrunner--make-task-buff-name "just"))
      (taskrunner--get-just-tasks-from-buffer))))

(defun taskrunner--get-mask-tasks-from-buffer ()
  "Retrieve all mask tasks from the current buffer."
  (goto-char (point-min))
  (let ((targets '()))
    (when (search-forward-regexp "SUBCOMMANDS:\n" nil t)
      (narrow-to-region (point-at-bol) (point-max))
      (setq targets (cl-map 'list
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
    (call-process "mask" nil (taskrunner--make-task-buff-name "mask") nil "--help")
    (with-temp-buffer
      (set-buffer (taskrunner--make-task-buff-name "mask"))
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
    (call-process "cargo" nil (taskrunner--make-task-buff-name "cargo-make") nil "make" "--list-all-steps")
    (with-temp-buffer
      (set-buffer (taskrunner--make-task-buff-name "cargo-make"))
      (taskrunner--get-cargo-make-tasks-from-buffer))))

(defun taskrunner--get-tusk-tasks-from-buffer ()
  "Retrieve all tusk tasks from buffer if any exist."
  (goto-char (point-min))
  (let ((beg (search-forward-regexp "^Tasks:" nil t))
        (end)
        (tasks '()))
    (when beg
      (setq beg
            (progn
              (forward-line 1)
              (point-at-bol)))
      (setq end (progn
                  (search-forward-regexp "^$" nil t)
                  (point-at-bol)))
      (narrow-to-region beg end)
      (dolist (elem (split-string (buffer-string) "\n"))
        (push (concat "TUSK" " " (car (split-string elem " " t))) tasks)))
    (kill-current-buffer)
    ;; The last line read is a blank one. This removes the blank task if any
    ;; have been collected
    (when tasks
      (pop tasks))
    tasks))

(defun taskrunner-get-tusk-tasks (DIR)
  "Retrieve the Tusk tasks for the project in directory DIR.
This function returns a list of the form:
\(\"TUSK TASK1\" \"TUSK TASK2\"...)"
  (let ((default-directory DIR)
        (exec-path (cons taskrunner-tusk-bin-path exec-path)))
    (call-process "tusk" nil (taskrunner--make-task-buff-name "tusk") nil "--help")
    (with-temp-buffer
      (set-buffer (taskrunner--make-task-buff-name "tusk"))
      (taskrunner--get-tusk-tasks-from-buffer))))

(defun taskrunner--get-buidler-tasks-from-buffer ()
  "Retrieve all tasks from the buidler if any are available."
  (goto-char (point-min))
  (let ((beg (search-forward-regexp "^AVAILABLE TASKS:\n\n" nil t))
        (end)
        (tasks '()))
    (when beg
      (setq beg (point-at-bol))
      (setq end
            (progn
              (search-forward-regexp "^$" nil t)
              (point-at-bol)))
      (narrow-to-region beg end)
      (dolist (line (split-string (buffer-string) "\n"))
        (push (concat "BUIDLER" " " (car (split-string line split-string-default-separators t))) tasks)))
    (kill-current-buffer)
    ;; The last line read is a blank one. This removes the blank task if any
    ;; have been collected
    (when tasks
      (pop tasks))
    tasks))

(defun taskrunner-get-buidler-tasks (DIR)
  "Retrieve the Buidler tasks for the project in directory DIR.
This function returns a list of the form:
\(\"BUIDLER TASK1\" \"BUIDLER TASK2\"...)

This function assumes that you have `npx' installed."
  (let ((default-directory DIR))
    (call-process "npx" nil (taskrunner--make-task-buff-name "buidler") nil "buidler" "--help")
    (with-temp-buffer
      (set-buffer (taskrunner--make-task-buff-name "buidler"))
      (taskrunner--get-buidler-tasks-from-buffer))))

(defun taskrunner--get-dobi-tasks-from-buffer ()
  "Retrieve all dobi tasks from buffer if any are available."
  (goto-char (point-min))
  (let ((beg (search-forward-regexp "Resources:\n" nil t))
        (tasks '()))
    (when beg
      (narrow-to-region (point-at-bol) (point-max))
      (dolist (line (split-string (buffer-string) "\n"))
        (push (concat "DOBI" " " (car (split-string line split-string-default-separators t))) tasks)))
    ;; The last line read is a blank one. This removes the blank task if any
    ;; have been collected
    (when tasks
      (pop tasks))
    (kill-current-buffer)
    tasks))

(defun taskrunner-get-dobi-tasks (DIR)
  "Retrieve the dobi tasks for the project in directory DIR.
This function returns a list of the form:
\(\"DOBI TASK1\" \"DOBI TASK2\"...)"
  (let ((default-directory DIR)
        (exec-path (cons taskrunner-dobi-bin-path2 exec-path)))
    (call-process taskruner-dobi-bin-name nil (taskrunner--make-task-buff-name "dobi") nil "list")
    (with-temp-buffer
      (set-buffer (taskrunner--make-task-buff-name "dobi"))
      (taskrunner--get-dobi-tasks-from-buffer))))

(provide 'taskrunner-general)
;;; taskrunner-general.el ends here
