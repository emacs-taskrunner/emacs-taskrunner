;;; taskrunner-ruby.el --- Provide functions to retrieve ruby tasks via Rake -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Yavor Konstantinov

;;;; Commentary:
;; Provide support for Rake(Ruby)

;;;; Code:
(require 'cl-lib)
(require 'taskrunner)

;;;; Functions

(defun taskrunner--retrieve-rake-tasks-from-buffer ()
  "Retrieve the rake tasks from the current buffer.
This function returns a list of the form:
\(\"RAKE TASK1\" \"RAKE TASK2\"...)"
  (goto-char (point-min))
  (let ((rake-tasks '()))
    (while (search-forward-regexp "^rake[[:space:]]+" nil t)
      (taskrunner--narrow-to-line)
      (push (concat "RAKE" " " (cadr (split-string (buffer-string) " " t))) rake-tasks)
      (widen))
    rake-tasks))


(defun taskrunner-get-rake-tasks (DIR)
  "Retrieve the ant tasks for the project in directory DIR.
This function returns a list of the form:
\(\"RAKE TASK1\" \"RAKE TASK2\"...)"
  (let ((default-directory DIR))
    (call-process "rake"  nil (taskrunner--make-task-buff-name "rake")  nil "-AT")
    (with-temp-buffer
      (set-buffer (taskrunner--make-task-buff-name "rake"))
      (taskrunner--retrieve-rake-tasks-from-buffer))))

(provide 'taskrunner-ruby)
;;; taskrunner-ruby.el ends here
