;;; taskrunner-ruby.el --- Provide functions to retrieve ruby tasks via Rake -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Yavor Konstantinov

;;;; Commentary:
;; Provide support for Rake(Ruby)

;;;; Code:
(require 'cl-lib)

;;;; Functions

;; These are here just to silence the bytecompiler. They are defined in
;; `taskrunner.el' and will be loaded later on but due to these files being
;; required before the function being loaded, a warning is emitted.
(declare-function taskrunner--narrow-to-line "ext:taskrunner")
(declare-function taskrunner--make-task-buff-name "ext:taskrunner")

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
    (kill-current-buffer)
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
