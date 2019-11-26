;;; taskrunner-leiningen.el --- Provide functions to retrieve clojure tasks via lein -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Yavor Konstantinov

;;;; Commentary:
;; Provide support for Leiningen(Clojure)

;;;; Code:

;;;; Requirements
(require 'cl-lib)

;;;; Variables

(defconst taskrunner-leiningen-task-section-header-regexp
  "Several tasks are available:\n"
  "Regexp used to match the start of the tasks output from leiningen.")

;;;; Functions

;; These are here just to silence the bytecompiler. They are defined in
;; `taskrunner.el' and will be loaded later on but due to these files being
;; required before the function being loaded, a warning is emitted.
(declare-function taskrunner--narrow-to-line "ext:taskrunner")
(declare-function taskrunner--make-task-buff-name "ext:taskrunner")

(defun taskrunner--get-leiningen-tasks-from-buffer ()
  "Retrieve all leiningen tasks from the current buffer."
  (let ((beg (search-forward-regexp "Several tasks are available:\n" nil t)))
    (when beg
      (narrow-to-region beg
                        (search-forward-regexp "^$"))
      (cl-map 'list (lambda (elem)
                      (concat "LEIN" " " (car (split-string elem " "))))
              (split-string (buffer-string) "\n")))))

(defun taskrunner-get-leiningen-tasks(DIR)
  "Retrieve the rake tasks for the project in directory DIR.
This function returns a list of the form:
\(\"LEIN TASK1\" \"LEIN TASK2\"...)"
  (let ((default-directory DIR)
        ;;; Store the elixir tasks retrieved
        (lein-tasks))

    (call-process "lein" nil (taskrunner--make-task-buff-name "leiningen") nil "-h")
    (with-temp-buffer
      (set-buffer (taskrunner--make-task-buff-name "leiningen"))
      (goto-char (point-min))
      (setq lein-tasks (taskrunner--get-leiningen-tasks-from-buffer))
      (kill-current-buffer))
    ;;; The last line of the output of Leiningen is always \n\n This results in
    ;;; erroneous output where the last task is of the form: "LEIN "
    ;;; (i.e. its blank) so it must be removed.
    (butlast lein-tasks)))

(provide 'taskrunner-leiningen)
;;; taskrunner-leiningen.el ends here
