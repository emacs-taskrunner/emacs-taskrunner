;;; taskrunner-java.el --- Provide functions to retrieve java build system targets -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Yavor Konstantinov

;;;; Commentary:
;; Provide function for extracting java build system targets.
;; Support included for:
;; - Ant
;; - Gradle

;;;; Code:

;;;; Variables

(defcustom taskrunner-gradle-heading-regexps
  '("Build tasks\n-+\n"
    "Help tasks\n-+\n"
    "Verification tasks\n-+\n"
    "Documentation tasks\n-+\n"
    "Distribution tasks\n-+\n"
    "Other tasks\n-+\n"
    "Build Setup tasks\n-+\n"
    "IDE tasks tasks\n-+\n"
    )
  "Regular expressions used to locate task section headings from gradle output."
  :type 'list
  :group 'taskrunner)

(defcustom taskrunner-gradle-tasks-buffer-name "*taskrunner-gradle-tasks*"
  "Name of the buffer used for parsing the tasks from the output of gradle."
  :type 'string
  :group 'taskrunner)

(defcustom taskrunner-ant-tasks-buffer-name "*taskrunner-ant-tasks*"
  "Name of the buffer used for parsing tasks from the output of ant."
  :type 'string
  :group 'taskrunner)

;;;; Functions

;; Each block contains several tasks which can be executed.
;; The block has the following form:
;; TASK-HEADING
;; ------------ (underline for the heading)
;; task 1
;; task 2
;; ...
;; task n-1
;; task n
;; BLANK-LINE
;; This function narrows to the region which starts directly after the underline
;; for the heading and ends at the blank line separating each block.
(defun taskrunner--retrieve-gradle-heading-tasks (heading)
  "Retrieve the gradle tasks below the heading HEADING and return as list."
  (widen)
  (goto-char (point-min))
  (let ((beg (re-search-forward heading nil t)))
    (when beg
      (narrow-to-region beg
                        (progn
                          (re-search-forward "^$" nil t)
                          ;; Go up a line so the empty line at the end of the block
                          ;; is not included in the output
                          (forward-line -1)
                          (line-end-position)))

      (map 'list (lambda (elem)
                   (concat "GRADLE" " " (car (split-string elem " "))))
           (split-string (buffer-string) "\n")))))

(defun taskrunner-get-gradle-tasks (dir)
  "Retrieve the gradle tasks for the project in directory DIR.
This function returns a list of the form:
\(\"GRADLE TASK1\" \"GRADLE TASK2\"...)"
  (let ((default-directory dir)
        (buff (get-buffer-create taskrunner-gradle-tasks-buffer-name))
        (gradle-tasks '()))
    (call-process "gradle"  nil taskrunner-gradle-tasks-buffer-name  nil "tasks")
    (with-temp-buffer
      (set-buffer buff)
      (dolist (curr-regex taskrunner-gradle-heading-regexps)
        (let ((tasks-retrieved (taskrunner--retrieve-gradle-heading-tasks curr-regex)))
          (when tasks-retrieved
            (setq gradle-tasks (append gradle-tasks tasks-retrieved))
            )))
      (kill-buffer buff))
    ;; Return the tasks acquired
    gradle-tasks))

;; In general, the output of 'ant -verbose -p' looks like this:
;; some-java-logging-commands...
;; ...
;; Main targets:
;;
;; main_target1 comment
;; ...
;; main_targetN comment
;; Other targets:
;;
;; other_target1
;; ...
;; other_targetN
;; Sometimes the buffer might end with 'Default target:' and sometimes
;; it might not if there is no default target assigned for project
(defun taskrunner--retrieve-ant-tasks-from-buffer ()
  "Retrieve all and tasks from the current buffer.
This function is meant to be used with the output of `ant -verbose -p'.
If you need to retrieve tasks from ant, use the function
`taskrunner-get-ant-tasks' instead of this."
  (goto-char (point-min))
  (let ((beg (search-forward-regexp "Main targets:\n\n" nil t))
        (ant-tasks '()))
    (when beg
      (narrow-to-region (point-at-bol)
                        (progn
                          (search-forward-regexp "Other targets:")
                          (forward-line -1)
                          (point-at-eol)))
      (map 'list (lambda (elem)
                   (if (not (string-equal elem ""))
                       (push (concat "ANT" " "  (car (split-string (string-trim elem) " "))) ant-tasks)
                     ))
           (split-string (buffer-string) "\n"))
      (widen))

    ;; Look for the 'Other targets' section
    (goto-char (point-min))
    (setq beg (search-forward-regexp "Other targets:\n\n" nil t))
    ;; If the section exists, retrieve the tasks in it.
    (when beg
      (narrow-to-region (point-at-bol)
                        ;; If the ant project has a default target then the last
                        ;; line of the output starts with 'Default target'
                        ;; which is not a task which can be ran but information.
                        ;; Otherwise, the last line is a task
                        (if (search-forward-regexp "Default target:" nil t)
                            (progn
                              (forward-line -1)
                              (point-at-eol))
                          (progn
                            (goto-char (point-max))
                            (point-at-eol))
                          ))
      (map 'list (lambda (elem)
                   (if (not (string-equal elem ""))
                       (push (concat "ANT" " "  (car (split-string (string-trim elem) " "))) ant-tasks)))
           (split-string (buffer-string) "\n")))
    (kill-current-buffer)

    ;; Return the tasks after killing buffer
    ant-tasks))

(defun taskrunner-get-ant-tasks (DIR)
  "Retrieve the ant tasks for the project in directory DIR.
This function returns a list of the form:
\(\"ANT TASK1\" \"ANT TASK2\"...)"
  (let ((default-directory DIR)
        (buff (get-buffer-create taskrunner-ant-tasks-buffer-name)))
    (call-process "ant"  nil taskrunner-ant-tasks-buffer-name  nil "-verbose" "-p")
    (with-temp-buffer
      (set-buffer buff)
      (taskrunner--retrieve-ant-tasks-from-buffer))))

(provide 'taskrunner-java)
;;; taskrunner-java.el ends here
