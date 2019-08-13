;; Functions/Variables/Warnings related to extracting the tasks from gradle
(require 'projectile)

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
  :group 'emacs-taskrunner)

(defcustom taskrunner-gradle-tasks-buffer-name "*taskrunner-gradle-tasks*"
  "Name of the buffer used for parsing the tasks from the output of gradle."
  :type 'string
  :group 'emacs-taskrunner)

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
(defun taskrunner--gradle-get-heading-tasks (heading)
  "Retrieve the gradle tasks below the heading HEADING and return as list."
  (widen)
  (goto-line 1)
  (let ((beg (re-search-forward heading nil t)))
    (when beg 
      (narrow-to-region beg 
                        (progn 
                          (re-search-forward "^$" nil t)
                          ;; Go up a line so the empty line at the end of the block
                          ;; is not included in the output
                          (previous-line 1)
                          (line-end-position)))

      (map 'list (lambda (elem)
                   (concat "GRADLE" " " (car (split-string elem " "))))
           (split-string (buffer-string) "\n"))
      )))

(defun taskrunner--gradle-tasks (dir)
"Retrieve the gradle tasks for the project in directory DIR."
(let ((default-directory dir)
      (buff (get-buffer-create "*taskrunner-gradle-tasks*"))
      (gradle-tasks '())
      )
  (call-process "gradle"  nil "*taskrunner-gradle-tasks*"  nil "tasks")
  (with-temp-buffer
    (set-buffer buff)
    (dolist (curr-regex taskrunner-gradle-heading-regexps)
      (let ((tasks-retrieved (taskrunner--gradle-get-heading-tasks curr-regex)))
        (when tasks-retrieved
          (setq gradle-tasks (append gradle-tasks tasks-retrieved))
          )
        ))
    (kill-buffer buff))
  ;; Return the tasks acquired
  gradle-tasks
  )
)

(provide 'taskrunner-gradle)
