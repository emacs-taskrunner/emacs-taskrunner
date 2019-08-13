;; Functions, variables and messages used in the process of retrieving tasks
;; for any build/task system typically used for C/C++/C# languages.
(require 'projectile)

(defconst taskrunner--make-phony-target-regexp "^\.PHONY[[:space:]]+:[[:space:]]+"
  "Regular expression used to locate all PHONY targets in makefile.")

(defconst taskrunner--make-non-phony-target-regexp "^[a-zA-Z_/\\-]+:"
  "Regular expression used to locate all Makefile targets which are not PHONY.")

(defconst taskrunner--cmake-warning
  "Taskrunner: Detected CMake build system but no build folder or Makefile were found! Please setup
CMake for either insource or outsource build and then call emacs-taskrunner again!"
  "A warning string used to indicate that a CMake project was detected but no
build folder or makefile was found.")

(defun taskrunner--narrow-to-line ()
  "Narrow the buffer to the current line."
  (narrow-to-region (progn
                      (beginning-of-line)
                      (point))
                    (progn
                      (end-of-line)
                      (point))))

(defun taskrunner--make-get-non-phony-targets ()
  "Retrieve all non-phony Makefile targets from the current buffer."
  (interactive)
  (let ((target-list '()))
    (goto-line 1)
    (while (re-search-forward taskrunner--make-target-regexp nil t)
      (taskrunner--narrow-to-line)
      (push (concat "MAKE" " " (car (split-string (buffer-string) ":"))) target-list)
      (widen)
      )
    target-list
    )
  )

(defun taskrunner--make-get-phony-targets ()
  "Retrieve all PHONY Makefile targets from the current buffer."
  (interactive)
  (let ((target-list '()))
    (goto-line 1)
    (text-mode)
    (while (re-search-forward taskrunner--make-phony-target-regexp nil t)
      (taskrunner--narrow-to-line)
      (push
       (concat "MAKE" " " (string-trim (cadr (split-string (buffer-string) ":")))) target-list)
      (widen))
    target-list
    )
  )

(provide 'taskrunner-clang)
