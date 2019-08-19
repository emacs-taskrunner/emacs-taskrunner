;;; taskrunner-clang.el --- Provide functions to retrieve c/c++ build system targets -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Yavor Konstantinov

;;;; Commentary:
;; Provide function for extracting makefile targets.
;; Support included for:
;; - Makefiles(Makefile, makefile, GNUmakefile)
;; - CMake

;;;; Requirements

(require 'projectile)

;;;; Code:

(defcustom taskrunner-retrieve-all-make-targets t
  "Variable indicates whether or not to always retrieve both phony and non-phony targets."
  :type 'symbol
  :options '(t nil)
  :group 'taskrunner)

;;;; Constants
(defconst taskrunner--make-phony-target-regexp "^\.[[:space:]]*PHONY[[:space:]]*:[[:space:]]*"
  "Regular expression used to locate all PHONY targets in makefile.")

(defconst taskrunner--make-non-phony-target-regexp "^[1-9a-zA-Z_/\\-]+[[:space:]]*:"
  "Regular expression used to locate all Makefile targets which are not PHONY.")

(defconst taskrunner--cmake-warning
  "Taskrunner: Detected CMake build system but no build folder or Makefile were found! Please setup
CMake for either insource or outsource build and then call emacs-taskrunner again!"
  "Warning used to indicate that not build folder was found for CMake.")

(defvar taskrunner-cmake-build-cache '()
  "A cache used to store the CMake build folders for retrieval.
It is an alist of the form (project-root . build-folder)")

;;;; Functions

(defun taskrunner-invalidate-cmake-cache ()
  "Delete the entire cmake cache."
  (setq taskrunner-cmake-build-cache '()))

(defun taskrunner--make-get-non-phony-targets ()
  "Retrieve all non-phony Makefile targets from the current buffer.
That is, retrieve all targets which do not start with PHONY."
  (interactive)
  (fundamental-mode)
  (goto-line 1)
  (let ((target-list '()))
    (while (search-forward-regexp taskrunner--make-non-phony-target-regexp nil t)
      (taskrunner--narrow-to-line)
      (push (concat "MAKE" " " (car (split-string (buffer-string) ":"))) target-list)
      (widen)
      )
    target-list
    )
  )

(defun taskrunner--make-get-phony-targets ()
"Retrieve all PHONY Makefile targets from the current buffer.
The targets retrieved are every line of the form `.PHONY'."
(interactive)
(fundamental-mode)
(let ((target-list '()))
  (goto-line 1)
  (text-mode)
  (while (search-forward-regexp taskrunner--make-phony-target-regexp nil t)
    (taskrunner--narrow-to-line)
    (push
     (concat "MAKE" " " (string-trim (cadr (split-string (buffer-string) ":")))) target-list)
    (widen)
    )
  target-list
  )
)

(defun taskrunner-get-make-targets (ROOT MAKEFILE-NAME ALL)
  "Retrieve all targets from makefile name MAKEFILE-NAME in directory ROOT.
If ALL is non-nil then retrieve all targets possible(phony/non-phony).
Otherwise, retrieve only phony targets."
  (let* ((targets '())
         (makefile-path (expand-file-name MAKEFILE-NAME ROOT))
         (buff (get-file-buffer makefile-path)))
    (with-temp-buffer
      ;; Check if the buffer might already be open
      (if buff
          (set-buffer buff)
        (find-file makefile-path))
      (fundamental-mode)
      ;; Collect phony targets
      (setq targets (append targets (taskrunner--make-get-phony-targets)))
      ;; Non-phony targets are only retrieved when specified
      (when ALL
        (setq targets (append targets (taskrunner--make-get-non-phony-targets))))
      (makefile-mode)
      ;; Kill the current buffer only if it has not been previously opened by user
      (unless buff
        (kill-current-buffer)))
    ;; Return targets
    targets
    )
  )

(defun taskrunner-cmake-find-build-folder (ROOT)
  "Attempt to locate the build folder in a CMake project in directory ROOT."
  (let ((dir-contents (directory-files ROOT))
        (build-path))
    (cond
     ((member "build" dir-contents)
      (setq build-path (expand-file-name "build" ROOT))
      (setq dir-contents (directory-files build-path))
      (when (member "Makefile" dir-contents)
        (taskrunner-get-make-targets build-path "Makefile" nil)))
     ((member "Build" dir-contents)
      (setq build-path (expand-file-name "Build" ROOT))
      (setq dir-contents (directory-files build-path))
      (when (member "Makefile" dir-contents)
        (taskrunner-get-make-targets build-path "Makefile" nil)))
     ;; Check if there are NO makefiles in the main folder.
     ;; If there are not then prompt user to select a build folder for the makefile
     ((not (or (member "Makefile" work-dir-files)
               (member "makefile" work-dir-files)
               (member "GNUmakefile" work-dir-files)))
      ;; Prompt and use that folder instead
      (setq build-path
            (ido-read-directory-name "Select CMake build folder: " ROOT nil t))
      )
     )
    )
  )

(provide 'taskrunner-clang)
;;; taskrunner-clang.el ends here
