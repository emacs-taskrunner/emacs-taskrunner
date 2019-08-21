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
  "Variable indicates whether or not to always retrieve targets starting with `_'."
  :type 'symbol
  :options '(t nil)
  :group 'taskrunner)

;;;; Constants

(defconst taskrunner--make-target-regexp-no-hidden
  "^[1-9a-zA-Z/\\-][1-9a-zA-Z-_/\\]*[[:space:]]*:"
  "Regular expression used to locate all Makefile targets which are not PHONY.")

(defconst taskrunner--make-target-regexp-hidden
  "^[1-9a-zA-Z-_/\\]+[[:space:]]*:"
  "Regular expression used to locate all Makefile targets which are not PHONY.")

(defconst taskrunner--make-nqp-command
  "make -nqp __BASH_MAKE_COMPLETION__=1 .DEFAULT 2>/dev/null"
  "Command used to build up the database of targets.")

(defconst taskrunner--cmake-warning
  "Taskrunner: Detected CMake build system but no build folder or Makefile were found! Please setup
CMake for either insource or outsource build and then call emacs-taskrunner again!"
  "Warning used to indicate that not build folder was found for CMake.")

(defvar taskrunner-cmake-build-cache '()
  "A cache used to store the CMake build folders for retrieval.
It is an alist of the form (project-root . build-folder)")

;;;; Functions

(defun taskrunner-add-to-build-cache (PROJ-ROOT BUILD-DIR)
  "Add BUILD-DIR as the build directory for make in PROJ-ROOT."
  (assoc-delete-all (intern PROJ-ROOT) taskrunner-cmake-build-cache)
  (push (list (intern PROJ-ROOT) BUILD-DIR) taskrunner-cmake-build-cache))

(defun taskrunner-get-build-cache (PROJ-ROOT)
  "Retrieve the build folder for PROJ-ROOT.  Return nil if it does not exist."
  (alist-get (intern PROJ-ROOT) taskrunner-cmake-build-cache nil))

(defun taskrunner-invalidate-cmake-cache ()
  "Delete the entire cmake cache."
  (setq taskrunner-cmake-build-cache '()))

(defun taskrunner-get-make-targets (DIR MAKEFILE-NAME HIDDEN)
  "Find all makefile targets from file called MAKEFILE-NAME located in DIR.
If HIDDEN is non-nil then include targets which start with _."
  (let* ((makefile-path (expand-file-name MAKEFILE-NAME DIR))
         (buff (get-file-buffer makefile-path))
         (curr-line)
         (targets '())
         (target-regexp (if HIDDEN
                            taskrunner--make-target-regexp-hidden
                          taskrunner--make-target-regexp-no-hidden)))
    (with-temp-buffer
      ;; Check if the current makefile is already opened
      (if buff
          (set-buffer buff)
        (insert-file-contents makefile-path))
      ;; Locate all targets
      (while (search-forward-regexp
              target-regexp nil t)
        (taskrunner--narrow-to-line)
        (setq curr-line (buffer-string))
        (if (and
             ;; Do not match anything of the form NAME := MORE_NAMES
             (not (string-match-p ".:=.*" curr-line))
             ;; Do not match anything of the form PHONY NAME : MORE_NAMES
             (not (string-match-p "^PHONY" curr-line))
             ;; Do not match anything of the form .PHONY NAME : MORE_NAMES
             (not (string-match-p "^\.PHONY" curr-line))
             ;; Do not match anything of the form FORCE NAME : MORE_NAMES
             (not (string-match-p "^FORCE" curr-line)))
            (push (car (split-string curr-line ":")) targets))
        (widen)
        )
      )
    (map 'list (lambda (elem)
                 (concat "MAKE" " " elem)) targets)
    )
  )

(defun taskrunner-cmake-find-build-folder (ROOT)
  "Attempt to locate the build folder in a CMake project in directory ROOT."
  (let ((dir-contents (directory-files ROOT))
        (build-path)
        (targets))
    (cond
     ((member "build" dir-contents)
      (setq build-path (expand-file-name "build" ROOT))
      (setq dir-contents (directory-files build-path))
      (when (member "Makefile" dir-contents)
        (setq targets (taskrunner-get-make-targets build-path "Makefile" taskrunner-retrieve-all-make-targets))))

     ((member "Build" dir-contents)
      (setq build-path (expand-file-name "Build" ROOT))
      (setq dir-contents (directory-files build-path))
      (when (member "Makefile" dir-contents)
        (setq targets (taskrunner-get-make-targets build-path "Makefile" taskrunner-retrieve-all-make-targets))
        )
      )
     ;; Check if there are NO makefiles in the main folder.
     ;; If there are not then prompt user to select a build folder for the makefile
     ((not (or (member "Makefile" dir-contents)
               (member "makefile" dir-contents)
               (member "GNUmakefile" dir-contents)))
      ;; Prompt and use that folder instead
      (setq build-path
            (read-directory-name "Select CMake build folder: " ROOT nil t))
      (setq targets (taskrunner-get-make-targets build-path "Makefile" taskrunner-retrieve-all-make-targets))
      )
     )
    (taskrunner-add-to-build-cache ROOT build-path)
    targets
    )
  )

(provide 'taskrunner-clang)
;;; taskrunner-clang.el ends here
