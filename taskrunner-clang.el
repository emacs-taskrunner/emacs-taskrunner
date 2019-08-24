;;; taskrunner-clang.el --- Provide functions to retrieve c/c++ build system targets -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Yavor Konstantinov

;;;; Commentary:
;; Provide function for extracting makefile targets.
;; Support included for:
;; - Makefiles(Makefile, makefile, GNUmakefile)
;; - CMake

;;;; Requirements

(require 'projectile)
(require 'cl-lib)

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
  (setq taskrunner-cmake-build-cache (assoc-delete-all (intern PROJ-ROOT) taskrunner-cmake-build-cache))
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
    (cl-map 'list (lambda (elem)
                    (concat "MAKE" " " elem)) targets)
    )
  )

(defcustom taskrunner-cmake-build-dir-list '("build" "Build")
  "A list containing the name of build folders which are used in CMake projects."
  :group 'taskrunner
  :type 'list)

(defun taskrunner-cmake-find-build-folder (ROOT)
  "Attempt to locate the build folder in a CMake project in directory ROOT."
  (let ((dir-contents (directory-files ROOT))
        (build-dir-name)
        (build-path)
        (targets)
        (found-flag nil)
        (i 0))
    (while (and
            (not found-flag)
            (<= i (length taskrunner-cmake-build-dir-list)))

      (when (member (elt taskrunner-cmake-build-dir-list i) dir-contents)
        (setq build-dir-name (elt taskrunner-cmake-build-dir-list i))
        (setq found-flag t))

      (setq i (1+ i)))
    (when found-flag
      (setq build-path (expand-file-name build-dir-name ROOT))
      (setq dir-contents (directory-files build-path))
      (when (member "Makefile" dir-contents)
        (setq targets (taskrunner-get-make-targets build-path "Makefile" nil))
        (taskrunner-add-to-build-cache ROOT build-path)))
    targets
    )
  )

(provide 'taskrunner-clang)
;;; taskrunner-clang.el ends here
