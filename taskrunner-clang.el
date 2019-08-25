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
(defcustom taskrunner-build-dir-list '("build" "Build" "buildDir" "builddir" "builds")
  "A list containing the name of build folders to be looked for."
  :group 'taskrunner
  :type 'list)

(defcustom taskrunner-source-dir-list '("src" "Src" "source" "Source")
  "A list containing the name of source code folders to be looked for."
  :group 'taskrunner
  :type 'list)

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


;;;; Functions


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
    (taskrunner-add-to-build-cache (projectile-project-root) DIR)
    (cl-map 'list (lambda (elem)
                    (concat "MAKE" " " elem)) targets)
    )
  )


(defun taskrunner-get-cmake-tasks (ROOT)
  "Retrieve all cmake tasks for the project in directory ROOT."
  (let ((dir-contents (directory-files ROOT))
        (build-dir-name)
        (build-path)
        (targets)
        (found-flag nil)
        (i 0))
    (while (and
            (not found-flag)
            (<= i (length taskrunner-build-dir-list)))

      (when (member (elt taskrunner-build-dir-list i) dir-contents)
        (setq build-dir-name (elt taskrunner-build-dir-list i))
        (setq found-flag t))

      (setq i (1+ i)))
    (when found-flag
      (setq build-path (expand-file-name build-dir-name ROOT))
      (setq dir-contents (directory-files build-path))
      (when (member "Makefile" dir-contents)
        (setq targets (taskrunner-get-make-targets build-path "Makefile" nil))))
    targets
    )
  )

(defun taskrunner-get-ninja-tasks (DIR)
  "Retrieve all ninja tasks from directory DIR."
  (let ((default-directory DIR)
        (targets '()))
    (call-process "ninja" nil (taskrunner--make-task-buff-name "ninja") nil "-t" "targets")
    (with-temp-buffer
      (set-buffer (taskrunner--make-task-buff-name "ninja"))
      (goto-char (point-min))
      (dolist (elem (split-string (buffer-string) "\n"))
        (push (concat "NINJA" " " (car (split-string elem ":"))) targets))
      (kill-current-buffer))
    (when targets
      (pop targets)
      (taskrunner-add-to-build-cache (projectile-project-root) DIR))
    targets))

(defun taskrunner-get-meson-tasks (ROOT)
  "Retrieve all ninja tasks from a meson project in directory ROOT."
  (let ((dir-contents (directory-files ROOT))
        (build-dir-name) ;; Build folder name
        (build-path) ;; Absolute path to build folder
        (targets '())
        (found-flag nil)
        (i 0))
    (while (and
            (not found-flag)
            (<= i (length taskrunner-build-dir-list)))

      (when (member (elt taskrunner-build-dir-list i) dir-contents)
        (setq build-dir-name (elt taskrunner-build-dir-list i))
        (setq found-flag t))
      (setq i (1+ i)))
    (when found-flag
      (setq build-path (expand-file-name build-dir-name ROOT))
      (setq targets (taskrunner-get-ninja-tasks build-path)))))

(provide 'taskrunner-clang)
;;; taskrunner-clang.el ends here
