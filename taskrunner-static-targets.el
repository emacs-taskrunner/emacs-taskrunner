;;; taskrunner-static-targets.el --- Provide functions to retrieve build system targets which do not chagne -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Yavor Konstantinov

;;;; Commentary:
;; Support for:
;; - Rust/cargo
;; - Golang/go
;; - Elisp/Cask
;; - Haskell/Cabal
;; - Haskell/Stack

;;;; Code:

;;;; Variables

(defvar taskrunner--rust-targets '("RUST build"
                                   "RUST check"
                                   "RUST clean"
                                   "RUST init"
                                   "RUST run"
                                   "RUST test"
                                   "RUST bench"
                                   "RUST update"
                                   "RUST search"
                                   "RUST install"
                                   "RUST uninstall"
                                   "RUST version")
  "Targets for Rust's cargo build system.
Included in taskrunner output only when Cargo.toml is present if project root.")

(defvar taskrunner--golang-targets '("GO build"
                                     "GO clean"
                                     "GO doc"
                                     "GO env"
                                     "GO fix"
                                     "GO fmt"
                                     "GO generate"
                                     "GO get"
                                     "GO install"
                                     "GO list"
                                     "GO run"
                                     "GO test"
                                     "GO tool"
                                     "GO version"
                                     "GO vet")
  "Targets used in the Golang build systems.
Included in taskrunner output only when go.mod or go.sum are present in
project root.")

(defvar taskrunner--cask-targets '("CASK exec"
                                   "CASK emacs"
                                   "CASK eval"
                                   "CASK help"
                                   "CASK info"
                                   "CASK install"
                                   "CASK list"
                                   "CASK load-path"
                                   "CASK outdated"
                                   "CASK pkg-file"
                                   "CASK package-directory"
                                   "CASK path"
                                   "CASK update"
                                   "CASK upgrade-cask"
                                   "CASK version"
                                   "CASK files"
                                   "CASK build"
                                   "CASK clean-elc"
                                   "CASK link"
                                   "CASK package")
  "Targets used in the Emacs Cask build/project management system.
Included in the taskrunner output only when a file named `Cask' is present
in the project root.")

(defvar taskrunner--stack-targets '("STACK build"
                                    "STACK install"
                                    "STACK test"
                                    "STACK bench"
                                    "STACK haddoc"
                                    "STACK new"
                                    "STACK templates"
                                    "STACK init"
                                    "STACK solver"
                                    "STACK setup"
                                    "STACK path"
                                    "STACK ls"
                                    "STACK unpack"
                                    "STACK update"
                                    "STACK upload"
                                    "STACK sdist"
                                    "STACK dot"
                                    "STACK clean"
                                    "STACK list-dependencies"
                                    "STACK query")
  "Static targets for stack based projects.
They are included when stack.yaml is present in the project root.")

(defvar taskrunner--cabal-targets '("CABAL update"
                                    "CABAL install"
                                    "CABAL info"
                                    "CABAL list"
                                    "CABAL fetch"
                                    "CABAL user-config"
                                    "CABAL get"
                                    "CABAL init"
                                    "CABAL configure"
                                    "CABAL build"
                                    "CABAL clean"
                                    "CABAL test"
                                    "CABAL bench"
                                    "CABAL check"
                                    "CABAL sdist"
                                    "CABAL upload"
                                    "CABAL report"
                                    "CABAL freeze"
                                    "CABAL gen-bounds"
                                    "CABAL outdated"
                                    "CABAL doctest"
                                    "CABAL haddock"
                                    "CABAL hscolour"
                                    "CABAL copy"
                                    "CABAL register"
                                    "CABAL reconfigure")
  "Static targets for cabal based projects.
They are used when any *.cabal files are present in project root.")

(provide 'taskrunner-static-targets)
;;; taskrunner-static-targets.el ends here
