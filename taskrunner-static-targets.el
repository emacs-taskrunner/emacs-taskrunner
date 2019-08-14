;; Variables which contain constant targets present in build systems such as
;; cargo(Rust) or go(Golang)

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

(provide 'taskrunner-static-targets)
