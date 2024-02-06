(lang dune 3.13)

(name lisp_interpreter)

(generate_opam_files true)

(source
 (github brandon-g-tripp/lisp-interpreter-ocaml))

(authors "Brandon Tripp")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name lisp_interpreter)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
