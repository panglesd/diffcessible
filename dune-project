(lang dune 3.11)

(name diffcessible)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Paul-Elliot <peada@free.fr>" "Jules Aguillon <jules@j3s.fr>")

(maintainers "Paul-Elliot <peada@free.fr>" "Jules Aguillon <jules@j3s.fr>")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name diffcessible)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  (ocaml
   (>= 4.14))
  dune
  cmdliner
  seq
  (notty
   (>= 0.2)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
