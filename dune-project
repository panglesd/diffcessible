(lang dune 3.11)

(name diffcessible)

(generate_opam_files true)

(source
 (github panglesd/diffcessible))

(authors "Paul-Elliot <peada@free.fr>" "Jules Aguillon <jules@j3s.fr>")

(maintainers "Paul-Elliot <peada@free.fr>" "Jules Aguillon <jules@j3s.fr>")

(license MIT)

(package
 (name diffcessible)
 (synopsis "An accessible diff viewer")
 (description "A TUI diff viewer aimed at being accessible.")
 (depends
  (ocaml
   (>= 4.14))
  dune
  cmdliner
  seq
  extunix
  (notty
   (>= 0.2)))
 (tags
  (diff "diff viewer" accessibility)))
