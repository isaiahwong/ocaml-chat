(lang dune 3.17)

(name chat)

(generate_opam_files true)

(source
 (github isaiahwong/ocaml-chat))

(authors "Isaiah Wong <iisaiah.dev@gmail.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name chat)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml lwt mtime alcotest alcotest-lwt)
 (tags
  (chat tcp)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
