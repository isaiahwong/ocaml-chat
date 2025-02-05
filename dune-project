(lang dune 3.17)

(name chat)

(generate_opam_files true)

(source
 (github isaiahwong/ocaml-chat))

(authors "Isaiah Wong <iisaiah.dev@gmail.com>")

(maintainers "Isaiah Wong <iisaiah.dev@gmail.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name chat)
 (synopsis "1 on 1 Chat application")
 (description "Chat application built with custom protocol")
 (depends ocaml lwt mtime alcotest alcotest-lwt cmdliner)
 (tags
  (chat tcp sockets application_layer)))

;
