(lang dune 3.0)
(name stac_client)
(generate_opam_files true)
(source (github mtelvers/stac-client))
(license MIT)
(authors "Mark Elvers <mark.elvers@tunbury.org>")
(homepage "https://github.com/mtelvers/stac-client")
(bug_reports "https://github.com/mtelvers/stac-client/issues")

(package
 (name stac_client)
 (synopsis "OCaml STAC API client with Planetary Computer signing")
 (depends
  (ocaml (>= 5.1.0))
  dune
  cohttp-eio
  http
  yojson
  eio
  eio_main
  ppx_deriving_yojson
  tls-eio
  ca-certs
  mirage-crypto-rng
  (alcotest :with-test)))
