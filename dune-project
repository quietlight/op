(lang dune 3.23)

(name op)

(cram enable)

(generate_opam_files true)

(implicit_transitive_deps false)

(source
 (github quietlight/op))

(authors "David Cary, Claude, GPT, Deepseek")

(maintainers "David Cary")

(license MIT)

(documentation https://github.com/quietlight/op)

(package
 (name op)
 (synopsis "Minimal agentic loop")
 (description "op is a minimal agentic loop")
 (depends
  ocaml
  cmdliner
  fmt
  re
  uri
  yojson
  eio
  eio_main
  cohttp
  cohttp-eio
  tls
  tls-eio
  ca-certs
  domain-name
  mirage-crypto-rng
  (alcotest :with-test)
  (ppx_expect :with-test))
 (tags
  ("agentic" "llm" "cli")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
