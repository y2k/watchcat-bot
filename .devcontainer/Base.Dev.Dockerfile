FROM ocaml/opam:debian-10-ocaml-4.13

RUN opam update && opam install dune yojson ppx_deriving_yojson ppx_compare ppx_inline_test

RUN opam update
RUN opam install -y ocaml-lsp-server
RUN opam install -y ocamlformat
RUN opam install alcotest

RUN sudo apt install -y pkg-config
RUN sudo apt install -y libgmp-dev libssl-dev

RUN opam repository add y2k git://github.com/y2k/opam
RUN opam pin y2k-telegraml 3.2.1
RUN opam update && opam install y2k-telegraml
