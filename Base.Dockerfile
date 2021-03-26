FROM ocaml/opam:alpine-3.12-ocaml-4.11

RUN sudo apk add openssl m4 libressl-dev gmp-dev
RUN opam pin ssl 0.5.9
RUN opam update && opam install dune yojson ppx_deriving_yojson ppx_compare ppx_inline_test
RUN opam repository add y2k git://github.com/y2k/opam
RUN opam pin y2k-telegraml 3.2.1
RUN opam update && opam install y2k-telegraml
