FROM ocaml/opam:alpine-3.12-ocaml-4.11 AS build

RUN sudo apk add openssl m4 libressl-dev
RUN opam update && opam install dune yojson ppx_deriving_yojson ppx_compare ppx_inline_test
RUN opam repository add y2k git://github.com/y2k/opam && opam install y2k-telegraml

COPY --chown=opam . /app
WORKDIR /app

RUN opam config exec -- dune build
# FIXME restore tests
# RUN opam config exec -- dune test

RUN ls -la /app/_build/default/app

FROM alpine:3.12.3

RUN apk add libressl-dev

WORKDIR /app
COPY --from=build /app/_build/default/app/main.exe .

ENTRYPOINT [ "./main.exe" ]
