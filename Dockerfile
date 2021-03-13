FROM ocaml/opam:alpine-3.12-ocaml-4.11 AS build

RUN sudo apk add openssl m4 libressl-dev
RUN opam pin ssl 0.5.9
RUN opam update && opam install dune yojson ppx_deriving_yojson ppx_compare ppx_inline_test
RUN opam repository add y2k git://github.com/y2k/opam && opam install y2k-telegraml

COPY --chown=opam dune-project /app/dune-project
COPY --chown=opam dune /app/dune
COPY --chown=opam app/dune /app/app/
COPY --chown=opam app/*.ml /app/app/
COPY --chown=opam lib/dune /app/lib/
COPY --chown=opam lib/*.ml /app/lib/
COPY --chown=opam test/dune /app/test/
COPY --chown=opam test/*.ml /app/test/
WORKDIR /app

RUN opam config exec -- dune build
RUN opam config exec -- dune test

FROM alpine:3.12.3

RUN apk add libressl-dev

WORKDIR /app
COPY --from=build /app/_build/default/app/main.exe .

ENTRYPOINT [ "./main.exe" ]
