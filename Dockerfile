FROM ocaml/opam:alpine-3.12-ocaml-4.11 AS build

RUN sudo apk add openssl m4 libressl-dev
RUN opam update && opam install dune yojson ppx_deriving_yojson ppx_compare telegraml

COPY --chown=opam . /app
WORKDIR /app

RUN opam config exec -- dune build

FROM alpine:3.12.3

RUN apk add libressl-dev

WORKDIR /app
COPY --from=build /app/_build/default/main.exe .

ENTRYPOINT [ "./main.exe" ]
