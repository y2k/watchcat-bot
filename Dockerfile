FROM ocaml/opam2:alpine-3.12-ocaml-4.11 AS build

RUN opam update && opam install dune

RUN sudo apk add openssl
RUN sudo apk add m4

RUN opam install yojson
RUN opam install ppx_deriving_yojson

RUN sudo apk add libressl-dev
RUN opam install telegraml

COPY --chown=opam . /app
WORKDIR /app

RUN opam config exec -- dune build

FROM alpine:3.12.3

WORKDIR /app
COPY --from=build /app/_build/default/main.exe .

RUN apk add libressl-dev

ENTRYPOINT [ "./main.exe" ]
