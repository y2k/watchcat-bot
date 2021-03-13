FROM y2khub/watchcat-bot-base AS build

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

ENTRYPOINT [ "export OCAMLRUNPARAM=b && ./main.exe" ]
