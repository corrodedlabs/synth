# Game server image for fly.io (or any container host).
# The server keeps all state in memory — run exactly one instance.
FROM racket/racket:8.11-full

WORKDIR /app

# dependencies first so source edits don't bust this layer
# --no-docs: scribble doc indexing fails in the slim image and isn't needed
RUN raco pkg install --auto --batch --no-docs rfc6455

COPY game.rkt client.rkt server.rkt ./
RUN raco make server.rkt

EXPOSE 8080

CMD ["racket", "server.rkt", "--port", "8080"]
