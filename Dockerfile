# Game server image for fly.io (or any container host).
# State lives in memory with SQLite persistence on /data — run exactly
# one instance, with a volume mounted at /data (see fly.toml).
FROM racket/racket:8.11-full

WORKDIR /app

# dependencies first so source edits don't bust this layer
# --no-docs: scribble doc indexing fails in the slim image and isn't needed
RUN raco pkg install --auto --batch --no-docs rfc6455
# sqlite backs match persistence and the leaderboard
RUN apt-get update \
    && apt-get install -y --no-install-recommends libsqlite3-0 \
    && rm -rf /var/lib/apt/lists/*

COPY game.rkt client.rkt server.rkt storage.rkt ./
RUN raco make server.rkt

EXPOSE 8080

CMD ["racket", "server.rkt", "--port", "8080"]
