FROM library/haskell:9.6

RUN apt-get update && \
    apt-get install -y curl ca-certificates gnupg lsb-release && \
    curl -fsSL https://www.postgresql.org/media/keys/ACCC4CF8.asc | gpg --dearmor -o /usr/share/keyrings/pgdg.gpg && \
    echo "deb [signed-by=/usr/share/keyrings/pgdg.gpg] http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list && \
    apt-get update && \
    apt-get install -y libpq-dev postgresql-server-dev-16 && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY idc-catalog.cabal .
RUN cabal update && cabal build --only-dependencies 2>&1
COPY . .
RUN cabal build all && cabal test all
