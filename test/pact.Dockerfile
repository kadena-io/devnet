ARG UBUNTUVER=20.04
FROM ubuntu:${UBUNTUVER}

ARG PACTVER=4.4
ARG GHCVER=8.10.7
ARG UBUNTUVER

LABEL version="$PACTVER"
LABEL ghc="$GHCVER"
LABEL ubuntu="$UBUNTUVER"

RUN apt-get update \
    && apt-get -y install curl binutils zip \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /pact

RUN curl -Ls \
    "https://github.com/kadena-io/pact/releases/download/v${PACTVER}/pact-${PACTVER}-linux-${UBUNTUVER}.zip" \
    -o pact.zip \
    && unzip pact.zip \
    && cp pact /usr/bin/ \
    && chmod 755 /usr/bin/pact

ENTRYPOINT ["/usr/bin/pact"]

