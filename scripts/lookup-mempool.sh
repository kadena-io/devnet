#!/usr/bin/env bash

NODE=${NODE:-mining-api-node}
CHAINID=${CHAINID:-0}
REQKEY=${REQKEY?missing REQKEY environment variable}

docker compose run --rm curl \
    -sk \
    "https://mining-api-node:1789/chainweb/0.0/development/chain/$CHAINID/mempool/lookup" \
    -XPOST \
    -d '["'"$REQKEY"'"]' \
    -H 'content-type: application/json' |
jq
