#!/usr/bin/env bash

NODE=${NODE:-mining-api-node}
CHAINID=${CHAINID:-0}

docker compose run --rm curl \
    -sk \
    "https://$NODE:1789/chainweb/0.0/development/chain/$CHAINID/mempool/getPending" \
    -XPOST \
    -d '{}' \
    -H 'content-type: application/json' |
jq
