#! /bin/bash

TXKEY=${1:?TXKEY is missing}

NODE=${NODE:-api-node}
CHAINID=${CHAINID:-0}

echo "request key: $TXKEY" 1>&2
res='{}'
while [[ "$res" = "{}" ]] ; do
    sleep 5
    res=$(curl \
        "http://$NODE:1848/chainweb/0.0/development/chain/$CHAINID/pact/api/v1/poll" \
        -XPOST \
        -d '{"requestKeys":["'"$TXKEY"'"]}' \
        -H "Content-Type:application/json" \
        -sk)
    echo "poll result: $res"
done
