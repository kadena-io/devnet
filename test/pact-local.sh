#!/usr/bin/env bash

NODE=${NODE:-api-node}
CHAINID=${CHAINID:-0}
CODE=${CODE:-$(cat "$1")}

pact -a <(cat <<EOF
code: |
    $(echo "$CODE" | sed '/^[[:space:]]*;/d' | tr -d '\n\r')
publicMeta:
  chainId: "$CHAINID"
  sender: "sender00"
  gasLimit: 150000
  gasPrice: 0.0000001
  ttl: 150000
  creationTime: $(date +%s)
networkId: "development"
nonce: "$(date)"
keyPairs:
  - public: 368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca
    secret: 251a920c403ae8c8f65f59142316af3c82b631fba46ddea92ee8c95035bd2898
    caps:
      - name: coin.GAS
        args: []
EOF
) |

jq -rc '.cmds[0]' |

{
    read -r PAYLOAD
    echo "paylaod: $PAYLOAD" 1>&2
    curl \
        "http://$NODE:1848/chainweb/0.0/development/chain/$CHAINID/pact/api/v1/local" \
        -XPOST \
        -d "$PAYLOAD" \
        -H "Content-Type:application/json" \
        -sk
}
