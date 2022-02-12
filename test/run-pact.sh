#!/bin/bash

NODE=${NODE:-api-node}
CHAINID=${CHAINID:-0}
CODE=${CODE:-$(cat "$1")}

pact -a <(cat <<EOF
code: |
    $(echo "$CODE" | sed '/^[[:space:]]*;/d' | tr -d '\n\r')
publicMeta:
  chainId: "$CHAINID"
  sender: "sender00"
  gasLimit: 5000
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
      - name: coin.TRANSFER
        args: ["sender00","sender01",1.0]
EOF
) |

{
    read -r PAYLOAD
    echo "paylaod: $PAYLOAD" 1>&2
    curl \
        "http://$NODE:1848/chainweb/0.0/development/chain/$CHAINID/pact/api/v1/send" \
        -XPOST \
        -d "$PAYLOAD" \
        -H "Content-Type:application/json" \
        -sk
} |

jq -rc '.requestKeys[0]' |

# poll
{
    read -r TXKEY
    echo "request key: $TXKEY" 1>&2
    res='{}'
    while [[ $res = "{}" ]] ; do
        sleep 5
        res=$(curl \
            "http://$NODE:1848/chainweb/0.0/development/chain/$CHAINID/pact/api/v1/poll" \
            -XPOST \
            -d '{"requestKeys":["'"$TXKEY"'"]}' \
            -H "Content-Type:application/json" \
            -sk)
        echo "poll result: $res"
    done
}

# ############################################################################ #
# INJECT INTO MEMPOOL

# tee /dev/stderr |
# 
# # format for mempool
# jq -rc '[ .cmds | .[] |  tojson ]' |
# 
# tee /dev/stderr |
# 
# # PUT into mempool
# curl \
#     "https://$NODE:1789/chainweb/0.0/development/chain/$CHAINID/mempool/insert" \
#     -XPUT \
#     -d @- \
#     -H "Content-Type:application/json" \
#     -sk
# 
# # Get pending TXs
# curl \
#     "https://$NODE:1789/chainweb/0.0/development/chain/$CHAINID/mempool/getPending" \
#     -XPOST \
#     -d '{}' \
#     -H "Content-Type:application/json" \
#     -sk
# echo
# 
# sleep 2
# 
# curl \
#     "https://$NODE:1789/chainweb/0.0/development/chain/$CHAINID/mempool/getPending" \
#     -XPOST \
#     -d '{}' \
#     -H "Content-Type:application/json" \
#     -sk
# echo
# 
# # poll
# res='{}'
# while [[ $res = "{}" ]] ; do
#     sleep 5
#     res=$(curl \
#         "http://$NODE:1848/chainweb/0.0/development/chain/$CHAINID/pact/api/v1/poll" \
#         -XPOST \
#         -d '{"requestKeys":["'$TXID'"]}' \
#         -H "Content-Type:application/json" \
#         -sk)
#     echo "poll result: $res"
# done
