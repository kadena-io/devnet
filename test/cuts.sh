#!/bin/bash

NODES="devnet_bootstrap-node_1 devnet_common-node_1 devnet_common-node_2 devnet_mining-node_1 devnet_mining-node_2 devnet_api-node_1"

{
  echo "{"
  for i in $NODES ; do
      echo "\"$i\": "
      curl -sk "https://$i:1789/chainweb/0.0/development/cut" || echo -n "{}"
      [[ "$i" = "devnet_api-node_1" ]] || echo ","
  done
  echo "}"
}
