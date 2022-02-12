#!/usr/bin/env bash

{
  echo "{"
  while read -r i ; do
      echo "\"$i\": "
      curl -sk -timeout=1 "https://$i:1789/chainweb/0.0/development/cut" || echo -n "{}"
      [[ "$i" = "db-server-node" ]] || echo ","
  done < ./NODES.txt
  echo "}"
} |
jq -crM
