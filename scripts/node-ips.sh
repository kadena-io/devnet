#!/usr/bin/env bash

nodes=$(docker compose ps --format json | jq -r '.[] | select(.Health == "healthy" and (.Name | test("node"))) | .Name')

for n in $nodes ; do
    ip=$(docker inspect -f '{{range.NetworkSettings.Networks}}{{.IPAddress}}{{end}}' "$n")
    echo "$n: $ip"
done

