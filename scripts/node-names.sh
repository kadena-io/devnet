#!/usr/bin/env bash

docker compose ps --format json | jq -r '.[] | select(.Health == "healthy" and (.Name | test("node"))) | .Name'
