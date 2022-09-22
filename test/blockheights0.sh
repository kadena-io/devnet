#!/usr/bin/env bash

bash cuts.sh | jq 'to_entries | .[] | .value = .value.hashes."0".height | select(.value)'
