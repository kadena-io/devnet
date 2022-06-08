#!/usr/bin/env bash

bash ./cuts.sh | jq 'to_entries | .[] | .value = .value.height'
