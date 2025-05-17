#!/usr/bin/env bash

set -e

for i in {1..3}; do
  output="$("$@" 2>&1)" && break
  if echo "$output" | grep -q "error: bad HTTP response code: '500 Internal Server Error'"; then
    echo "Retrying due to HTTP 500 error ($i/3)..."
    sleep 2
  else
    echo "$output"
    exit 1
  fi
done