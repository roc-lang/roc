#!/usr/bin/env bash

# retrieves roc_releases.json, expects TOKEN to be set to ${{ secrets.GITHUB_TOKEN }}

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
# set -euxo pipefail

echo $(echo "$AUTH_HEADER" | wc -c)

curl --request GET \
          --url https://api.github.com/repos/roc-lang/roc/releases \
          --header '$AUTH_HEADER' \
          --header 'content-type: application/json' \
          --output roc_releases.json

cat roc_releases.json