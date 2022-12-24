#!/usr/bin/env bash

# assumes roc_releases.json is present

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

LATEST_RELEASE_URL=`cat roc_releases.json | jq --arg arch $1 '.[0] | .assets | map(.browser_download_url) | map(select(. | contains("\($arch)-"))) | last'`

if [[ "$LATEST_RELEASE_URL" == "null" ]]
then
  echo "I expected to get a url but I got null instead. The provided argument for this bash script may not have had any matches when used as arch in the jq query above."
  exit 1
else
  echo $LATEST_RELEASE_URL | sed s/\"//g
fi
