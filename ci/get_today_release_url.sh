#!/usr/bin/env bash

# assumes roc_releases.json is present

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

TODAY_RELEASE_URL=`cat roc_releases.json | jq --arg arch $1 --arg today $(date +'%Y-%m-%d') '.[0] | .assets | map(.browser_download_url) | map(select(. | contains("\($arch)-\($today)"))) | .[0]'`

if [[ "$TODAY_RELEASE_URL" == "null" ]]
then
  echo "I expected to get a url but I got null instead. Today's release may not have been uploaded?"
  exit 1
else
  echo $TODAY_RELEASE_URL | sed s/\"//g
fi
