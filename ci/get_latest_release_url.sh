#!/usr/bin/env bash

# assumes roc_releases.json is present

LATEST_RELEASE_URL=`cat roc_releases.json | jq --arg arch $1 --arg today $(date +'%Y-%m-%d') '.[0] | .assets | map(.browser_download_url) | map(select(. | contains("\($arch)-\($today)"))) | .[0]'`

if [[ "$LATEST_RELEASE_URL" == "null" ]]
then
  echo "I expected to get a url but I got null instead. Today's release may not have been uploaded?"
  exit 1
else
  echo $LATEST_RELEASE_URL | sed s/\"//g
fi