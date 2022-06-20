#!/usr/bin/env bash
# version.txt is used by the CLI: roc --version
printf 'nightly pre-release, built from commit ' > version.txt && git log --pretty=format:'%h' -n 1 >> version.txt && printf ' on ' >> version.txt && date -u >> version.txt