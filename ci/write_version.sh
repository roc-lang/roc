#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# version.txt is used by the CLI: roc --version
printf 'nightly pre-release, built from commit ' > version.txt && git log --pretty=format:'%h' -n 1 >> version.txt && printf ' on ' >> version.txt && date -u >> version.txt