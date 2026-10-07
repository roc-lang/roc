#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# Check if the correct number of arguments is given
if [ "$#" -ne 2 ]; then
    echo "Usage: ./update_basic_cli_url.sh OLD_URL NEW_URL"
    exit 1
fi

OLD_URL=$1
NEW_URL=$2

# Use git ls-files to list all files tracked by Git, excluding those in .gitignore
files=$(git ls-files)

# Use perl to replace OLD_URL with NEW_URL in the files
for file in $files; do
    perl -pi -e "s|\Q$OLD_URL\E|$NEW_URL|g" $file
done

echo "Replaced all old basic-cli URLs with the new one."