#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# download fonts just-in-time so we don't have to bloat the repo with them.
# TODO convert .woff fonts to .woff2
# wget -O SourceCodePro.woff https://fonts.gstatic.com/s/sourcecodepro/v22/HI_diYsKILxRpg3hIP6sJ7fM7PqPMcMnZFqUwX28DMyQtMdrSlcZZJmOpwVS.woff
# wget -O lato.woff2 https://fonts.gstatic.com/s/lato/v23/S6uyw4BMUTPHjx4wXiWtFCc.woff2

# cd into the directory where this script lives.
# This allows us to run this script from the root project directory,
# which is what Netlify wants to do.

cp -f public/tutorial/index.html build/tutorial/index.html
cp -f public/*.css build/
cp -f public/*.svg build/
