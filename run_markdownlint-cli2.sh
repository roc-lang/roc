docker run \
  -v $PWD:/workdir \
  davidanson/markdownlint-cli2:v0.5.1 \
  "**/*.md" \
  "#node_modules"
