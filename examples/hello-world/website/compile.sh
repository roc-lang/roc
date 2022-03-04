#!/bin/bash
WEBSITE_DIR=$(dirname "${BASH_SOURCE[0]}")
ROC_DIR=$WEBSITE_DIR/../../..
(
  cd $ROC_DIR
  cargo run -- build --target=wasm32 examples/hello-world/hello_world.roc
  cd examples/hello-world/website
  if mv ../hello_world.wasm .; then
    echo "Run a web server in the website directory (\`$WEBSITE_DIR\`) to see the result in a web browser."
  else
    echo "Tip: Make sure that the app (\`examples/hello-world/hello_world.roc\`) is using \`web-platform\`!"
  fi
)
