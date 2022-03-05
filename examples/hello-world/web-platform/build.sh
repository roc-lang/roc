#!/bin/bash
WEBSITE_DIR=$(dirname "${BASH_SOURCE[0]}")
ROC_DIR=$WEBSITE_DIR/../../..
(
  cd $ROC_DIR
  cargo run -- build --target=wasm32 examples/hello-world/web-platform/hello_web.roc
  echo "Run a web server in $WEBSITE_DIR to see the result in a web browser."
)
