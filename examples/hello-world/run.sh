#!/bin/bash
HELLO_WORLD_DIR=$(dirname "${BASH_SOURCE[0]}")
(cd $HELLO_WORLD_DIR && cargo run hello_world.roc)
