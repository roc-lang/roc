#!/usr/bin/env bash

LOG_FILE="earthly_err.txt"

script -efq output.log -c "earthly +test-all"

if grep -q "unexpected closing delimiter" "$LOG_FILE"; then
  echo ""
  echo ""
  echo "------<<<<<<!!!!!!>>>>>>------"
  echo "DETECTED FAILURE TO MOUNT ERROR: running without cache"
  echo "------<<<<<<!!!!!!>>>>>>------"
  echo ""
  echo ""
  earthly --no-cache +test-all
else
    cat $ERR_FILE
fi
