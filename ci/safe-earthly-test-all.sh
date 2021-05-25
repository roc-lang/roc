#!/usr/bin/env bash

LOG_FILE="earthly_log.txt"
touch $LOG_FILE

script -efq $LOG_FILE -c "earthly --buildkit-cache-size-mb 25000 +test-all"

if grep -q "failed to mount" "$LOG_FILE"; then
  echo ""
  echo ""
  echo "------<<<<<<!!!!!!>>>>>>------"
  echo "DETECTED FAILURE TO MOUNT ERROR: running without cache"
  echo "------<<<<<<!!!!!!>>>>>>------"
  echo ""
  echo ""
  earthly --no-cache --buildkit-cache-size-mb 25000 +test-all
else
  return 1
fi
