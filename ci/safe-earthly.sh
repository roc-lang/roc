#!/usr/bin/env bash

LOG_FILE="earthly_log.txt"
touch $LOG_FILE

ARGS=$1

if [[ $ARGS == *"bench"* ]]; then
  ARGS="--allow-privileged $ARGS"
fi

script -efq $LOG_FILE -c "earthly --config ci/earthly-conf.yml $ARGS"
EXIT_CODE=$?

if grep -q "failed to mount" "$LOG_FILE"; then
  echo ""
  echo ""
  echo "------<<<<<<!!!!!!>>>>>>------"
  echo "DETECTED FAILURE TO MOUNT ERROR: running without cache"
  echo "------<<<<<<!!!!!!>>>>>>------"
  echo ""
  echo ""
  earthly --config ci/earthly-conf.yml --no-cache $ARGS
else
  exit $EXIT_CODE
fi
