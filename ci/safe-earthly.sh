#!/usr/bin/env bash
LOG_FILE="earthly_log.txt"
touch $LOG_FILE

# first arg + everything after
ARGS=${@:1}
FULL_CMD="earthly --config ci/earthly-conf.yml  --no-cache $ARGS"
echo $FULL_CMD
script -efq $LOG_FILE -c "$FULL_CMD"
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
