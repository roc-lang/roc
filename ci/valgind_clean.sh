#!/usr/bin/env bash

valgrind "$@" 2>&1 | grep -v "Warning: DWARF2 reader: Badly formed extended line op encountered"
exit ${PIPESTATUS[0]}
