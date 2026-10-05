#!/bin/sh

# Set PS4, the sh -x prompt.
PS4='kcov@${BASH_SOURCE}@${LINENO}@'

# Turn on tracing
set -x
