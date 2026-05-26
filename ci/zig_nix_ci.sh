#!/usr/bin/env bash
set -euo pipefail

heartbeat() {
    while true; do
        sleep 60
        printf 'ci heartbeat: nix zig build still running at %s\n' "$(date -u +%H:%M:%S)"
    done
}

heartbeat &
heartbeat_pid=$!

cleanup() {
    kill "$heartbeat_pid" 2>/dev/null || true
    wait "$heartbeat_pid" 2>/dev/null || true
}
trap cleanup EXIT

zig build "$@"
zig build snapshot "$@"
zig build test "$@"
