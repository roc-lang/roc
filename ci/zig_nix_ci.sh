#!/usr/bin/env bash
set -euo pipefail

heartbeat() {
    while true; do
        sleep 60
        printf 'ci heartbeat: nix zig build still running at %s\n' "$(date -u +%H:%M:%S)"
    done
}

build_only=0
if [[ "${1:-}" == "--build-only" ]]; then
    build_only=1
    shift
fi

heartbeat &
heartbeat_pid=$!

cleanup() {
    kill "$heartbeat_pid" 2>/dev/null || true
    wait "$heartbeat_pid" 2>/dev/null || true
}
trap cleanup EXIT

zig build "$@"
if [[ "$build_only" -eq 1 ]]; then
    printf 'Skipping MiniCI in this Nix leg; the regular Zig target matrix covers it for: %s\n' "$*"
    exit 0
fi

# Every MiniCI job whose result can differ by host, as declared for this file
# in the `workflows` table in src/build/minici.zig. MiniCI forwards the
# trailing arguments to each child `zig build`.
zig build minici -- --minici-lane secondary "$@"
