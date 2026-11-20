#!/usr/bin/env bash
set -euo pipefail

heartbeat() {
    while true; do
        sleep 60
        printf 'ci heartbeat: nix zig build still running at %s\n' "$(date -u +%H:%M:%S)"
    done
}

build_ci=0
if [[ "${1:-}" == "--build-ci" ]]; then
    build_ci=1
    shift
fi

heartbeat &
heartbeat_pid=$!

cleanup() {
    kill "$heartbeat_pid" 2>/dev/null || true
    wait "$heartbeat_pid" 2>/dev/null || true
}
trap cleanup EXIT

if [[ "$build_ci" -eq 0 ]]; then
    zig build "$@"
    exit 0
fi

# Everything MiniCI runs, built with the dev shell's own Zig (built from
# source by Nix) and the libraries the flake provides, kcov's among them, with
# MiniCI's own build options (`minici_build_args` in src/build/minici.zig).
# The tests themselves run on this host in ci_zig.yml's MiniCI shards.
zig build build-ci -Dembed-builtin-unit=true "$@"
