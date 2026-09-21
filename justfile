# local-only build helpers
#   see .git/info/exclude

# list all available just recipes
_default:
   @ just --list --unsorted

alias install := install-latest-release

# fetch and install the latest roc with rust_glue
[linux]
install-latest-release: rebase install-release

# clean build and install
[linux]
install-dev: clean && install-rust-glue
    zig build roc
    cp ./zig-out/bin/roc ~/.local/bin/

# clean build and install release-fast
[linux]
install-release: clean && install-rust-glue
    zig build build-release
    cp ./zig-out/bin/roc ~/.local/bin/
    just sync-roc-coding

# refresh the global roc-coding skill's reference copies
[linux]
sync-roc-coding:
    mkdir -p "$HOME/.dotfiles/.config/polytoken/skills/roc-coding"
    cp "docs/mini-tutorial-new-compiler.md" "$HOME/.dotfiles/.config/polytoken/skills/roc-coding/mini-tutorial-new-compiler.md"
    cp "test/echo/all_syntax_test.roc" "$HOME/.dotfiles/.config/polytoken/skills/roc-coding/all_syntax_test.roc"

# install src/glue/src/RustGlue.roc as the `rust_glue` shorthand for the roc on PATH
[linux]
install-rust-glue:
    #!/usr/bin/env bash
    set -euo pipefail
    tmp=$(mktemp -d)
    server_pid=""
    trap 'kill "$server_pid" 2>/dev/null || true; rm -rf "$tmp"' EXIT
    mkdir "$tmp/serve"
    cp src/glue/src/RustGlue.roc "$tmp/main.roc"
    (cd "$tmp" && roc bundle --output-dir "$tmp/serve" main.roc)
    bundle=$(basename "$tmp"/serve/*.tar.zst)
    # no `roc uninstall` exists, and reinstalling the same shorthand from a
    # different URL is a hard error, so drop this compiler version's entry
    version=$(roc version | awk '{print $NF}')
    rm -rf "${ROC_INSTALL_DIR:-${XDG_DATA_HOME:-$HOME/.local/share}/roc}/$version/rust_glue"
    python3 -u -m http.server 0 --bind 127.0.0.1 --directory "$tmp/serve" >"$tmp/server.log" 2>&1 &
    server_pid=$!
    port=""
    for _ in $(seq 1 50); do
        port=$(sed -n 's/.*port \([0-9]*\).*/\1/p' "$tmp/server.log" | head -n1)
        [ -n "$port" ] && break
        sleep 0.1
    done
    [ -n "$port" ] || { echo "http.server did not start"; cat "$tmp/server.log"; exit 1; }
    roc install rust_glue "http://127.0.0.1:$port/$bundle"

# clean build and test
[linux]
test: clean
    zig build test

# clean local build artifacts and the cache
[linux]
clean:
    git clean -dfx -e justfile -e .claude
    rm -rf ~/.cache/roc

# rebase this local justfile branch onto upstream main and push it
rebase:
    git fetch origin && git rebase origin/main
    git push --force-with-lease origin local-install
