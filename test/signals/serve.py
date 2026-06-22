#!/usr/bin/env python3
import argparse
import functools
import http.server
import os
import pathlib
import shlex
import shutil
import subprocess
import sys
import urllib.parse


SCRIPT_PATH = pathlib.Path(__file__).resolve()
SIGNALS_DIR = SCRIPT_PATH.parent
REPO_ROOT = SIGNALS_DIR.parent.parent
BROWSER_DIR = SIGNALS_DIR / "browser"
DEFAULT_APP = SIGNALS_DIR / "apps" / "counter.roc"
DEFAULT_ROC_BIN = REPO_ROOT / "zig-out" / "bin" / "roc"


def parse_args():
    parser = argparse.ArgumentParser(
        description=(
            "Build the Signals browser host with ReleaseSmall, build a wasm32 "
            "Signals app, and serve the Signals browser asset directory."
        )
    )
    parser.add_argument(
        "app",
        nargs="?",
        default=str(DEFAULT_APP.relative_to(REPO_ROOT)),
        help="Roc app to build. Defaults to test/signals/apps/counter.roc.",
    )
    parser.add_argument(
        "--port",
        type=int,
        default=int(os.environ.get("PORT", "8000")),
        help="HTTP server port. Defaults to PORT or 8000.",
    )
    parser.add_argument(
        "--output",
        default=os.environ.get("OUTPUT"),
        help="Wasm output path. Defaults to test/signals/browser/<app>.wasm.",
    )
    parser.add_argument(
        "--app-opt",
        choices=("dev", "size"),
        default=os.environ.get("APP_OPT", "size"),
        help="Roc --opt mode for the app build. Defaults to APP_OPT or size.",
    )
    parser.add_argument(
        "--roc-bin",
        default=os.environ.get("ROC_BIN", str(DEFAULT_ROC_BIN)),
        help="Roc compiler path. Defaults to ROC_BIN or ./zig-out/bin/roc.",
    )
    parser.add_argument(
        "--no-server",
        action="store_true",
        default=os.environ.get("NO_SERVER") == "1",
        help="Build only; do not start the server.",
    )
    return parser.parse_args()


def repo_path(value):
    path = pathlib.Path(value)
    if path.is_absolute():
        return path
    return REPO_ROOT / path


def command_path(value):
    path = pathlib.Path(value)
    if path.is_absolute() or len(path.parts) > 1:
        return repo_path(value)

    resolved = shutil.which(value)
    if resolved:
        return pathlib.Path(resolved)

    return path


def browser_relative(path):
    resolved = path.resolve()
    try:
        return resolved.relative_to(BROWSER_DIR)
    except ValueError:
        return None


def run(command):
    quoted = " ".join(shlex.quote(str(part)) for part in command)
    print(f"\n==> {quoted}", flush=True)
    subprocess.run(command, cwd=REPO_ROOT, check=True)


def build_host():
    run(
        [
            "zig",
            "build",
            "build-test-hosts",
            "-Dplatform=signals",
            "-Doptimize=ReleaseSmall",
        ]
    )


def ensure_roc(roc_bin):
    if roc_bin.exists() and os.access(roc_bin, os.X_OK):
        return

    if roc_bin == DEFAULT_ROC_BIN:
        run(["zig", "build", "roc"])

    if not roc_bin.exists() or not os.access(roc_bin, os.X_OK):
        raise SystemExit(f"missing Roc compiler: {roc_bin}")


def build_app(roc_bin, app, output, app_opt):
    run(
        [
            str(roc_bin),
            "build",
            "--target=wasm32",
            f"--opt={app_opt}",
            "--no-cache",
            f"--output={output}",
            str(app),
        ]
    )


def serve(port, wasm_relative):
    wasm_url = f"./{wasm_relative.as_posix()}"
    if wasm_relative.as_posix() == "counter.wasm":
        page_url = f"http://localhost:{port}/counter.html"
    else:
        query_value = urllib.parse.quote(wasm_url, safe="./")
        page_url = f"http://localhost:{port}/counter.html?wasm={query_value}"

    handler = functools.partial(
        http.server.SimpleHTTPRequestHandler,
        directory=str(BROWSER_DIR),
    )
    server = http.server.ThreadingHTTPServer(("", port), handler)

    print(f"\nServing {page_url}", flush=True)
    print("Press Ctrl-C to stop the server.\n", flush=True)

    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nKeyboard interrupt received, exiting.", flush=True)
    finally:
        server.server_close()


def main():
    args = parse_args()

    app = repo_path(args.app)
    if not app.is_file():
        raise SystemExit(f"missing app: {app}")

    if args.output:
        output = repo_path(args.output)
    else:
        output = BROWSER_DIR / f"{app.stem}.wasm"

    wasm_relative = browser_relative(output)
    if not args.no_server and wasm_relative is None:
        raise SystemExit(
            f"when serving, --output must be under {BROWSER_DIR}: {output}"
        )

    output.parent.mkdir(parents=True, exist_ok=True)

    roc_bin = command_path(args.roc_bin)
    ensure_roc(roc_bin)
    build_host()
    build_app(roc_bin, app, output, args.app_opt)

    if args.no_server:
        return

    serve(args.port, wasm_relative)


if __name__ == "__main__":
    try:
        main()
    except subprocess.CalledProcessError as err:
        sys.exit(err.returncode)
