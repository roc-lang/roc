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
DEFAULT_ROC_BIN = REPO_ROOT / "zig-out" / "bin" / "roc"
APP_SUITE = (
    SIGNALS_DIR / "apps" / "ops_dashboard.roc",
    SIGNALS_DIR / "apps" / "checkout_wizard.roc",
    SIGNALS_DIR / "apps" / "kanban_board.roc",
    SIGNALS_DIR / "apps" / "identity_stress.roc",
    SIGNALS_DIR / "apps" / "component_composition.roc",
    SIGNALS_DIR / "apps" / "async_effects.roc",
)
SUITE_STEMS = tuple(app.stem for app in APP_SUITE)


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
        default=None,
        help=(
            "Roc app to build. Defaults to the maintained six-app suite; pass "
            "one app path for targeted QA."
        ),
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


def default_apps():
    return APP_SUITE


def app_title(app):
    return app.stem.replace("_", " ").title()


def app_page_url(port, wasm_relative, title):
    wasm_url = f"./{wasm_relative.as_posix()}"
    query_value = urllib.parse.urlencode({"wasm": wasm_url, "app": title})
    return f"http://localhost:{port}/app.html?{query_value}"


def serve(port, pages):
    if len(pages) == len(SUITE_STEMS) and tuple(page[0] for page in pages) == SUITE_STEMS:
        page_url = f"http://localhost:{port}/"
    else:
        page_url = app_page_url(port, pages[0][1], pages[0][2])

    handler = functools.partial(
        http.server.SimpleHTTPRequestHandler,
        directory=str(BROWSER_DIR),
    )
    server = http.server.ThreadingHTTPServer(("", port), handler)

    print(f"\nServing {page_url}", flush=True)
    if len(pages) > 1:
        print("App URLs:", flush=True)
        for stem, wasm_relative, title in pages:
            print(f"  {stem}: {app_page_url(port, wasm_relative, title)}", flush=True)
    print("Press Ctrl-C to stop the server.\n", flush=True)

    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nKeyboard interrupt received, exiting.", flush=True)
    finally:
        server.server_close()


def main():
    args = parse_args()

    apps = [repo_path(args.app)] if args.app else list(default_apps())
    for app in apps:
        if not app.is_file():
            raise SystemExit(f"missing app: {app}")

    if len(apps) > 1 and args.output:
        raise SystemExit("--output is only valid when building one app")

    if args.output:
        output = repo_path(args.output)
    else:
        output = BROWSER_DIR / f"{apps[0].stem}.wasm"

    if len(apps) == 1:
        wasm_relative = browser_relative(output)
        if not args.no_server and wasm_relative is None:
            raise SystemExit(
                f"when serving, --output must be under {BROWSER_DIR}: {output}"
            )

    roc_bin = command_path(args.roc_bin)
    ensure_roc(roc_bin)
    build_host()

    pages = []
    for app in apps:
        app_output = output if len(apps) == 1 else BROWSER_DIR / f"{app.stem}.wasm"
        app_output.parent.mkdir(parents=True, exist_ok=True)
        build_app(roc_bin, app, app_output, args.app_opt)
        relative = browser_relative(app_output)
        if not args.no_server and relative is None:
            raise SystemExit(
                f"when serving, app output must be under {BROWSER_DIR}: {app_output}"
            )
        if relative is not None:
            pages.append((app.stem, relative, app_title(app)))

    if args.no_server:
        return

    serve(args.port, pages)


if __name__ == "__main__":
    try:
        main()
    except subprocess.CalledProcessError as err:
        sys.exit(err.returncode)
