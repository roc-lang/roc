#!/usr/bin/env python3
import argparse
import datetime
import functools
import http.server
import os
import pathlib
import shlex
import shutil
import subprocess
import sys
import time
import urllib.parse


SCRIPT_PATH = pathlib.Path(__file__).resolve()
SIGNALS_DIR = SCRIPT_PATH.parent
REPO_ROOT = SIGNALS_DIR.parent.parent
BROWSER_DIR = SIGNALS_DIR / "browser"
DEFAULT_ROC_BIN = REPO_ROOT / "zig-out" / "bin" / "roc"
TAILWIND_INPUT = BROWSER_DIR / "signals.tailwind.css"
TAILWIND_OUTPUT = BROWSER_DIR / "signals.generated.css"
TAILWIND_CONFIG = SIGNALS_DIR / "tailwind.config.js"
APP_SUITE = (
    SIGNALS_DIR / "apps" / "ops_dashboard.roc",
    SIGNALS_DIR / "apps" / "checkout_wizard.roc",
    SIGNALS_DIR / "apps" / "kanban_board.roc",
    SIGNALS_DIR / "apps" / "identity_stress.roc",
    SIGNALS_DIR / "apps" / "component_composition.roc",
    SIGNALS_DIR / "apps" / "async_effects.roc",
)
SUITE_STEMS = tuple(app.stem for app in APP_SUITE)

# API contract for the ops dashboard.
#
# `/api/ops/dashboard` is the typed app contract. It returns a fixed-order
# UTF-8 line protocol:
#
#   key=value\n
#
# Values do not contain newlines. The Roc app decodes this explicit contract
# into a concrete `Dashboard` record before rendering.
#
# The section endpoints remain UTF-8 text diagnostics for manual QA. Every
# response is deterministic simulated data derived from the current time. The
# version advances every two seconds so the dashboard can exercise async refresh
# behavior without external dependencies:
#
#   /api/ops/dashboard fixed-order dashboard line protocol
#   /api/ops/summary   status lines with updated/version/rollup values
#   /api/ops/traffic   metric trend rows with an ASCII bar
#   /api/ops/jobs      active job rows with id/state/progress/owner/age
#   /api/ops/alerts    incident/alert rows with severity/service/state/age
#   /api/ops/health    service health rows with state/latency/capacity/detail
#
# `version` and updated timestamps are explicit fields so successive responses
# can be correlated during manual QA.
OPS_API_PATHS = {
    "/api/ops/dashboard",
    "/api/ops/summary",
    "/api/ops/traffic",
    "/api/ops/jobs",
    "/api/ops/alerts",
    "/api/ops/health",
}


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
        default=os.environ.get("APP_OPT", "dev"),
        help="Roc --opt mode for the app build. Defaults to APP_OPT or dev.",
    )
    parser.add_argument(
        "--roc-bin",
        default=os.environ.get("ROC_BIN", "zig-out/bin/roc"),
        help="Roc compiler path. Defaults to ROC_BIN or ./zig-out/bin/roc.",
    )
    parser.add_argument(
        "--tailwind-bin",
        default=os.environ.get("TAILWIND_BIN", "tailwindcss"),
        help="Tailwind CSS standalone CLI path. Defaults to TAILWIND_BIN or tailwindcss.",
    )
    parser.add_argument(
        "--skip-tailwind",
        action="store_true",
        default=os.environ.get("SKIP_TAILWIND") == "1",
        help="Skip generating test/signals/browser/signals.generated.css.",
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

    return path


def command_arg(value):
    if isinstance(value, pathlib.Path):
        try:
            return str(value.relative_to(REPO_ROOT))
        except ValueError:
            return str(value)
    return str(value)


def repo_display(path):
    try:
        return str(path.relative_to(REPO_ROOT))
    except ValueError:
        return str(path)


def browser_relative(path):
    resolved = path.resolve()
    try:
        return resolved.relative_to(BROWSER_DIR)
    except ValueError:
        return None


def ops_api_snapshot(now=None):
    timestamp = int(time.time() if now is None else now)
    version = timestamp // 2
    wave = version % 12
    slow_wave = (version // 3) % 8
    incident_active = wave in (7, 8, 9)
    queue_depth = 18 + ((version * 7) % 37)
    requests_per_minute = 980 + wave * 41 + (slow_wave * 13)
    error_permille = 4 + ((version + slow_wave) % 9) + (7 if incident_active else 0)
    latency_ms = 72 + ((version * 11) % 55) + (35 if incident_active else 0)
    running_jobs = 8 + (version % 5)
    healthy_services = 6 if incident_active else 7
    updated = datetime.datetime.fromtimestamp(timestamp, datetime.timezone.utc).strftime(
        "%H:%M:%S UTC"
    )
    return {
        "version": version,
        "updated": updated,
        "wave": wave,
        "slow_wave": slow_wave,
        "incident_active": incident_active,
        "queue_depth": queue_depth,
        "requests_per_minute": requests_per_minute,
        "error_permille": error_permille,
        "latency_ms": latency_ms,
        "running_jobs": running_jobs,
        "healthy_services": healthy_services,
    }


def ops_bar(seed, width=12):
    filled = 3 + (seed % (width - 3))
    return "#" * filled + "-" * (width - filled)


def ops_api_rows(path, now=None):
    source_now = time.time() if now is None else now
    snap = ops_api_snapshot(source_now)
    version = snap["version"]
    updated = snap["updated"]
    incident_active = snap["incident_active"]
    queue_depth = snap["queue_depth"]
    requests = snap["requests_per_minute"]
    error_rate = f"{snap['error_permille'] / 10:.1f}%"
    latency = snap["latency_ms"]

    if path == "/api/ops/dashboard":
        return None

    if path == "/api/ops/summary":
        overall = "Degraded" if incident_active else "Nominal"
        tone = "bad" if incident_active else ("warn" if queue_depth > 42 else "good")
        return [
            f"Updated: {updated}  version {version}",
            f"Overall: {overall}  incidents {1 if incident_active else 0}  tone {tone}",
            f"Traffic: {requests:,} rpm  edge and api ingress",
            f"Errors: {error_rate}  rolling 5 minute rate",
            f"Queue: {queue_depth} jobs  running {snap['running_jobs']}",
            f"Services: {snap['healthy_services']}/7 healthy  primary region usw2",
        ]

    if path == "/api/ops/traffic":
        return [
            f"Ingress        {requests:,} rpm  {ops_bar(version)}  +{snap['wave'] + 2}% over 5m",
            f"API p95        {latency} ms     {ops_bar(version + 4)}  target 120 ms",
            f"Error rate     {error_rate}       {ops_bar(version + 8)}  budget burn {1 + (version % 4)}x",
            f"Webhook fanout {620 + (version % 9) * 17:,} rpm    {ops_bar(version + 2)}  retry pool steady",
            f"DB writes      {440 + (version % 6) * 23:,} rpm    {ops_bar(version + 6)}  replica lag {version % 4}s",
        ]

    if path == "/api/ops/jobs":
        progress_a = 28 + ((version * 9) % 63)
        progress_b = 15 + ((version * 7) % 72)
        progress_c = 44 + ((version * 5) % 49)
        return [
            f"job-{version % 1000:03d}  running  {progress_a:>3}%  workers/search  {2 + version % 8}m  Rebuild search index",
            f"job-{(version + 17) % 1000:03d}  queued     0%  billing         {4 + version % 11}m  Backfill billing events",
            f"job-{(version + 31) % 1000:03d}  running  {progress_b:>3}%  compliance      {1 + version % 6}m  Export audit archive",
            f"job-{(version + 53) % 1000:03d}  running  {progress_c:>3}%  ml-platform     {3 + version % 9}m  Warm recommendations cache",
            f"job-{(version + 71) % 1000:03d}  retrying {20 + version % 45:>3}%  identity        {5 + version % 7}m  Prune stale sessions",
        ]

    if path == "/api/ops/alerts":
        if incident_active:
            return [
                f"CRITICAL payments-api active     {4 + version % 9}m  Checkout latency above SLO",
                f"WARNING  workers      active     {7 + version % 13}m  Queue age approaching cap",
                f"INFO     edge         monitoring {1 + version % 5}m  Canary pool shifted 10 percent",
            ]
        return [
            f"WARNING workers      monitoring {3 + version % 8}m  Retry queue elevated",
            f"INFO    payments-api recovering {2 + version % 6}m  Error budget burn back below 1x",
        ]

    if path == "/api/ops/health":
        api_state = "degraded" if incident_active else "ok"
        worker_state = "watch" if queue_depth > 42 else "ok"
        return [
            f"edge      ok        p95 {48 + version % 10} ms   8 pods    all regions serving",
            f"api       {api_state:<8} p95 {latency} ms  12 pods    primary deploy api-{version % 100:02d}",
            f"workers   {worker_state:<8} oldest {5 + version % 12}m  24 slots   {queue_depth} queued jobs",
            f"database  ok        lag {version % 4}s      2 writers  failover warm",
            f"billing   {'degraded' if incident_active else 'ok':<8} p95 {92 + version % 20} ms   6 pods    webhooks draining",
            f"search    ok        refresh {12 + version % 9}s 5 shards   index green",
            f"identity  ok        p95 {41 + version % 7} ms   4 pods    session cache hot",
        ]

    return None


def ops_dashboard_fields(now=None):
    source_now = time.time() if now is None else now
    snap = ops_api_snapshot(source_now)
    version = snap["version"]
    incident_active = snap["incident_active"]
    queue_depth = snap["queue_depth"]
    requests = snap["requests_per_minute"]
    latency = snap["latency_ms"]

    return {
        "updated_version": version,
        "overall_code": 1 if incident_active else 0,
        "tone_code": 2 if incident_active else (1 if queue_depth > 42 else 0),
        "incidents": 1 if incident_active else 0,
        "requests_per_minute": requests,
        "error_permille": snap["error_permille"],
        "latency_ms": latency,
        "webhook_rpm": 620 + (version % 9) * 17,
        "db_write_rpm": 440 + (version % 6) * 23,
        "ingress_bar_code": version % 9,
        "latency_bar_code": (version + 4) % 9,
        "error_bar_code": (version + 8) % 9,
        "queue_depth": queue_depth,
        "running_jobs": snap["running_jobs"],
        "oldest_job_min": 5 + version % 12,
        "job_a_id": version % 1000,
        "job_a_progress": 28 + ((version * 9) % 63),
        "job_a_age_min": 2 + version % 8,
        "job_b_id": (version + 17) % 1000,
        "job_b_age_min": 4 + version % 11,
        "job_c_id": (version + 31) % 1000,
        "job_c_progress": 15 + ((version * 7) % 72),
        "job_c_age_min": 1 + version % 6,
        "alert_a_age_min": (4 + version % 9) if incident_active else (3 + version % 8),
        "alert_b_age_min": (7 + version % 13) if incident_active else (2 + version % 6),
        "healthy_services": snap["healthy_services"],
        "total_services": 7,
        "api_state_code": 2 if incident_active else 0,
        "worker_state_code": 1 if queue_depth > 42 else 0,
        "billing_state_code": 2 if incident_active else 0,
    }


def ops_api_dashboard_body(now=None):
    rows = ops_dashboard_fields(now)
    body = "\n".join(f"{key}={value}" for key, value in rows.items())
    return body.encode("utf-8")


def ops_api_body(path, now=None):
    if path == "/api/ops/dashboard":
        return ops_api_dashboard_body(now)

    rows = ops_api_rows(path, now)
    if rows is None:
        return None
    return "\n".join(rows).encode("utf-8")


def ops_api_content_type(path):
    return "text/plain; charset=utf-8"


def run(command):
    command = [command_arg(part) for part in command]
    quoted = " ".join(shlex.quote(part) for part in command)
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


def ensure_tailwind(tailwind_bin):
    if tailwind_bin.is_absolute() or len(tailwind_bin.parts) > 1:
        if tailwind_bin.exists() and os.access(tailwind_bin, os.X_OK):
            return
    elif shutil.which(str(tailwind_bin)):
        return

    raise SystemExit(
        "missing Tailwind CSS CLI: install the standalone tailwindcss executable "
        "or set TAILWIND_BIN"
    )


def build_css(tailwind_bin):
    run(
        [
            tailwind_bin,
            "-c",
            TAILWIND_CONFIG,
            "-i",
            TAILWIND_INPUT,
            "-o",
            TAILWIND_OUTPUT,
            "--minify",
        ]
    )


def build_app(roc_bin, app, output, app_opt):
    run(
        [
            roc_bin,
            "build",
            "--target=wasm32",
            f"--opt={app_opt}",
            "--no-cache",
            f"--output={command_arg(output)}",
            app,
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


class SignalsRequestHandler(http.server.SimpleHTTPRequestHandler):
    def do_GET(self):
        path = urllib.parse.urlparse(self.path).path
        if path.startswith("/api/ops/"):
            self.serve_ops_api(path)
            return

        super().do_GET()

    def serve_ops_api(self, path):
        body = ops_api_body(path)
        if body is None:
            self.send_error(404, "unknown ops api endpoint")
            return

        self.send_response(200)
        self.send_header("Content-Type", ops_api_content_type(path))
        self.send_header("Cache-Control", "no-store")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)


def serve(port, pages):
    if len(pages) == len(SUITE_STEMS) and tuple(page[0] for page in pages) == SUITE_STEMS:
        page_url = f"http://localhost:{port}/"
    else:
        page_url = app_page_url(port, pages[0][1], pages[0][2])

    handler = functools.partial(
        SignalsRequestHandler,
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
            raise SystemExit(f"missing app: {repo_display(app)}")

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
                "when serving, --output must be under "
                f"{repo_display(BROWSER_DIR)}: {repo_display(output)}"
            )

    roc_bin = command_path(args.roc_bin)
    ensure_roc(roc_bin)
    tailwind_bin = command_path(args.tailwind_bin)
    if not args.skip_tailwind:
        ensure_tailwind(tailwind_bin)
        build_css(tailwind_bin)
    build_host()

    pages = []
    for app in apps:
        app_output = output if len(apps) == 1 else BROWSER_DIR / f"{app.stem}.wasm"
        app_output.parent.mkdir(parents=True, exist_ok=True)
        build_app(roc_bin, app, app_output, args.app_opt)
        relative = browser_relative(app_output)
        if not args.no_server and relative is None:
            raise SystemExit(
                "when serving, app output must be under "
                f"{repo_display(BROWSER_DIR)}: {repo_display(app_output)}"
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
