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
# `/api/ops/dashboard` is the typed app contract. It returns a schema-versioned
# UTF-8 name/value line protocol:
#
#   key=value\n
#
# Values do not contain newlines. Field order is not meaningful. The Roc app
# decodes this explicit contract into a concrete `Dashboard` record before
# rendering.
#
# The section endpoints remain UTF-8 text diagnostics for manual QA. Every
# response is deterministic simulated data derived from the current time. The
# version advances every two seconds so the dashboard can exercise async refresh
# behavior without external dependencies:
#
#   /api/ops/dashboard name-keyed dashboard line protocol
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
        "--example",
        choices=SUITE_STEMS,
        help=(
            "Build and serve one maintained example by stem, for example "
            "ops_dashboard. Defaults to the full suite."
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
    args = parser.parse_args()
    if args.app and args.example:
        parser.error("pass either --example or an app path, not both")
    return args


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


def triangle_wave(step, period, amplitude):
    pos = step % period
    half = period // 2
    if pos <= half:
        return (pos * amplitude) // half
    return ((period - pos) * amplitude) // (period - half)


def clamp(value, low, high):
    return max(low, min(high, value))


def bar_code(value, scale):
    return clamp((value * 8) // scale, 0, 8)


def health_state_text(code):
    if code == 2:
        return "degraded"
    if code == 1:
        return "watch"
    return "ok"


def job_state_text(code):
    if code == 3:
        return "blocked"
    if code == 2:
        return "retrying"
    if code == 1:
        return "queued"
    return "running"


def ops_api_snapshot(now=None):
    timestamp = int(time.time() if now is None else now)
    version = timestamp // 2
    clock = datetime.datetime.fromtimestamp(timestamp, datetime.timezone.utc)
    cycle = version % 90
    incident_active = 32 <= cycle < 44
    recovery = 44 <= cycle < 58
    traffic_wave = triangle_wave(version, 60, 520)
    deploy_wave = triangle_wave(version + 17, 28, 120)
    incident_ramp = clamp(cycle - 31, 0, 12)
    recovery_ramp = clamp(58 - cycle, 0, 14)
    requests_per_minute = 1120 + traffic_wave + deploy_wave + (160 if incident_active else 0)
    latency_pressure = traffic_wave // 26 + deploy_wave // 10
    incident_latency = incident_ramp * 9 if incident_active else recovery_ramp * 4 if recovery else 0
    latency_ms = 58 + latency_pressure + incident_latency
    error_permille = 3 + traffic_wave // 130 + (incident_ramp * 4 if incident_active else recovery_ramp)
    queue_base = 22 + traffic_wave // 36 + ((version * 5) % 18)
    incident_queue = incident_ramp * 6 if incident_active else recovery_ramp * 3 if recovery else 0
    queue_depth = queue_base + incident_queue
    running_jobs = 9 + (version % 4) - (1 if incident_active else 0)
    blocked_jobs = (2 if incident_active else 1 if queue_depth > 58 else 0)
    worker_state_code = 2 if queue_depth > 88 else 1 if queue_depth > 58 else 0
    api_state_code = 2 if incident_active else 1 if latency_ms > 145 else 0
    billing_state_code = 2 if incident_active else 1 if recovery else 0
    database_state_code = 1 if queue_depth > 70 else 0
    edge_state_code = 1 if requests_per_minute > 1650 else 0
    search_state_code = 1 if version % 23 in (18, 19, 20) else 0
    identity_state_code = 0
    degraded_count = sum(
        1
        for code in (
            edge_state_code,
            api_state_code,
            worker_state_code,
            database_state_code,
            billing_state_code,
            search_state_code,
            identity_state_code,
        )
        if code == 2
    )
    watch_count = sum(
        1
        for code in (
            edge_state_code,
            api_state_code,
            worker_state_code,
            database_state_code,
            billing_state_code,
            search_state_code,
            identity_state_code,
        )
        if code == 1
    )
    phase_code = 2 if incident_active else 3 if recovery else 1 if watch_count else 0
    incidents = 1 if incident_active else 0
    tone_code = 2 if degraded_count else 1 if watch_count or queue_depth > 58 else 0
    overall_code = 1 if degraded_count else 0
    healthy_services = 7 - degraded_count
    traffic_delta_percent = 2 + triangle_wave(version + 9, 18, 13) + (5 if incident_active else 0)
    burn_rate_x10 = 7 + error_permille // 2 + (22 if incident_active else 6 if recovery else 0)
    budget_remaining_permille = clamp(995 - (version % 140) - (incident_ramp * 9), 820, 999)
    oldest_job_min = 5 + queue_depth // 11
    webhook_rpm = 610 + requests_per_minute // 4 + triangle_wave(version + 5, 24, 90)
    db_write_rpm = 390 + requests_per_minute // 5 + triangle_wave(version + 11, 20, 75)
    job_b_state_code = 1 if queue_depth > 52 else 0
    job_d_state_code = 3 if incident_active and cycle % 3 == 0 else 2 if incident_active else 0
    job_d_progress = 38 + ((version * 4) % 44)
    if job_d_state_code == 3:
        job_d_progress = clamp(job_d_progress, 45, 74)
    alert_a_code = 1 if incident_active else 2
    alert_b_code = 2 if incident_active else 4
    alert_c_code = 3 if incident_active else 5
    return {
        "version": version,
        "updated": clock.strftime("%H:%M:%S UTC"),
        "updated_hour": clock.hour,
        "updated_minute": clock.minute,
        "updated_second": clock.second,
        "cycle": cycle,
        "incident_active": incident_active,
        "recovery": recovery,
        "phase_code": phase_code,
        "incidents": incidents,
        "tone_code": tone_code,
        "overall_code": overall_code,
        "traffic_wave": traffic_wave,
        "requests_per_minute": requests_per_minute,
        "traffic_delta_percent": traffic_delta_percent,
        "latency_ms": latency_ms,
        "latency_target_ms": 120,
        "error_permille": error_permille,
        "burn_rate_x10": burn_rate_x10,
        "budget_remaining_permille": budget_remaining_permille,
        "ingress_bar_code": bar_code(max(0, requests_per_minute - 1000), 900),
        "latency_bar_code": bar_code(latency_ms, 220),
        "error_bar_code": bar_code(error_permille, 80),
        "budget_bar_code": bar_code(1000 - budget_remaining_permille, 200),
        "webhook_rpm": webhook_rpm,
        "webhook_bar_code": bar_code(max(0, webhook_rpm - 780), 480),
        "db_write_rpm": db_write_rpm,
        "db_write_bar_code": bar_code(max(0, db_write_rpm - 520), 360),
        "queue_depth": queue_depth,
        "queue_trend_code": 2 if incident_active else 0 if recovery else 1,
        "queue_capacity": 120,
        "running_jobs": running_jobs,
        "blocked_jobs": blocked_jobs,
        "oldest_job_min": oldest_job_min,
        "job_a_id": version % 1000,
        "job_a_progress": 18 + ((version * 7) % 78),
        "job_a_age_min": 2 + version % 8,
        "job_a_state_code": 0,
        "job_b_id": (version + 17) % 1000,
        "job_b_progress": 0 if job_b_state_code == 1 else 12 + ((version * 5) % 71),
        "job_b_age_min": 4 + version % 11,
        "job_b_state_code": job_b_state_code,
        "job_c_id": (version + 31) % 1000,
        "job_c_progress": 23 + ((version * 9) % 70),
        "job_c_age_min": 1 + version % 6,
        "job_c_state_code": 0,
        "job_d_id": (version + 71) % 1000,
        "job_d_progress": job_d_progress,
        "job_d_age_min": 5 + version % 7,
        "job_d_state_code": job_d_state_code,
        "alert_a_code": alert_a_code,
        "alert_a_age_min": (4 + version % 9) if incident_active else (3 + version % 8),
        "alert_b_code": alert_b_code,
        "alert_b_age_min": (7 + version % 13) if incident_active else (2 + version % 6),
        "alert_c_code": alert_c_code,
        "alert_c_age_min": (1 + version % 5) if incident_active else (9 + version % 13),
        "healthy_services": healthy_services,
        "total_services": 7,
        "edge_state_code": edge_state_code,
        "edge_latency_ms": 44 + traffic_wave // 44 + (8 if edge_state_code else 0),
        "api_state_code": api_state_code,
        "api_latency_ms": latency_ms,
        "worker_state_code": worker_state_code,
        "worker_oldest_job_min": oldest_job_min,
        "database_state_code": database_state_code,
        "database_lag_sec": (version % 4) + (3 if database_state_code else 0),
        "billing_state_code": billing_state_code,
        "billing_latency_ms": 88 + deploy_wave // 7 + (75 if billing_state_code == 2 else 18 if billing_state_code == 1 else 0),
        "search_state_code": search_state_code,
        "search_refresh_sec": 11 + (version % 12) + (14 if search_state_code else 0),
        "identity_state_code": identity_state_code,
        "identity_latency_ms": 39 + version % 9,
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
    recovery = snap["recovery"]
    queue_depth = snap["queue_depth"]
    requests = snap["requests_per_minute"]
    error_rate = f"{snap['error_permille'] / 10:.1f}%"
    latency = snap["latency_ms"]

    if path == "/api/ops/dashboard":
        return None

    if path == "/api/ops/summary":
        overall = "Degraded" if snap["overall_code"] else "Nominal"
        tone = "bad" if snap["tone_code"] == 2 else ("warn" if snap["tone_code"] == 1 else "good")
        phase = "active incident" if incident_active else ("recovering" if recovery else "steady")
        return [
            f"Updated: {updated}  version {version}",
            f"Overall: {overall}  phase {phase}  incidents {snap['incidents']}  tone {tone}",
            f"Traffic: {requests:,} rpm  {snap['traffic_delta_percent']}% over 5m",
            f"Errors: {error_rate}  budget burn {snap['burn_rate_x10'] / 10:.1f}x",
            f"Queue: {queue_depth} jobs  running {snap['running_jobs']}  blocked {snap['blocked_jobs']}",
            f"Services: {snap['healthy_services']}/7 healthy  primary region usw2",
        ]

    if path == "/api/ops/traffic":
        return [
            f"Ingress        {requests:,} rpm  {ops_bar(snap['ingress_bar_code'])}  +{snap['traffic_delta_percent']}% over 5m",
            f"API p95        {latency} ms     {ops_bar(snap['latency_bar_code'])}  target 120 ms",
            f"Error rate     {error_rate}       {ops_bar(snap['error_bar_code'])}  budget burn {snap['burn_rate_x10'] / 10:.1f}x",
            f"Webhook fanout {snap['webhook_rpm']:,} rpm    {ops_bar(version + 2)}  retries coherent with api",
            f"DB writes      {snap['db_write_rpm']:,} rpm    {ops_bar(version + 6)}  replica lag {snap['database_lag_sec']}s",
        ]

    if path == "/api/ops/jobs":
        return [
            f"job-{snap['job_a_id']:03d}  running  {snap['job_a_progress']:>3}%  workers/search  {snap['job_a_age_min']}m  Rebuild search index",
            f"job-{snap['job_b_id']:03d}  {job_state_text(snap['job_b_state_code']):<8} {snap['job_b_progress']:>3}%  billing         {snap['job_b_age_min']}m  Backfill billing events",
            f"job-{snap['job_c_id']:03d}  running  {snap['job_c_progress']:>3}%  compliance      {snap['job_c_age_min']}m  Export audit archive",
            f"job-{snap['job_d_id']:03d}  {job_state_text(snap['job_d_state_code']):<8} {snap['job_d_progress']:>3}%  identity        {snap['job_d_age_min']}m  Prune stale sessions",
        ]

    if path == "/api/ops/alerts":
        if incident_active:
            return [
                f"CRITICAL payments-api active     {snap['alert_a_age_min']}m  Checkout latency above SLO",
                f"WARNING  workers      active     {snap['alert_b_age_min']}m  Queue age approaching cap",
                f"INFO     edge         monitoring {snap['alert_c_age_min']}m  Canary pool shifted 10 percent",
            ]
        return [
            f"WARNING workers      monitoring {snap['alert_a_age_min']}m  Retry queue elevated",
            f"INFO    payments-api recovering {snap['alert_b_age_min']}m  Error budget burn back below 1x",
            f"INFO    edge         steady     {snap['alert_c_age_min']}m  Canary pool normal",
        ]

    if path == "/api/ops/health":
        api_state = health_state_text(snap["api_state_code"])
        worker_state = health_state_text(snap["worker_state_code"])
        return [
            f"edge      {health_state_text(snap['edge_state_code']):<8} p95 {snap['edge_latency_ms']} ms   8 pods    all regions serving",
            f"api       {api_state:<8} p95 {snap['api_latency_ms']} ms  12 pods    primary deploy api-{version % 100:02d}",
            f"workers   {worker_state:<8} oldest {snap['worker_oldest_job_min']}m  24 slots   {queue_depth} queued jobs",
            f"database  {health_state_text(snap['database_state_code']):<8} lag {snap['database_lag_sec']}s      2 writers  failover warm",
            f"billing   {health_state_text(snap['billing_state_code']):<8} p95 {snap['billing_latency_ms']} ms   6 pods    webhooks draining",
            f"search    {health_state_text(snap['search_state_code']):<8} refresh {snap['search_refresh_sec']}s 5 shards   index green",
            f"identity  {health_state_text(snap['identity_state_code']):<8} p95 {snap['identity_latency_ms']} ms   4 pods    session cache hot",
        ]

    return None


def ops_dashboard_fields(now=None):
    source_now = time.time() if now is None else now
    snap = ops_api_snapshot(source_now)

    return {
        "schema": 1,
        "updated_version": snap["version"],
        "updated_hour": snap["updated_hour"],
        "updated_minute": snap["updated_minute"],
        "updated_second": snap["updated_second"],
        "phase_code": snap["phase_code"],
        "requests_per_minute": snap["requests_per_minute"],
        "traffic_delta_percent": snap["traffic_delta_percent"],
        "error_permille": snap["error_permille"],
        "burn_rate_x10": snap["burn_rate_x10"],
        "budget_remaining_permille": snap["budget_remaining_permille"],
        "latency_ms": snap["latency_ms"],
        "latency_target_ms": snap["latency_target_ms"],
        "webhook_rpm": snap["webhook_rpm"],
        "webhook_bar_code": snap["webhook_bar_code"],
        "db_write_rpm": snap["db_write_rpm"],
        "db_write_bar_code": snap["db_write_bar_code"],
        "ingress_bar_code": snap["ingress_bar_code"],
        "latency_bar_code": snap["latency_bar_code"],
        "error_bar_code": snap["error_bar_code"],
        "budget_bar_code": snap["budget_bar_code"],
        "queue_depth": snap["queue_depth"],
        "queue_trend_code": snap["queue_trend_code"],
        "queue_capacity": snap["queue_capacity"],
        "running_jobs": snap["running_jobs"],
        "blocked_jobs": snap["blocked_jobs"],
        "oldest_job_min": snap["oldest_job_min"],
        "job_a_id": snap["job_a_id"],
        "job_a_progress": snap["job_a_progress"],
        "job_a_age_min": snap["job_a_age_min"],
        "job_a_state_code": snap["job_a_state_code"],
        "job_b_id": snap["job_b_id"],
        "job_b_progress": snap["job_b_progress"],
        "job_b_age_min": snap["job_b_age_min"],
        "job_b_state_code": snap["job_b_state_code"],
        "job_c_id": snap["job_c_id"],
        "job_c_progress": snap["job_c_progress"],
        "job_c_age_min": snap["job_c_age_min"],
        "job_c_state_code": snap["job_c_state_code"],
        "job_d_id": snap["job_d_id"],
        "job_d_progress": snap["job_d_progress"],
        "job_d_age_min": snap["job_d_age_min"],
        "job_d_state_code": snap["job_d_state_code"],
        "alert_a_code": snap["alert_a_code"],
        "alert_a_age_min": snap["alert_a_age_min"],
        "alert_b_code": snap["alert_b_code"],
        "alert_b_age_min": snap["alert_b_age_min"],
        "alert_c_code": snap["alert_c_code"],
        "alert_c_age_min": snap["alert_c_age_min"],
        "edge_state_code": snap["edge_state_code"],
        "edge_latency_ms": snap["edge_latency_ms"],
        "api_state_code": snap["api_state_code"],
        "api_latency_ms": snap["api_latency_ms"],
        "worker_state_code": snap["worker_state_code"],
        "worker_oldest_job_min": snap["worker_oldest_job_min"],
        "database_state_code": snap["database_state_code"],
        "database_lag_sec": snap["database_lag_sec"],
        "billing_state_code": snap["billing_state_code"],
        "billing_latency_ms": snap["billing_latency_ms"],
        "search_state_code": snap["search_state_code"],
        "search_refresh_sec": snap["search_refresh_sec"],
        "identity_state_code": snap["identity_state_code"],
        "identity_latency_ms": snap["identity_latency_ms"],
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


def suite_app(stem):
    for app in APP_SUITE:
        if app.stem == stem:
            return app

    raise SystemExit(f"unknown Signals example: {stem}")


def selected_apps(args):
    if args.example:
        return [suite_app(args.example)]

    if args.app:
        return [repo_path(args.app)]

    return list(default_apps())


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

    apps = selected_apps(args)
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
