"""CSV, summary table and scatter plots from results.jsonl and loc.json."""
import csv
import json
import math
from pathlib import Path
import statistics

P = Path.home() / ".cache/perf"
SCENARIOS = ["cold", "warm", "root_changed", "import_changed"]
LABELS = {"cold": "Cold", "warm": "Warm", "root_changed": "Root changed", "import_changed": "Import changed"}
CMDS = ["check", "build", "test"]
CMD_TITLES = {"check": "roc check", "build": "roc build --opt=dev", "test": "roc test"}
COLORS = {"cold": "#2a78d6", "warm": "#eb6834", "root_changed": "#1baf7a", "import_changed": "#52514e"}

runs = [json.loads(line) for line in (P / "results.jsonl").read_text().splitlines()]
locs = json.loads((P / "loc.json").read_text())
extra = json.loads((P / "unmeasured.json").read_text()) if (P / "unmeasured.json").exists() else []


def loc_of(repo, root):
    entry = locs[f"{repo}|{root}"]
    own_key = next(k for k in ("Application", "Package", "Module", "Platform") if k in entry)
    own = entry[own_key]
    local = entry.get("Local Dependencies", (0, 0))
    https = entry.get("HTTPS Dependencies", (0, 0))
    total = entry.get("Total", own)
    return dict(kind=own_key, own=own[0], own_code=own[1], local_deps=local[0], https_deps=https[0], total=total[0], total_code=total[1])


# Raw CSV: one row per timed command.
with (P / "roc-build-timings-raw.csv").open("w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["project", "command", "root", "scenario", "repetition", "exit_code", "wall_seconds", "user_seconds", "system_seconds", "peak_rss_mb",
                "loc_without_deps", "loc_with_deps", "edited_import", "edited_import_importers"])
    for r in runs:
        loc = loc_of(r["repo"], r["root"])
        w.writerow([r["repo"], r["cmd"], r["root"], r["scenario"], r["rep"], r["rc"], f"{r['wall']:.4f}", f"{r['user']:.4f}", f"{r['sys']:.4f}",
                    f"{r['max_rss_kb'] / 1024:.1f}", loc["own"], loc["total"], r["import_file"] or "", r["import_importers"] or ""])

# Summary: medians over the repetitions.
groups = {}
for r in runs:
    groups.setdefault((r["repo"], r["cmd"], r["root"]), {}).setdefault(r["scenario"], []).append(r)
summary = []
for (repo, cmd, root), by in groups.items():
    loc = loc_of(repo, root)
    row = dict(project=repo, command=cmd, root=root, **loc)
    first = next(iter(by.values()))[0]
    row["edited_import"] = first["import_file"]
    row["edited_import_importers"] = first["import_importers"]
    row["reps"] = len(by["cold"])
    for s in SCENARIOS:
        xs = by.get(s)
        if not xs:
            continue
        walls = [x["wall"] for x in xs]
        row[f"{s}_median"] = statistics.median(walls)
        row[f"{s}_min"] = min(walls)
        row[f"{s}_max"] = max(walls)
        row[f"{s}_user_median"] = statistics.median(x["user"] for x in xs)
        row[f"{s}_rss_mb_median"] = statistics.median(x["max_rss_kb"] for x in xs) / 1024
    summary.append(row)
summary.sort(key=lambda r: (CMDS.index(r["command"]), r["total"]))

cols = ["project", "command", "root", "kind", "own", "own_code", "local_deps", "https_deps", "total", "total_code", "edited_import", "edited_import_importers", "reps"]
for s in SCENARIOS:
    cols += [f"{s}_median", f"{s}_min", f"{s}_max", f"{s}_user_median", f"{s}_rss_mb_median"]
with (P / "roc-build-timings-summary.csv").open("w", newline="") as f:
    w = csv.writer(f)
    names = {"own": "loc_without_deps", "own_code": "loc_without_deps_code_only", "local_deps": "loc_local_deps", "https_deps": "loc_https_deps",
             "total": "loc_with_deps", "total_code": "loc_with_deps_code_only", "kind": "root_kind"}
    w.writerow([names.get(c, c.replace("_median", "_median_seconds").replace("_min", "_min_seconds").replace("_max", "_max_seconds").replace("_user_median_seconds", "_user_median_seconds").replace("_rss_mb_median_seconds", "_peak_rss_mb_median")) for c in cols])
    for row in summary:
        w.writerow([("" if row.get(c) is None else (f"{row[c]:.4f}" if isinstance(row.get(c), float) else row[c])) for c in cols])
    for u in extra:
        w.writerow([u["project"], u["command"], u["root"], "", "", "", "", "", "", "", "", "", 0] + [u["status"]] + [""] * (len(cols) - 14))


def ms(x):
    if x is None:
        return "—"
    return f"{x * 1000:,.0f} ms" if x >= 0.1 else f"{x * 1000:.1f} ms"


lines = ["| Project | Command | Root | LOC (own) | LOC (with deps) | Cold | Warm | Root changed | Import changed | Edited import |",
         "|---|---|---|---:|---:|---:|---:|---:|---:|---|"]
for row in summary:
    imp = f"`{row['edited_import']}` ({row['edited_import_importers']} importers)" if row["edited_import"] else "— (no local imports)"
    lines.append(f"| {row['project']} | {row['command']} | `{row['root']}` | {row['own']:,} | {row['total']:,} | " +
                 " | ".join(ms(row.get(f"{s}_median")) for s in SCENARIOS) + f" | {imp} |")
(P / "roc-build-timings-table.md").write_text("\n".join(lines) + "\n")


# Charts: one SVG panel per command, a polyline per scenario, in each axis-scale variant.
W, H = 560, 420
ML, MR, MT, MB = 70, 20, 44, 56
# (file suffix, log x, log y, marker scale, marker ring width, line width) in SVG units; PNGs render at 2x.
VARIANTS = [("", True, True, 1.0, 2, 2), ("-linear-time", True, False, 1.0, 2, 2), ("-linear", False, False, 0.6, 0.5, 1)]


def nice_step(span, target=5):
    raw = span / target
    mag = 10 ** math.floor(math.log10(raw))
    for m in (1, 2, 2.5, 5, 10):
        if m * mag >= raw:
            return m * mag
    return 10 * mag


def axis(values, log):
    """(lo, hi, ticks) for one axis."""
    if log:
        lo, hi = 10 ** math.floor(math.log10(min(values))), 10 ** math.ceil(math.log10(max(values)))
        ticks, e = [], math.log10(lo)
        while e <= math.log10(hi) + 1e-9:
            ticks.append(10 ** e)
            e += 1
        return lo, hi, ticks
    step = nice_step(max(values))
    hi = math.ceil(max(values) / step) * step
    return 0.0, hi, [i * step for i in range(round(hi / step) + 1)]


def scaler(lo, hi, log, a, b):
    if log:
        return lambda v: a + (math.log10(v) - math.log10(lo)) / (math.log10(hi) - math.log10(lo)) * (b - a)
    return lambda v: a + (v - lo) / (hi - lo) * (b - a)


def marker(s, x, y, k=1.0, ring_width=2):
    c = COLORS[s]
    ring = f'stroke="#fcfcfb" stroke-width="{ring_width}"'
    if s == "cold":
        return f'<circle cx="{x:.1f}" cy="{y:.1f}" r="{5 * k:.2f}" fill="{c}" {ring}/>'
    if s == "warm":
        return f'<rect x="{x - 4.5 * k:.1f}" y="{y - 4.5 * k:.1f}" width="{9 * k:.2f}" height="{9 * k:.2f}" rx="{k:.2f}" fill="{c}" {ring}/>'
    if s == "root_changed":
        return f'<polygon points="{x:.1f},{y - 6 * k:.1f} {x + 5.5 * k:.1f},{y + 4 * k:.1f} {x - 5.5 * k:.1f},{y + 4 * k:.1f}" fill="{c}" {ring}/>'
    return f'<polygon points="{x:.1f},{y - 6 * k:.1f} {x + 6 * k:.1f},{y:.1f} {x:.1f},{y + 6 * k:.1f} {x - 6 * k:.1f},{y:.1f}" fill="{c}" {ring}/>'


def time_label(v):
    if v == 0:
        return "0"
    return f"{v * 1000:g} ms" if v < 1 else f"{v:g} s"


def loc_label(v):
    if v == 0:
        return "0"
    return f"{v / 1000:g}k" if v >= 1000 else f"{int(v):,}"


xs_all = [r["total"] for r in summary]
ys_all = [r[f"{s}_median"] for r in summary for s in SCENARIOS if r.get(f"{s}_median")]

for suffix, xlog, ylog, mk, ring_w, line_w in VARIANTS:
    panels = []
    for cmd in CMDS:
        rows = sorted((r for r in summary if r["command"] == cmd), key=lambda r: r["total"])
        # Log axes share one range across commands; linear axes fit each command's own data.
        xvals = xs_all if xlog else [r["total"] for r in rows]
        yvals = ys_all if ylog else [r[f"{s}_median"] for r in rows for s in SCENARIOS if r.get(f"{s}_median")]
        xlo, xhi, xticks = axis(xvals, xlog)
        ylo, yhi, yticks = axis(yvals, ylog)
        sx = scaler(xlo, xhi, xlog, ML, W - MR)
        sy = scaler(ylo, yhi, ylog, H - MB, MT)
        parts = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" viewBox="0 0 {W} {H}" font-family="Inter, Helvetica, Arial, sans-serif">',
                 f'<rect width="{W}" height="{H}" fill="#fcfcfb"/>',
                 f'<text x="{ML}" y="24" font-size="15" font-weight="600" fill="#0b0b0b">{CMD_TITLES[cmd]}</text>',
                 f'<text x="{W - MR}" y="24" font-size="11" fill="#52514e" text-anchor="end">{len(rows)} projects · median of 5 runs</text>']
        for v in xticks:
            parts.append(f'<line x1="{sx(v):.1f}" x2="{sx(v):.1f}" y1="{MT}" y2="{H - MB}" stroke="#e6e5e1" stroke-width="1"/>')
            parts.append(f'<text x="{sx(v):.1f}" y="{H - MB + 16}" font-size="11" fill="#52514e" text-anchor="middle">{loc_label(v)}</text>')
        for v in yticks:
            parts.append(f'<line x1="{ML}" x2="{W - MR}" y1="{sy(v):.1f}" y2="{sy(v):.1f}" stroke="#e6e5e1" stroke-width="1"/>')
            parts.append(f'<text x="{ML - 8}" y="{sy(v) + 4:.1f}" font-size="11" fill="#52514e" text-anchor="end">{time_label(v)}</text>')
        parts.append(f'<line x1="{ML}" x2="{W - MR}" y1="{H - MB}" y2="{H - MB}" stroke="#8a8984" stroke-width="1"/>')
        xname = "Lines of code including dependencies (roc loc" + (", log scale)" if xlog else ")")
        yname = "Wall time" + (" (log scale)" if ylog else "")
        parts.append(f'<text x="{(ML + W - MR) / 2:.1f}" y="{H - 14}" font-size="12" fill="#52514e" text-anchor="middle">{xname}</text>')
        parts.append(f'<text transform="translate(16 {(MT + H - MB) / 2:.1f}) rotate(-90)" font-size="12" fill="#52514e" text-anchor="middle">{yname}</text>')
        for s in SCENARIOS:
            pts = [(sx(r["total"]), sy(r[f"{s}_median"])) for r in rows if r.get(f"{s}_median")]
            if len(pts) > 1:
                d = " ".join(f"{x:.1f},{y:.1f}" for x, y in pts)
                parts.append(f'<polyline points="{d}" fill="none" stroke="{COLORS[s]}" stroke-width="{line_w}" stroke-linejoin="round" opacity="0.75"/>')
        for s in SCENARIOS:
            for r in rows:
                if r.get(f"{s}_median"):
                    parts.append(marker(s, sx(r["total"]), sy(r[f"{s}_median"]), mk, ring_w))
        lx = ML + 8
        for s in SCENARIOS:
            parts.append(marker(s, lx + 6, MT + 12))
            parts.append(f'<text x="{lx + 16}" y="{MT + 16}" font-size="11" fill="#0b0b0b">{LABELS[s]}</text>')
            lx += 16 + len(LABELS[s]) * 6.4 + 18
        parts.append("</svg>")
        svg = "\n".join(parts)
        (P / f"chart-{cmd}{suffix}.svg").write_text(svg)
        panels.append(svg)
    html = ['<!doctype html><html><head><meta charset="utf-8"><style>body{margin:0;background:#fcfcfb;display:flex;flex-direction:column;gap:0}</style></head><body>']
    html += panels
    html.append("</body></html>")
    (P / f"charts{suffix}.html").write_text("\n".join(html))
print(f"{len(runs)} runs, {len(summary)} project/command pairs")
