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


# Charts: one SVG panel per command, log-log, a polyline per scenario.
W, H = 560, 420
ML, MR, MT, MB = 70, 20, 44, 56
xs_all = [r["total"] for r in summary]
ys_all = [r[f"{s}_median"] for r in summary for s in SCENARIOS if r.get(f"{s}_median")]
xmin, xmax = 10 ** math.floor(math.log10(min(xs_all))), 10 ** math.ceil(math.log10(max(xs_all)))
ymin, ymax = 10 ** math.floor(math.log10(min(ys_all))), 10 ** math.ceil(math.log10(max(ys_all)))


def sx(v):
    return ML + (math.log10(v) - math.log10(xmin)) / (math.log10(xmax) - math.log10(xmin)) * (W - ML - MR)


def sy(v):
    return H - MB - (math.log10(v) - math.log10(ymin)) / (math.log10(ymax) - math.log10(ymin)) * (H - MT - MB)


def marker(s, x, y):
    c = COLORS[s]
    ring = 'stroke="#fcfcfb" stroke-width="2"'
    if s == "cold":
        return f'<circle cx="{x:.1f}" cy="{y:.1f}" r="5" fill="{c}" {ring}/>'
    if s == "warm":
        return f'<rect x="{x - 4.5:.1f}" y="{y - 4.5:.1f}" width="9" height="9" rx="1" fill="{c}" {ring}/>'
    if s == "root_changed":
        return f'<polygon points="{x:.1f},{y - 6:.1f} {x + 5.5:.1f},{y + 4:.1f} {x - 5.5:.1f},{y + 4:.1f}" fill="{c}" {ring}/>'
    return f'<polygon points="{x:.1f},{y - 6:.1f} {x + 6:.1f},{y:.1f} {x:.1f},{y + 6:.1f} {x - 6:.1f},{y:.1f}" fill="{c}" {ring}/>'


def tick_label(v, unit):
    if unit == "s":
        return f"{v * 1000:g} ms" if v < 1 else f"{v:g} s"
    return f"{int(v):,}"


panels = []
for cmd in CMDS:
    rows = sorted((r for r in summary if r["command"] == cmd), key=lambda r: r["total"])
    parts = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" viewBox="0 0 {W} {H}" font-family="Inter, Helvetica, Arial, sans-serif">',
             f'<rect width="{W}" height="{H}" fill="#fcfcfb"/>',
             f'<text x="{ML}" y="24" font-size="15" font-weight="600" fill="#0b0b0b">{CMD_TITLES[cmd]}</text>',
             f'<text x="{W - MR}" y="24" font-size="11" fill="#52514e" text-anchor="end">{len(rows)} projects · median of 5 runs</text>']
    e = math.log10(xmin)
    while e <= math.log10(xmax) + 1e-9:
        v = 10 ** e
        parts.append(f'<line x1="{sx(v):.1f}" x2="{sx(v):.1f}" y1="{MT}" y2="{H - MB}" stroke="#e6e5e1" stroke-width="1"/>')
        parts.append(f'<text x="{sx(v):.1f}" y="{H - MB + 16}" font-size="11" fill="#52514e" text-anchor="middle">{tick_label(v, "loc")}</text>')
        e += 1
    e = math.log10(ymin)
    while e <= math.log10(ymax) + 1e-9:
        v = 10 ** e
        parts.append(f'<line x1="{ML}" x2="{W - MR}" y1="{sy(v):.1f}" y2="{sy(v):.1f}" stroke="#e6e5e1" stroke-width="1"/>')
        parts.append(f'<text x="{ML - 8}" y="{sy(v) + 4:.1f}" font-size="11" fill="#52514e" text-anchor="end">{tick_label(v, "s")}</text>')
        e += 1
    parts.append(f'<line x1="{ML}" x2="{W - MR}" y1="{H - MB}" y2="{H - MB}" stroke="#8a8984" stroke-width="1"/>')
    parts.append(f'<text x="{(ML + W - MR) / 2:.1f}" y="{H - 14}" font-size="12" fill="#52514e" text-anchor="middle">Lines of code including dependencies (roc loc, log scale)</text>')
    parts.append(f'<text transform="translate(16 {(MT + H - MB) / 2:.1f}) rotate(-90)" font-size="12" fill="#52514e" text-anchor="middle">Wall time (log scale)</text>')
    for s in SCENARIOS:
        pts = [(sx(r["total"]), sy(r[f"{s}_median"])) for r in rows if r.get(f"{s}_median")]
        if len(pts) > 1:
            d = " ".join(f"{x:.1f},{y:.1f}" for x, y in pts)
            parts.append(f'<polyline points="{d}" fill="none" stroke="{COLORS[s]}" stroke-width="2" stroke-linejoin="round" opacity="0.75"/>')
    for s in SCENARIOS:
        for r in rows:
            if r.get(f"{s}_median"):
                parts.append(marker(s, sx(r["total"]), sy(r[f"{s}_median"])))
    # Legend.
    lx = ML + 8
    for s in SCENARIOS:
        parts.append(marker(s, lx + 6, MT + 12))
        parts.append(f'<text x="{lx + 16}" y="{MT + 16}" font-size="11" fill="#0b0b0b">{LABELS[s]}</text>')
        lx += 16 + len(LABELS[s]) * 6.4 + 18
    parts.append("</svg>")
    svg = "\n".join(parts)
    (P / f"chart-{cmd}.svg").write_text(svg)
    panels.append(svg)

html = ['<!doctype html><html><head><meta charset="utf-8"><style>body{margin:0;background:#fcfcfb;display:flex;flex-direction:column;gap:0}</style></head><body>']
html += panels
html.append("</body></html>")
(P / "charts.html").write_text("\n".join(html))
print(f"{len(runs)} runs, {len(summary)} project/command pairs")
