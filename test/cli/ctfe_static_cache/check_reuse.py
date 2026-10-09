"""Run the static-root cache regression directly, without platform host builds."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile


parser = argparse.ArgumentParser()
parser.add_argument("roc", type=Path)
parser.add_argument("--evidence", type=Path)
options = parser.parse_args()
binary = options.roc.resolve()
fixture = Path(__file__).resolve().parent
root = (
    options.evidence.resolve()
    if options.evidence
    else Path(tempfile.mkdtemp(prefix="ctfe-static-cache-"))
)
root.mkdir(parents=True, exist_ok=True)
source = root / "source"
source.mkdir()
for path in fixture.glob("*.roc"):
    shutil.copy2(path, source / path.name)
cache = root / "cache"
base_env = {key: value for key, value in os.environ.items() if not key.startswith("ROC_")}
base_env["ROC_CACHE_DIR"] = str(cache)
results = []


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


helper_before = digest(source / "Static.roc")
binary_before = digest(binary)


def run(label, name, *, trace=False, no_cache=False, expected=0):
    env = dict(base_env)
    if trace:
        env["ROC_PACK_TRACE"] = "1"
    command = [str(binary), "check", name, "--no-color"]
    if no_cache:
        command.append("--no-cache")
    result = subprocess.run(
        command, cwd=source, env=env, capture_output=True, text=True, timeout=60
    )
    (root / f"{label}.stdout").write_text(result.stdout)
    (root / f"{label}.stderr").write_text(result.stderr)
    results.append({"label": label, "command": command, "returncode": result.returncode})
    (root / "commands.json").write_text(json.dumps(results, indent=2))
    assert result.returncode == expected, f"{label}: {result.stdout}\n{result.stderr}"
    return result


seed = run("seed", "Probe.roc", trace=True)
offers = re.findall(
    r"^offer ([0-9a-f]{64}) [0-9a-f]+ Static\.read\s*$",
    seed.stderr,
    flags=re.MULTILINE,
)
assert offers, "Seed trace did not establish a published Static.read specialization"

probe = source / "Probe.roc"
text = probe.read_text()
assert text.count("Static.read(0) + Static.read(1)") == 1
text = text.replace("Static.read(0) + Static.read(1)", "Static.read(1) + Static.read(2)")
text = text.replace("233.I64", "235.I64")
text = text.replace("import Static\n", "import Static\n\npadding : List(I64)\npadding = [101.I64, 103]\n")
probe.write_text(text)
assert digest(source / "Static.roc") == helper_before
consumer = run("edited-consumer", "Probe.roc", trace=True)
hit_keys = set(re.findall(r"lookup monotype key=([0-9a-f]+) hit", consumer.stderr))
assert any(key[:16] in hit_keys for key in offers), (
    "Edited consumer did not establish reuse of the published Static.read offer"
)

# The same unchanged helper must preserve both ordinary crashes and failure
# messages from a guarded hoist, not merely produce the correct success value.
for name, marker in (
    ("Overflow", "Integer addition overflowed"),
    ("FailedGuard", "I64 division by zero"),
):
    uncached = run(f"{name}-uncached", f"{name}.roc", no_cache=True, expected=1)
    cached = run(f"{name}-cached", f"{name}.roc", expected=1)
    repeated = run(f"{name}-cached-repeat", f"{name}.roc", expected=1)
    baseline = uncached.stdout + uncached.stderr
    assert marker in baseline and "module Static" in baseline
    assert cached.stdout + cached.stderr == baseline, f"{name}: cached diagnostic changed"
    assert repeated.stdout + repeated.stderr == baseline, f"{name}: repeated diagnostic changed"

assert digest(source / "Static.roc") == helper_before
assert digest(binary) == binary_before
summary = {
    "binary": str(binary),
    "binary_sha256": binary_before,
    "helper_sha256": helper_before,
    "published_helper_keys": offers,
    "edited_consumer_hit_keys": sorted(hit_keys),
    "evidence": str(root),
    "commands": results,
}
(root / "summary.json").write_text(json.dumps(summary, indent=2))
print(json.dumps(summary, indent=2))
