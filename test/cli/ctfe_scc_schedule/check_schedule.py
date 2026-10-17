"""Preserve dependency, callable, and guarded-failure behavior across cache histories."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile


parser = argparse.ArgumentParser()
parser.add_argument("roc", type=Path)
parser.add_argument("--evidence", type=Path)
options = parser.parse_args()
binary = options.roc.resolve()
fixture = Path(__file__).resolve().parent
root = options.evidence.resolve() if options.evidence else Path(tempfile.mkdtemp(prefix="ctfe-scc-"))
root.mkdir(parents=True, exist_ok=True)
source = root / "source"
source.mkdir()
for path in fixture.glob("*.roc"):
    shutil.copy2(path, source / path.name)
env = {key: value for key, value in os.environ.items() if not key.startswith("ROC_")}
env["ROC_CACHE_DIR"] = str(root / "cache")
binary_hash = hashlib.sha256(binary.read_bytes()).hexdigest()
commands = []


def run(label, args, expected=0):
    command = [str(binary), *args, "--no-color"]
    result = subprocess.run(
        command, cwd=source, env=env, capture_output=True, text=True, timeout=120
    )
    (root / f"{label}.stdout").write_text(result.stdout)
    (root / f"{label}.stderr").write_text(result.stderr)
    commands.append({"label": label, "command": command, "returncode": result.returncode})
    (root / "commands.json").write_text(json.dumps(commands, indent=2))
    assert result.returncode == expected, f"{label}: {result.stdout}\n{result.stderr}"
    assert hashlib.sha256(binary.read_bytes()).hexdigest() == binary_hash
    return result


for name in ("Forward", "Functions", "Guarded"):
    run(f"{name}-uncached", ["check", "--no-cache", f"{name}.roc"])
    run(f"{name}-seed", ["check", f"{name}.roc"])
    run(f"{name}-repeat", ["check", f"{name}.roc"])
    # This forces consumer finalization again rather than merely restoring the
    # same completed frontend result before native test execution.
    with (source / f"{name}.roc").open("a") as stream:
        stream.write("\n# Recheck the same dependency contract in an edited consumer.\n")
    result = run(f"{name}-edited-native", ["test", "--opt=dev", f"{name}.roc"])
    assert "All (1) tests passed" in result.stdout, result.stdout

for name, marker in (
    ("FailedGuard", "I64 division by zero"),
    ("Cycle", "circular value definition"),
):
    baseline = run(f"{name}-uncached", ["check", "--no-cache", f"{name}.roc"], expected=1)
    assert marker in baseline.stderr, baseline.stderr
    for suffix in ("cached", "repeat"):
        result = run(f"{name}-{suffix}", ["check", f"{name}.roc"], expected=1)
        assert (result.stdout, result.stderr) == (baseline.stdout, baseline.stderr), (
            f"{name}: cached diagnostics changed"
        )

summary = {
    "binary": str(binary),
    "binary_sha256": binary_hash,
    "commands": commands,
    "contracts": [
        "forward dependency through invoked callback",
        "mutual function recursion",
        "guarded failure not reached",
        "guarded failure reached",
        "eager value cycle",
    ],
}
(root / "summary.json").write_text(json.dumps(summary, indent=2))
print(json.dumps(summary, indent=2))
