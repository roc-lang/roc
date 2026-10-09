"""Exercise cached callable values and literal rejection without broad CLI suites."""

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
project = Path(__file__).resolve().parents[3]
root = (
    options.evidence.resolve()
    if options.evidence
    else Path(tempfile.mkdtemp(prefix="ctfe-cache-mixing-"))
)
root.mkdir(parents=True, exist_ok=True)
cache = root / "cache"
source = root / "source"
source.mkdir()
callable_fixture = project / "test/cli/issue_11344_shared_ctfe"
shutil.copy2(callable_fixture / "CallableHelper.roc", source / "CallableHelper.roc")
# The original headerless fixture is a test entrypoint. Give the staged copy an
# explicit nominal module so `check` treats it as a library, not a default app.
callable_path = source / "Callable.roc"
callable_text = (callable_fixture / "callable.roc").read_text()
assert callable_text.count("import CallableHelper\n") == 1
callable_path.write_text(
    callable_text.replace(
        "import CallableHelper\n", "import CallableHelper\n\nCallable := [].{}\n"
    )
)
base_env = {key: value for key, value in os.environ.items() if not key.startswith("ROC_")}
base_env["ROC_CACHE_DIR"] = str(cache)
commands = []


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


binary_before = digest(binary)
helper_before = digest(source / "CallableHelper.roc")


def run(label, args, *, trace=False, expected=0):
    env = dict(base_env)
    if trace:
        env["ROC_PACK_TRACE"] = "1"
    command = [str(binary), *args, "--no-color"]
    result = subprocess.run(
        command,
        cwd=project,
        env=env,
        stdin=subprocess.DEVNULL,
        capture_output=True,
        text=True,
        timeout=120,
    )
    (root / f"{label}.stdout").write_text(result.stdout)
    (root / f"{label}.stderr").write_text(result.stderr)
    commands.append({"label": label, "command": command, "returncode": result.returncode})
    (root / "commands.json").write_text(json.dumps(commands, indent=2))
    assert result.returncode == expected, f"{label}: {result.stdout}\n{result.stderr}"
    assert digest(binary) == binary_before
    return result


# Keep the same checked/evaluated callable producer, but force the consumer to
# finalize again before native test execution reads the frozen captured values.
run("callable-seed-check", ["check", str(callable_path)], trace=True)
with callable_path.open("a") as stream:
    stream.write("\n# Recheck the consumer without changing its imported callable.\n")
for label, trace in (("callable-edited-dev", True), ("callable-repeat-dev", False)):
    result = run(label, ["test", "--opt=dev", str(callable_path)], trace=trace)
    assert re.search(r"All \(6\) tests passed", result.stdout), result.stdout
assert digest(source / "CallableHelper.roc") == helper_before


# These are the commands from the focused rejected-literal CLI scenario, run
# directly so the CLI runner does not build unrelated platform hosts.
literal_fixture = project / "test/cli/literal_root_rejected"
rejecting = literal_fixture / "Rejecting.roc"
baseline = run("literal-check-uncached", ["check", "--no-cache", str(rejecting)], expected=1)
assert baseline.stderr.count("invalid string") == 1
for label in ("literal-check-cached", "literal-check-repeat"):
    result = run(label, ["check", str(rejecting)], expected=1)
    assert (result.stdout, result.stderr) == (baseline.stdout, baseline.stderr)

output = "--output=" + str(root / "literal-output")
run(
    "literal-clean-dev-seed",
    ["build", "--opt=dev", str(literal_fixture / "Clean.roc"), output],
)
for name, reported_at in (("Rejecting", "Rejecting.roc"), ("RejectingAtRuntime", "Query.roc")):
    results = []
    for suffix, no_cache in (("cached", False), ("repeat", False), ("uncached", True)):
        args = ["build", "--opt=dev", str(literal_fixture / f"{name}.roc"), output]
        if no_cache:
            args.append("--no-cache")
        result = run(f"{name}-dev-{suffix}", args, expected=1)
        assert result.stderr.count("invalid string") == 1, result.stderr
        assert reported_at in result.stderr, result.stderr
        assert "invariant violated" not in result.stderr
        assert "panic" not in result.stderr
        # Build status includes wall time; diagnostic text and locations must
        # match exactly, but duration is not part of the diagnostic contract.
        status = re.sub(
            r"(found in )\S+( while successfully building:)",
            r"\1<duration>\2",
            result.stdout,
        )
        results.append((status, result.stderr))
    assert results[0] == results[1] == results[2], f"{name}: cached diagnostic changed"

summary = {
    "binary": str(binary),
    "binary_sha256": binary_before,
    "helper_sha256": helper_before,
    "callable_tests_per_execution": 6,
    "evidence": str(root),
    "commands": commands,
}
(root / "summary.json").write_text(json.dumps(summary, indent=2))
print(json.dumps(summary, indent=2))
