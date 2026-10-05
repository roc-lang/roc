"""Run focused late object reuse checks using a fresh .tmp directory.

Use --expect-hits with a compiler built with -Dperf-late-callable-cache=true;
omit it for the disabled-feature oracle. Source fixtures are never edited.
"""

import argparse
import hashlib
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--roc", type=Path, required=True)
    parser.add_argument("--expect-hits", action="store_true")
    options = parser.parse_args()
    roc = options.roc.resolve()
    fixture = Path(__file__).resolve().parent
    root = fixture.parents[2]
    (root / ".tmp").mkdir(exist_ok=True)
    work = Path(tempfile.mkdtemp(prefix="late-callable-test-", dir=root / ".tmp"))
    app = work / "app"
    shutil.copytree(fixture / "pkg", app / "pkg")
    source = (fixture / "main.roc").read_text().replace(
        "../../fx-open/platform/main.roc",
        Path(os.path.relpath(root / "test/fx-open/platform/main.roc", app)).as_posix(),
    )
    (app / "main.roc").write_text(source)
    package_before = {path.name: hashlib.sha256(path.read_bytes()).hexdigest()
                      for path in (app / "pkg").glob("*.roc")}
    env = dict(os.environ, ROC_CACHE_DIR=str(work / "cache"), ROC_PACK_TRACE="1")
    lookup = re.compile(r"lookup late-callable key=([0-9a-f]+) identity=([0-9a-f]+) (hit|miss)")
    skips = re.compile(r"skip late-callable lir-body identity=([0-9a-f]+)")
    clone_ineligible = re.compile(
        r"ineligible late-callable identity=([0-9a-f]+) abi=finite captures=0 clone=true return-reuse=none"
    )

    def build(label, cached=True, reversed_order=False, callback_names=("one", "two"), captures=False,
              debug_message=None, duplicate_results=False, expected_runs=None):
        executable = work / label
        command = [str(roc), "build", "--jobs=2", "--specialize=yes", "--opt=dev",
                   "--timings", f"--output={executable}", str(app / "main.roc")]
        if not cached:
            command.insert(2, "--no-cache")
        result = subprocess.run(command, env=env, capture_output=True, text=True, timeout=180)
        (work / f"{label}.stdout").write_text(result.stdout)
        (work / f"{label}.stderr").write_text(result.stderr)
        assert result.returncode == 0, f"{label}: {result.stderr}\n{result.stdout}"
        assert "panic" not in result.stderr
        if debug_message is not None:
            assert result.stderr.count(debug_message) == 1, "compile-time debug replay changed"
        if expected_runs is None:
            expected_runs = []
            for args in [[], ["input"]]:
                names = [name + ("-zero" if not args else "") for name in callback_names]
                if reversed_order:
                    names.reverse()
                if captures:
                    names = ["0", "1"] if not args else ["2", "3"]
                if duplicate_results:
                    names = [name * 2 for name in names]
                expected_runs.append((args, "\n".join(names * 2) + "\n"))
        for args, expected in expected_runs:
            ran = subprocess.run([str(executable), *args], capture_output=True, text=True, timeout=20)
            assert ran.returncode == 0 and ran.stdout == expected and ran.stderr == "", (
                label, ran.returncode, ran.stdout, ran.stderr
            )
        return result.stderr

    build("uncached", cached=False)
    cold = build("cold")
    # Force real app checking/lowering; unchanged-app checked replay cannot pass.
    (app / "main.roc").write_text(source + "\n# app-only warm rebuild\n")
    warm = build("warm-edit")
    assert package_before == {path.name: hashlib.sha256(path.read_bytes()).hexdigest()
                              for path in (app / "pkg").glob("*.roc")}
    if options.expect_hits:
        cold_ids = {identity for _, identity, _ in lookup.findall(cold)}
        hits = {(key, identity) for key, identity, state in lookup.findall(warm) if state == "hit"}
        # Both callbacks can join one finite callable set. Its completed
        # procedure identifies the entire set, not one procedure per call.
        assert cold_ids, "higher-order helper did not reach the late boundary"
        assert hits, "edited app did not reuse the imported higher-order helper"
        assert {identity for _, identity in hits} <= cold_ids, "warm procedure identities changed"
        assert {identity for _, identity in hits} <= set(skips.findall(warm)), (
            "late lookups did not skip downstream LIR bodies"
        )
    else:
        assert not lookup.search(cold + warm), "disabled feature performed a late lookup"
        assert not skips.search(warm), "disabled feature skipped a late body"
    # Reverse caller order after another app-only edit.
    reversed_source = source.replace(
        "Helpers.apply(Callbacks.one, value)", "Helpers.apply(Callbacks.swap, value)"
    ).replace("Helpers.apply(Callbacks.two, value)", "Helpers.apply(Callbacks.one, value)").replace(
        "Helpers.apply(Callbacks.swap, value)", "Helpers.apply(Callbacks.two, value)"
    )
    (app / "main.roc").write_text(reversed_source + "\n# reversed caller order\n")
    reversed_trace = build("reversed", reversed_order=True)
    if options.expect_hits:
        # Multi-target member order belongs to the existing callable tag ABI.
        # A changed ABI must miss rather than borrow code with the old tags.
        reversed_ids = {identity for _, identity, _ in lookup.findall(reversed_trace)}
        assert reversed_ids
        (app / "main.roc").write_text(reversed_source + "\n# reversed warm rebuild\n")
        reversed_warm = build("reversed-warm", reversed_order=True)
        reversed_hits = {identity for _, identity, state in lookup.findall(reversed_warm) if state == "hit"}
        assert reversed_hits and reversed_hits <= reversed_ids
        assert reversed_hits <= set(skips.findall(reversed_warm))
    # The same arrow with a different *complete* target set must not reuse the
    # first callback's specialized code. Isolated single-target requests make
    # this dimension observable even when a multi-call app joins its targets.
    variant_identities = []
    for index, (callback, other) in enumerate([("one", "two"), ("two", "one"), ("one", "two")]):
        variant_source = source.replace(f"Callbacks.{other}", f"Callbacks.{callback}")
        (app / "main.roc").write_text(variant_source)
        prefix = f"single-{index}-{callback}"
        build(f"{prefix}-uncached", cached=False, callback_names=(callback, callback))
        variant_cold = build(f"{prefix}-cold", callback_names=(callback, callback))
        (app / "main.roc").write_text(variant_source + f"\n# warm {callback} only\n")
        variant_warm = build(f"{prefix}-warm", callback_names=(callback, callback))
        if options.expect_hits:
            identities = {identity for _, identity, _ in lookup.findall(variant_cold)}
            warm_hits = {identity for _, identity, state in lookup.findall(variant_warm) if state == "hit"}
            assert identities and warm_hits and warm_hits <= identities
            assert warm_hits <= set(skips.findall(variant_warm))
            variant_identities.append(identities)
    if options.expect_hits:
        assert variant_identities[0].isdisjoint(variant_identities[1]), (
            "same arrow/different callback bodies selected the same procedure identity"
        )
        assert variant_identities[0] == variant_identities[2], "reversed single-target build order changed identity"
    capture_source = (fixture / "captures.roc").read_text().replace(
        "../../fx-open/platform/main.roc",
        Path(os.path.relpath(root / "test/fx-open/platform/main.roc", app)).as_posix(),
    )
    (app / "main.roc").write_text(capture_source)
    build("captures-uncached", cached=False, captures=True)
    capture_cold = build("captures-cold", captures=True)
    (app / "main.roc").write_text(capture_source + "\n# same code, distinct runtime environments\n")
    capture_warm = build("captures-warm", captures=True)
    if options.expect_hits:
        capture_ids = {identity for _, identity, _ in lookup.findall(capture_cold)}
        capture_hits = {identity for _, identity, state in lookup.findall(capture_warm) if state == "hit"}
        assert capture_hits and capture_hits <= capture_ids
        assert capture_hits <= set(skips.findall(capture_warm))
    else:
        assert not lookup.search(capture_cold + capture_warm)
    container_source = (fixture / "containers.roc").read_text().replace(
        "../../fx-open/platform/main.roc",
        Path(os.path.relpath(root / "test/fx-open/platform/main.roc", app)).as_posix(),
    )
    (app / "main.roc").write_text(container_source)
    build("containers-uncached", cached=False, duplicate_results=True)
    container_cold = build("containers-cold", duplicate_results=True)
    (app / "main.roc").write_text(container_source + "\n# nominal and nested record warm rebuild\n")
    container_warm = build("containers-warm", duplicate_results=True)
    if options.expect_hits:
        # Existing SpecConstr creates constructor-pattern clones for these
        # fields. Their complete identity is renderable, but the unchanged
        # plain-procedure guard must still withhold lookup/publication.
        assert len(set(clone_ineligible.findall(container_cold))) >= 2
        assert len(set(clone_ineligible.findall(container_warm))) >= 2
        assert not lookup.search(container_cold + container_warm)
    else:
        assert not lookup.search(container_cold + container_warm)
    debug_message = '[dbg] "late callable observation"'
    observed_source = source + '''
observed = {
    dbg "late callable observation"
    Helpers.apply(Callbacks.one, 0)
}
expect observed == "one-zero"
'''
    (app / "main.roc").write_text(observed_source)
    build("observed-cold", debug_message=debug_message)
    (app / "main.roc").write_text(observed_source + "\n# observed warm rebuild\n")
    build("observed-warm", debug_message=debug_message)
    failures = [
        ("\nfailed = {\n    expect Helpers.apply(Callbacks.one, 0) == \"wrong\"\n    42\n}\n",
         "This expect failed during compile-time evaluation."),
        ('''
bad = {
    candidate : Try(U64, Str)
    candidate = Err("bad")
    match candidate {
        Ok(value) => Helpers.apply(Callbacks.one, value)
    }
}
''', "discovered empirically"),
    ]
    quiet_env = env.copy()
    quiet_env.pop("ROC_PACK_TRACE")
    for index, (failure_source, diagnostic) in enumerate(failures):
        (app / "main.roc").write_text(observed_source + failure_source)
        reports = []
        for cached in [False, True, True]:
            command = [str(roc), "check", "--jobs=2", "--no-color", str(app / "main.roc")]
            if not cached:
                command.insert(2, "--no-cache")
            checked = subprocess.run(command, env=quiet_env, capture_output=True, text=True, timeout=180)
            assert checked.returncode != 0 and diagnostic in checked.stderr, checked.stderr
            assert "panic" not in checked.stderr and "invariant violated" not in checked.stderr
            reports.append(checked.stderr)
        assert reports[0] == reports[1] == reports[2], "cold/warm error diagnostics changed"
        (work / f"observation-error-{index}.stderr").write_text(reports[0])
    # Exercise the existing converter regressions without building an unrelated
    # host platform: stage their apps against the already-used open platform.
    literal_dir = work / "literal"
    literal_dir.mkdir()
    platform_relative = Path(os.path.relpath(root / "test/fx-open/platform/main.roc", literal_dir)).as_posix()
    for path in (root / "test/cli/literal_root_rejected").glob("*.roc"):
        staged = path.read_text()
        if "app [main!]" in staged:
            staged = staged.replace("../../fx/platform/main.roc", platform_relative)
            staged = staged.replace("main! = || {", "main! = |args| {")
            staged = staged.replace("import pf.Stdin\n", "")
            staged = staged.replace("Str.count_utf8_bytes(Stdin.line!())", "args.len()")
            if "args.len()" not in staged:
                staged = staged.replace("main! = |args| {", "main! = |_args| {")
            body, end = staged.rsplit("}", 1)
            staged = body + "    Ok({})\n}" + end
        (literal_dir / path.name).write_text(staged)
    for name, cached in [
        ("Clean", True),
        ("Rejecting", True), ("Rejecting", True), ("Rejecting", False),
        ("RejectingAtRuntime", True), ("RejectingAtRuntime", True), ("RejectingAtRuntime", False),
        ("CallableLiteral", True), ("CallableLiteral", True), ("CallableLiteral", False),
    ]:
        command = [str(roc), "build", "--jobs=2", "--specialize=yes", "--opt=dev", "--no-color",
                   f"--output={work / 'literal-output'}", str(literal_dir / f"{name}.roc")]
        if not cached:
            command.insert(2, "--no-cache")
        literal = subprocess.run(command, env=quiet_env, capture_output=True, text=True, timeout=180)
        expected_reports = 0 if name == "Clean" else 1
        assert literal.stderr.count("invalid string") == expected_reports, literal.stderr
        assert "panic" not in literal.stderr and "invariant violated" not in literal.stderr
        assert (literal.returncode == 0) == (name == "Clean"), literal.stderr
        (work / f"literal-{name}-{'cached' if cached else 'uncached'}.stderr").write_text(literal.stderr)
    assert package_before == {path.name: hashlib.sha256(path.read_bytes()).hexdigest()
                              for path in (app / "pkg").glob("*.roc")}
    # A callback body edit changes source identity even though its arrow and
    # runtime layout stay identical. Old app/package packs must not serve it.
    callback_path = app / "pkg/Callbacks.roc"
    callback_path.write_text(callback_path.read_text().replace(
        'if value == 0 "one-zero" else "one"',
        'if value == 0 "changed-one-zero" else "changed-one"',
    ))
    (app / "main.roc").write_text(source)
    changed = build("callback-edited", callback_names=("changed-one", "two"))
    if options.expect_hits:
        changed_ids = {identity for _, identity, _ in lookup.findall(changed)}
        assert changed_ids and changed_ids.isdisjoint(cold_ids), "callback source edit retained old identity"
        assert all(state == "miss" for _, _, state in lookup.findall(changed)), (
            "edited callback borrowed an artifact from its previous source"
        )
    (app / "main.roc").write_text(source + "\n# warm edited callback\n")
    changed_warm = build("callback-edited-warm", callback_names=("changed-one", "two"))
    if options.expect_hits:
        changed_hits = {identity for _, identity, state in lookup.findall(changed_warm) if state == "hit"}
        assert changed_hits and changed_hits <= changed_ids
        assert changed_hits <= set(skips.findall(changed_warm))
    # A closed module export's List.map call is inlined, but worker preflight
    # reserves standalone map/helper procedures. Late publication must not
    # promote those unused reservations into native pack roots.
    shutil.copyfile(fixture / "Pack.roc", app / "Pack.roc")
    pack_source = (fixture / "pack_app.roc").read_text().replace(
        "../../fx-open/platform/main.roc",
        Path(os.path.relpath(root / "test/fx-open/platform/main.roc", app)).as_posix(),
    )
    (app / "main.roc").write_text(pack_source)
    pack_oracle = [([], "[2, 3, 4]\n")]
    build("pack-uncached", cached=False, expected_runs=pack_oracle)
    build("pack-cold", expected_runs=pack_oracle)
    (app / "main.roc").write_text(pack_source + "\n# representation-only map reservations\n")
    build("pack-warm", expected_runs=pack_oracle)
    print(f"PASS late callable cache {'on' if options.expect_hits else 'off'}; logs: {work}")


if __name__ == "__main__":
    main()
