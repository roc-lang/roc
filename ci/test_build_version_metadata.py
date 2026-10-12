#!/usr/bin/env python3
"""Exercise the real cached build's optional Git display metadata inputs."""
import argparse
import json
import re
import shutil
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("zig")
    parser.add_argument("--work-dir", type=Path)
    args, options = parser.parse_known_args()
    if options[:1] == ["--"]:
        options = options[1:]
    work = (args.work_dir or Path(tempfile.mkdtemp(prefix="roc-version-metadata-"))).resolve()
    work.mkdir(parents=True, exist_ok=True)
    source = work / "source"
    source.mkdir()
    for directory in ("src", "vendor", "test", "ci"):
        shutil.copytree(ROOT / directory, source / directory,
                        ignore=shutil.ignore_patterns("__pycache__", "*.pyc", "*.pyo"))
    for name in ("build.zig", "build.zig.zon", "design.md", "legal_details", "README.md"):
        shutil.copyfile(ROOT / name, source / name)
    build = source / "build.zig"
    anchor = "const compiler_version_module = compiler_version_options.createModule();"
    contents = build.read_text()
    assert contents.count(anchor) == 1
    # Expose the production Options output through a standard install step.
    # The version reader itself is unchanged, and every case reuses this argv.
    build.write_text(contents.replace(anchor, anchor + '''
    b.step("version-metadata-probe", "Expose display metadata for a test")
        .dependOn(&b.addInstallFile(compiler_version_options.getOutput(), "version-metadata.zig").step);
'''))
    if not any(value.startswith("-Droc-deps-path=") for value in options):
        bundle = work / "empty-dependencies"
        (bundle / "include").mkdir(parents=True)
        (bundle / "lib").mkdir()
        options.append(f"-Droc-deps-path={bundle}")
    zig_lib = [value for value in options if value.startswith("--zig-lib=")]
    remaining = [value for value in options if value not in zig_lib]
    command = [args.zig, "build", *zig_lib, "version-metadata-probe", *remaining,
               "--cache-dir", str(work / "cache"), "--prefix", str(work / "out"),
               "--summary", "all", "--cache-poison=disallowed"]
    records = []

    def check(label, expected):
        result = subprocess.run(command, cwd=source, capture_output=True, text=True)
        output = result.stdout + result.stderr
        (work / f"{label}.log").write_text(output)
        assert result.returncode == 0, output
        actual = re.search(r'compiler_version_git: \[\]const u8 = "([^"]+)"',
                           (work / "out/version-metadata.zig").read_text()).group(1)
        assert actual == expected, (label, expected, actual)
        records.append({"label": label, "argv": command, "versionGit": actual})
        print(f"{label}: {actual}")

    check("gitless", "no-git")
    check("gitless-unchanged", "no-git")
    git = source / ".git"
    git.mkdir()
    head = git / "HEAD"
    head.write_text("a" * 40 + "\n")
    check("directory-without-commondir", "aaaaaaaa")
    head.write_text("b" * 40 + "\n")
    check("head-edited", "bbbbbbbb")
    head.unlink()
    check("head-deleted", "no-git")
    head.write_text("ref: refs/heads/topic/nested\n")
    check("missing-reference-parents", "no-git")
    ref = git / "refs/heads/topic/nested"
    ref.parent.mkdir(parents=True)
    ref.write_text("c" * 40 + "\n")
    check("loose-reference-created", "cccccccc")
    ref.write_text("d" * 40 + "\n")
    check("loose-reference-edited", "dddddddd")
    packed = git / "packed-refs"
    packed.write_text("e" * 40 + " refs/heads/topic/nested\n")
    ref.unlink()
    check("reference-packed", "eeeeeeee")
    packed.write_text("f" * 40 + " refs/heads/topic/nested\n")
    check("packed-reference-edited", "ffffffff")
    packed.unlink()
    check("packed-reference-deleted", "no-git")
    shutil.rmtree(git)
    check("git-directory-deleted", "no-git")
    external = work / "worktree-git"
    external.mkdir()
    (external / "HEAD").write_text("1" * 40 + "\n")
    git.write_text("gitdir: ../worktree-git\n")
    check("relative-worktree-pointer", "11111111")
    (external / "HEAD").write_text("ref: refs/heads/shared\n")
    check("worktree-reference-missing", "no-git")
    common = work / "common-git"
    common.mkdir()
    (external / "commondir").write_text("../common-git\n")
    shared = common / "refs/heads/shared"
    shared.parent.mkdir(parents=True)
    shared.write_text("2" * 40 + "\n")
    check("worktree-common-reference-created", "22222222")
    shared.write_text("3" * 40 + "\n")
    check("worktree-common-reference-edited", "33333333")
    (common / "packed-refs").write_text("4" * 40 + " refs/heads/shared\n")
    shared.unlink()
    check("worktree-common-reference-packed", "44444444")
    (external / "commondir").unlink()
    check("worktree-commondir-deleted", "no-git")
    git.unlink()
    check("worktree-pointer-deleted", "no-git")
    def malformed(label, diagnostic):
        result = subprocess.run(command, cwd=source, capture_output=True, text=True)
        output = result.stdout + result.stderr
        (work / f"{label}.log").write_text(output)
        assert result.returncode != 0 and diagnostic in output, output
        records.append({"label": label, "argv": command, "exitCode": result.returncode,
                        "expectedFailure": True, "diagnostic": diagnostic})

    git.mkdir()
    (git / "HEAD").mkdir()
    malformed("non-file-head", "expected regular Git metadata file")
    (git / "HEAD").rmdir()
    (git / "HEAD").write_text("ref: refs/heads/topic\n")
    (git / "refs").write_text("not a directory\n")
    malformed("non-directory-reference-parent", "cannot inspect Git metadata")
    (work / "results.json").write_text(json.dumps(records, indent=2) + "\n")


if __name__ == "__main__":
    main()
