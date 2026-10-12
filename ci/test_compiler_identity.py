#!/usr/bin/env python3
"""Exercise compiler compatibility invalidation through its real host tool."""

from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

TOOL = str(Path(sys.argv.pop(1)).resolve())


class CompilerIdentity(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.source = self.root / "declared-inputs"
        (self.source / "src").mkdir(parents=True)
        (self.source / "vendor").mkdir()
        (self.source / "src/checker.zig").write_text("const semantic_rule = 1;\n")
        (self.source / "vendor/runtime.c").write_text("void runtime(void) {}\n")
        self.zig = self.root / "zig"
        self.zig.write_bytes(b"exact toolchain bytes")
        self.deps = self.root / "deps.json"
        self.deps.write_text('{"llvm":"22.1.8"}')
        self.output = self.root / "identity.zig"

    def identity(self, options=("optimize=ReleaseFast", "target=x86_64-linux-musl"), source=None, files=None):
        command = [TOOL, "--source-root", str(source or self.source), "--zig-exe", str(self.zig),
                   "--output", str(self.output)]
        for option in options:
            command.extend(("--option", option))
        for name, path in files or [("dependencies", self.deps)]:
            command.extend(("--file", name, str(path)))
        subprocess.run(command, check=True, capture_output=True)
        return self.output.read_bytes()

    def test_same_declared_contents_reuse_identity(self):
        initial = self.identity()
        self.assertEqual(initial, self.identity())
        # Editing unrelated version-control metadata cannot invalidate it.
        (self.root / "HEAD").write_text("same commit as before")
        self.assertEqual(initial, self.identity())

    def test_dirty_semantic_edit_invalidates_and_revert_restores(self):
        initial = self.identity()
        file = self.source / "src/checker.zig"
        original = file.read_bytes()
        file.write_text("const semantic_rule = 2;\n")
        self.assertNotEqual(initial, self.identity())
        file.write_bytes(original)
        self.assertEqual(initial, self.identity())

    def test_import_membership_add_delete_and_rename(self):
        initial = self.identity()
        file = self.source / "src/import.roc"
        file.write_text("module []")
        added = self.identity()
        self.assertNotEqual(initial, added)
        file.rename(file.with_name("renamed.roc"))
        self.assertNotEqual(added, self.identity())
        file.with_name("renamed.roc").unlink()
        self.assertEqual(initial, self.identity())

    def test_dependency_and_toolchain_edits_invalidate(self):
        initial = self.identity()
        self.deps.write_text('{"llvm":"changed build with same version"}')
        self.assertNotEqual(initial, self.identity())
        self.deps.write_text('{"llvm":"22.1.8"}')
        self.zig.write_bytes(b"other compiler with same version string")
        self.assertNotEqual(initial, self.identity())

    def test_semantic_options_are_order_independent_but_changes_invalidate(self):
        initial = self.identity()
        self.assertEqual(initial, self.identity(("target=x86_64-linux-musl", "optimize=ReleaseFast")))
        self.assertNotEqual(initial, self.identity(("target=x86_64-linux-musl", "optimize=ReleaseSafe")))

    def test_source_location_does_not_change_identity(self):
        initial = self.identity()
        import shutil
        copied = self.root / "another-worktree"
        shutil.copytree(self.source, copied)
        self.assertEqual(initial, self.identity(source=copied))

    def test_dependency_order_is_stable_and_names_are_unique(self):
        second = self.root / "other.json"
        second.write_text("other dependency")
        files = [("dependencies", self.deps), ("second", second)]
        self.assertEqual(self.identity(files=files), self.identity(files=list(reversed(files))))
        with self.assertRaises(subprocess.CalledProcessError):
            self.identity(files=[("duplicate", self.deps), ("duplicate", second)])


if __name__ == "__main__":
    unittest.main()
