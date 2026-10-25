#!/usr/bin/env python3
"""Focused issue #12593 CLI checks\n    does not run MiniCI or compiler eval suites."""

import argparse
from concurrent.futures import ThreadPoolExecutor
import os
from pathlib import Path
import re
import subprocess
import tempfile


def is_outcome(path):
    if not path.is_file():
        return False
    with path.open('rb') as source:
        return source.read(len(b'ROC_TEST_OUTCOMES_V1')) == b'ROC_TEST_OUTCOMES_V1'


def transcript(data):
    # Only the final run epilogue is outside the transcript contract.
    return re.split(rb"(?m)^(?:All \(\d+\) tests passed|Ran \d+ tests) in ", data)[0]


class Fixture:
    def __init__(self, root, roc):
        self.root = root
        self.roc = roc
        self.cache = root / "cache"
        self.env = dict(os.environ, ROC_CACHE_DIR=str(self.cache))
        self.path = root / "Main.roc"

    def write(self, text, name="Main.roc"):
        (self.root / name).write_text(text)

    def run(self, *, cache=True, timings=False, mode="dev"):
        args = [str(self.roc), "test", str(self.path), f"--opt={mode}",
                "--specialize=yes", "--no-color", "--verbose"]
        if not cache:
            args.append("--no-cache")
        if timings:
            args.append("--timings")
        result = subprocess.run(args, env=self.env, capture_output=True, timeout=120)
        assert result.returncode in (0, 1, 2), result.stderr.decode(errors="replace")
        assert b"panic" not in result.stderr, result.stderr.decode(errors="replace")
        return result

    def check(self, hits=None, *, mode="dev"):
        cached = self.run(mode=mode)
        fresh = self.run(cache=False, mode=mode)
        assert cached.returncode == fresh.returncode, (cached.stderr, fresh.stderr)
        for stream in ("stdout", "stderr"):
            a, b = transcript(getattr(cached, stream)), transcript(getattr(fresh, stream))
            assert a == b, (stream, a.decode(errors="replace"), b.decode(errors="replace"))
        if hits is not None:
            # The first run above publishes all misses, so all roots must now hit.
            warm = self.run(timings=True, mode=mode)
            self.hits(warm, hits)
            text = re.sub(r"\s+", " ", warm.stderr.decode(errors="replace"))
            for label in ('Roots submitted for specialization',):
                assert re.search(label + r"\s+0\b", text), text
            if mode == 'dev':
                for label in ('Root wrappers generated', 'Roots executed'):
                    assert re.search(label + r"\s+0\b", text), text
        return cached

    @staticmethod
    def hits(result, expected):
        text = result.stderr.decode(errors="replace")
        text = re.sub(r"\s+", " ", text)
        match = re.search(r"Roots cached\s+(\d+)", text)
        assert match, text
        assert int(match[1]) == expected, (expected, match[1], text)


def exercise(f):
    base = ('helper = |x| x + 1\n'
            'expect helper(2) == 3\n'
            'expect 4 == 4\n'
            'expect { dbg "stable"\n    5 == 6 }\n')
    f.write(base)
    f.hits(f.run(timings=True), 0)
    f.check(3)
    f.write(base.replace('4 == 4', '4 == 3'))
    f.hits(f.run(timings=True), 2)
    f.check(3)
    f.write(base)
    f.hits(f.run(timings=True), 3)
    f.write(base.replace('x + 1', 'x + 2'))
    f.hits(f.run(timings=True), 2)
    f.check(3)

    binding = 'value = 1\nexpect value == 2\nexpect 3 == 3\n'
    f.write(binding)
    f.check(2)
    f.write(binding.replace('value = 1', 'value=  1'))
    f.hits(f.run(timings=True), 1)
    f.check(2)

    # Duplicate bodies remain separate reporting slots\n    moving, adding and
    # deleting roots must preserve current source provenance and multiplicity.
    duplicate = 'expect { dbg "duplicate"\n    1 == 1 }\n'
    for source in (duplicate * 2, '\n' + duplicate * 2,
                   duplicate * 3, duplicate, duplicate + base):
        f.write(source)
        f.check(source.count('expect '))

    # Both roots reach the same inline source site. Editing one root changes
    # its contribution while preserving coalesced counts and reporting order.
    inline = ('observe = |x| { expect x > 0\n    x }\n'
              'expect observe(1) == 1\n'
              'expect observe(0) == 0\n')
    f.write(inline)
    f.check(2)
    f.write(inline.replace('observe(0) == 0', 'observe(2) == 2'))
    f.hits(f.run(timings=True), 1)
    f.check(2)
    f.write(inline)
    f.hits(f.run(timings=True), 2)
    f.check(2)

    # Static dependency closure includes a branch never executed, recursive
    # helpers, a compile-time constant and a closure capture.
    deps = ('unused = |x| x + 1\n'
            'recur = |n| if n == 0 { 0 } else { recur(n - 1) }\n'
            'constant = 17\n'
            'capture = |x| { y = constant\n    |z| x + y + z }\n'
            'expect (if True { 1 } else { unused(0) }) == 1\n'
            'expect recur(3) == 0\n'
            'expect capture(1)(2) == 20\n'
            'expect 7 == 7\n')
    f.write(deps)
    f.check(4)
    for old, new, hits in [('x + 1', 'x + 2', 3),
                           ('{ 0 }', '{ 1 }', 3),
                           ('constant = 17', 'constant = 18', 3)]:
        f.write(deps.replace(old, new))
        f.hits(f.run(timings=True), hits)
        f.check(4)
        f.write(deps)
        f.check(4)

    # Imported implementation summaries, including transitive dependencies,
    # distinguish a relevant helper change from an unrelated imported test.
    leaf = 'module [helper]\nhelper = |x| x + 1\nexpect 8 == 8\n'
    middle = 'module [call]\nimport Leaf\ncall = |x| Leaf.helper(x)\n'
    main = 'import Middle\nexpect Middle.call(2) == 3\nexpect 9 == 9\n'
    f.write(leaf, 'Leaf.roc')
    f.write(middle, 'Middle.roc')
    f.write(main)
    f.check(3)
    f.write(leaf.replace('8 == 8', '8 == 7'), 'Leaf.roc')
    f.hits(f.run(timings=True), 2)
    f.check(3)
    f.write(leaf.replace('x + 1', 'x + 2'), 'Leaf.roc')
    # The independent Main root and reverted Leaf root remain hits.
    f.hits(f.run(timings=True), 2)
    f.check(3)

    # Custom numeral conversion and inspection are semantic inputs, including
    # the generated structural inspection used by the second root.
    custom = """MyNum := { value: I64 }.{
    from_numeral : Numeral -> Try(MyNum, [InvalidNumeral(Str)])
    from_numeral = |_| {
        dbg "conversion"
        Ok({ value: 1 })
    }
    plus : MyNum, MyNum -> MyNum
    plus = |a, b| { value: a.value + b.value }
    to_i64 = |a| a.value
    to_inspect : MyNum -> Str
    to_inspect = |_| "before"
}
add_one = |x| x.plus(1)
expect {
    value : MyNum
    value = { value: 0 }
    result = add_one(value)
    dbg result
    result.to_i64() == 1
}
expect {
    dbg { names: ["one", "two"], tag: Some(3) }
    True
}
"""
    for source in (custom, custom.replace('value: 1', 'value: 2'),
                   custom.replace('"before"', '"after!"'), custom):
        f.write(source)
        f.check(2)
    # Other consumers preserve the same partial-admission ownership contract.
    for mode in ('interpreter', 'speed'):
        f.check(2, mode=mode)
    f.write(main)
    f.check(3)

    # Atomic publication and checksums: every truncated or mutated outcome is
    # a miss, even if its corrupted status byte would otherwise be valid.
    outcomes = [p for p in f.cache.rglob('*') if is_outcome(p)]
    assert outcomes, 'no per-root outcome records published'
    for p in outcomes:
        data = bytearray(p.read_bytes())
        if len(data) % 2:
            p.write_bytes(data[:20])
        else:
            data[-33] ^= 1
            p.write_bytes(data)
    f.hits(f.run(timings=True), 0)
    f.check(3)
    for p in f.cache.rglob('*'):
        if is_outcome(p):
            p.unlink()
    with ThreadPoolExecutor(max_workers=3) as pool:
        runs = list(pool.map(lambda _: f.run(), range(3)))
    for result in runs:
        assert result.returncode == runs[0].returncode
        assert transcript(result.stdout) == transcript(runs[0].stdout)
        assert transcript(result.stderr) == transcript(runs[0].stderr)
    f.check(3)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--roc', type=Path, required=True)
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix='roc-test-outcomes-') as temp:
        exercise(Fixture(Path(temp), args.roc.resolve()))
    print('issue 12593: focused test-outcome cache checks passed')


if __name__ == '__main__':
    main()
