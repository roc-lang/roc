#!/usr/bin/env python3
"""Focused test-outcome, completed-command and reporting-relocation checks under both strategies."""

import argparse
from concurrent.futures import ThreadPoolExecutor
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import time


COMMAND_MAGIC = b'ROC_COMPLETED_DEV_TEST_V4'


def is_outcome(path):
    if not path.is_file():
        return False
    with path.open('rb') as source:
        return source.read(len(b'ROC_TEST_OUTCOMES_V3')) == b'ROC_TEST_OUTCOMES_V3'


def transcript(data):
    # Only the final run epilogue is outside the transcript contract.
    return re.split(rb"(?m)^(?:All \(\d+\) tests passed|Ran \d+ tests) in ", data)[0]


class Fixture:
    def __init__(self, root, roc, specialize):
        self.root = root
        self.roc = roc
        self.specialize = specialize
        self.cache = root / "cache"
        self.env = dict(os.environ, ROC_CACHE_DIR=str(self.cache))
        if specialize == "yes":
            self.env["ROC_TRACE_BOXY_WORK"] = "1"
        self.path = root / "Main.roc"

    def write(self, text, name="Main.roc"):
        (self.root / name).write_text(text)

    def run(self, *, cache=True, timings=False, mode="dev", work=False):
        args = [str(self.roc), "test", str(self.path), f"--opt={mode}",
                f"--specialize={self.specialize}", "--no-color", "--verbose"]
        if not cache:
            args.append("--no-cache")
        if timings:
            args.append("--timings")
        env = dict(self.env, ROC_TEST_COMMAND_WORK="1") if work else self.env
        result = subprocess.run(args, env=env, capture_output=True, timeout=120)
        assert result.returncode in (0, 1, 2), result.stderr.decode(errors="replace")
        assert b"panic" not in result.stderr, result.stderr.decode(errors="replace")
        if self.specialize == "yes":
            assert b"boxy-work:" not in result.stderr, result.stderr.decode(errors="replace")
        # Checking traces its literal arguments only when it checks afresh.
        result.stderr = re.sub(rb"(?m)^literal-args: .*\n", b"", result.stderr)
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
            if 'Completed test command' in text:
                return cached
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


def relocation_checks(f):
    def edit(source, hits, executed=0, name="Main.roc"):
        f.write(source, name)
        run = f.run(timings=True, work=True)
        f.hits(run, hits)
        counter = re.search(rb"test-work test_root_execution=(\d+)", run.stderr)
        assert counter and int(counter[1]) == executed, run.stderr.decode(errors="replace")
        f.check()

    passing = 'expect { dbg "Main.roc:123: user bytes"\n    1 == 1 }\n'
    failing = 'expect { dbg "failed"\n    2 == 3 }\n'
    base = passing + failing
    f.write(base)
    f.check(2)
    edit('# inserted comment\n\n' + base, 2)
    edit(base, 2)
    edit(failing + passing, 2)
    edit(failing + passing.replace('1 == 1', '100 == 100'), 1, 1)
    edit(passing.replace('1 == 1', '1000 == 1000') + failing, 1, 1)
    edit('expect 42 == 42\n' + passing + failing, 2, 1)
    edit(passing * 3 + failing * 2, 5)
    edit('\n# duplicate occurrences move together\n' + failing * 2 + passing * 3, 5)

    duplicate_inline = 'expect { expect False\n    dbg "nested duplicate"\n    True }\n'
    f.write(duplicate_inline * 2)
    f.check(2)
    edit('# before duplicate inline sites\n' + duplicate_inline + '\n' + duplicate_inline, 2)

    helper = ('observe = |x| { dbg "Helper.roc:5: user text"\n'
              '    expect x > 0\n    x }\n')
    roots = 'expect observe(1) == 1\nexpect observe(0) == 0\n'
    f.write(helper + roots)
    f.check(2)
    edit('# move helper and roots\n\n' + helper + roots, 2)
    edit(roots + '\n# helper moved independently\n' + helper, 2)
    edit(roots + helper.replace('x > 0', 'x > 1'), 0, 2)

    imported = 'module [observe]\n' + helper
    f.write(imported, 'Observer.roc')
    f.write('import Observer\nexpect Observer.observe(0) == 0\n')
    f.check(1)
    edit('module [observe]\n# helper moved\n\n' + helper, 1, name='Observer.roc')
    edit(imported.replace('x > 0', 'x > 1'), 0, 1, name='Observer.roc')

    typed = ('module [make]\nThing := { value : U64 }\n'
             'make = || Thing.{ value: 1 }\n')
    f.write(typed, 'Typed.roc')
    f.write('import Typed\nhelper = || Typed.make()\nexpect helper().value == 1\n')
    f.check(1)
    edit('# moved imported nominal and helper\n' + typed, 1, name='Typed.roc')
    edit(typed.replace('module [make]\n', 'module [make]\nexpect 37 == 37\n'), 1, 1, name='Typed.roc')
    edit(typed.replace('value: 1', 'value: 2'), 0, 1, name='Typed.roc')

    alias = ('module [make]\nThing : { value : U64 }\n'
             'make : {} -> Thing\nmake = |_| { value: 1 }\n')
    f.write(alias, 'TypedAlias.roc')
    f.write('import TypedAlias\nhelper = || TypedAlias.make({})\nexpect helper().value == 1\n')
    f.check(1)
    edit('# moved imported alias\n' + alias, 1, name='TypedAlias.roc')
    edit(alias.replace('U64', 'I64'), 0, 1, name='TypedAlias.roc')

    default_helper = ('WithDefault := { value : U64 ?? compute() }.{}\n'
                      'compute = || { expect True\n    1 }\n')
    f.write(default_helper, 'WithDefault.roc')
    f.write('import WithDefault\nexpect WithDefault.{}.value == 1\n')
    f.check(1)
    edit('# moved a default and its helper\n' + default_helper, 1, name='WithDefault.roc')
    edit(default_helper.replace('    1 }', '    2 }'), 0, 1, name='WithDefault.roc')

    defaults = ('Config := { value : U64 ?? 1 }\n'
                'make = || Config.{}\n'
                'expect make().value == 1\n')
    f.write(defaults)
    f.check(1)
    edit('# before a defaulted type\n' + defaults, 1)
    edit(defaults.replace('?? 1', '?? 2'), 0, 1)

    scoped = ('left = || { value = 1\n    expect value == 1\n    True }\n'
              'right = || { value = 2\n    expect value == 1\n    True }\n'
              'expect left()\nexpect right()\n')
    f.write(scoped)
    f.check(2)
    edit('# before distinct binding scopes\n' + scoped, 2)
    edit(scoped.replace('value = 2', 'value = 3'), 1, 1)

    asset = 'import "asset.txt" as data : Str\nexpect data == "before"\n'
    f.write('before', 'asset.txt')
    f.write(asset)
    f.check(1)
    edit('# before a file import\n' + asset, 1)
    edit('after', 0, 1, name='asset.txt')

    # Failure details include current binding excerpts; user messages are exact.
    crashing = 'expect { dbg "unchanged crash output"\n    crash "Main.roc:99" }\n'
    f.write(crashing)
    f.check(1)
    edit('# before a crash\n' + crashing, 1)
    question = 'expect { value : Try(U64, [Oops])\n    value = Err(Oops)\n    value? == 1 }\n'
    f.write(question)
    f.check(1)
    edit('\n# before an extended failure span\n' + question, 1)


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

WORK_COUNTERS = ('specialization', 'ctfe_execution', 'second_specialization', 'runtime_lir',
                 'arc', 'static_data', 'native_test_procedures', 'test_root_execution')


def counter(run, name):
    match = re.search(rb'(?m)^test-work ' + name.encode() + rb'=(\d+)$', run.stderr)
    assert match, run.stderr.decode(errors='replace')
    return int(match[1])


def complete_hit(f, **kwargs):
    """The record replays with every post-check counter at zero."""
    run = f.run(timings=True, work=True, **kwargs)
    text = re.sub(r'\s+', ' ', run.stderr.decode(errors='replace'))
    assert re.search(r'Completed test command.*?Hits\s+1\b', text), text
    for name in WORK_COUNTERS:
        assert counter(run, name) == 0, (name, text)
    return run


def is_command_record(path):
    if not path.is_file():
        return False
    with path.open('rb') as source:
        return source.read(len(COMMAND_MAGIC)) == COMMAND_MAGIC


def same_report(a, b):
    assert a.returncode == b.returncode, (a.stderr, b.stderr)
    assert transcript(a.stdout) == transcript(b.stdout), (a.stdout, b.stdout)
    assert transcript(a.stderr) == transcript(b.stderr), (a.stderr, b.stderr)


# Each case is (name, files, [(file, old, new)]): an edit outside the test body
# that changes what the test observes. A cached outcome that survived the edit
# would disagree with a fresh run, and a fresh run proves the edit is observable.
TH = 'module [T, make]\nT := [A].{\n  render = |_| "one"\n}\nmake = || T.A\n'
EQ = 'module [T, make]\nT := [A, B].{\n  is_eq = |_, _| True\n}\nmake = |x| if x T.A else T.B\n'
NUM = ('module [N, make]\nN := { v: I64 }.{\n'
       '  from_numeral : Numeral -> Try(N, [InvalidNumeral(Str)])\n'
       '  from_numeral = |_| Ok({ v: 1 })\n  is_eq = |a, b| a.v == b.v\n  get = |a| a.v\n}\n'
       'make = |v| N.{ v }\n')
QUOTE = ('module [Q, get]\nQ := { v: I64 }.{\n'
         '  from_quote : Str -> Try(Q, [BadQuotedBytes(Str)])\n'
         '  from_quote = |_| Ok({ v: 1 })\n  get = |a| a.v\n}\nget = |q| q.get()\n')
IDENTITY_CASES = [
    ('test body', {'Main.roc': 'expect 1 == 1\n'}, [('Main.roc', '1 == 1', '1 == 2')]),
    ('imported function', {'Lib.roc': 'module [f]\nf = |x| x + 1\n', 'Main.roc': 'import Lib\nexpect Lib.f(1) == 2\n'},
     [('Lib.roc', 'x + 1', 'x + 2')]),
    ('transitive function', {'Leaf.roc': 'module [f]\nf = |x| x + 1\n',
                             'Lib.roc': 'module [g]\nimport Leaf\ng = |x| Leaf.f(x)\n',
                             'Main.roc': 'import Lib\nexpect Lib.g(1) == 2\n'},
     [('Leaf.roc', 'x + 1', 'x + 2')]),
    ('generic import dispatches a third module',
     {'Gen.roc': 'module [show]\nshow = |v| v.render()\n', 'Th.roc': TH,
      'Main.roc': 'import Gen\nimport Th\nexpect Gen.show(Th.make()) == "one"\n'}, [('Th.roc', '"one"', '"two"')]),
    ('local generic dispatches an import',
     {'Th.roc': TH, 'Main.roc': 'import Th\nh = |v| v.render()\nexpect h(Th.make()) == "one"\n'}, [('Th.roc', '"one"', '"two"')]),
    ('where clause target',
     {'Lib.roc': 'module [f]\nf : a -> Str where [a.name : a -> Str]\nf = |x| x.name()\n',
      'Main.roc': 'import Lib\nT := [A].{\n  name = |_| h()\n}\nh = || "one"\nexpect Lib.f(T.A) == "one"\n'},
     [('Main.roc', '|| "one"', '|| "two"')]),
    ('structural equality override', {'Th.roc': EQ, 'Main.roc': 'import Th\nexpect { a: Th.make(True) } == { a: Th.make(False) }\n'},
     [('Th.roc', 'is_eq = |_, _| True', 'is_eq = |_, _| False')]),
    ('override behind an import',
     {'Th.roc': EQ, 'Mid.roc': 'module [mk]\nimport Th\nmk = |x| { a: Th.make(x) }\n',
      'Main.roc': 'import Mid\nexpect Mid.mk(True) == Mid.mk(False)\n'}, [('Th.roc', 'is_eq = |_, _| True', 'is_eq = |_, _| False')]),
    ('inspection override',
     {'Th.roc': 'module [T, make]\nT := [A].{\n  to_inspect = |_| "one"\n}\nmake = || T.A\n',
      'Mid.roc': 'module [mk]\nimport Th\nmk = || [Th.make()]\n',
      'Main.roc': 'import Mid\nexpect {\n  dbg Mid.mk()\n  True\n}\n'}, [('Th.roc', '"one"', '"two"')]),
    ('numeral conversion in a pattern',
     {'Nm.roc': NUM, 'Main.roc': 'import Nm\nf = |x| match x {\n  5 => "five"\n  _ => "other"\n}\nexpect f(Nm.make(1)) == "five"\n'},
     [('Nm.roc', 'Ok({ v: 1 })', 'Ok({ v: 2 })')]),
    ('numeral conversion through a generic literal',
     {'Nm.roc': NUM, 'G.roc': 'module [one]\none = || 5\n',
      'Main.roc': 'import Nm\nimport G\nexpect {\n  x : Nm.N\n  x = G.one()\n  x.get() == 1\n}\n'},
     [('Nm.roc', 'Ok({ v: 1 })', 'Ok({ v: 2 })')]),
    ('quote conversion',
     {'Q.roc': QUOTE, 'Main.roc': 'import Q\nexpect {\n  x : Q.Q\n  x = "hi"\n  Q.get(x) == 1\n}\n'},
     [('Q.roc', 'Ok({ v: 1 })', 'Ok({ v: 2 })')]),
    ('compile-time constant', {'Lib.roc': 'module [f]\nf = |x| x + 1\n', 'Main.roc': 'import Lib\nk = Lib.f(1)\nexpect k == 2\n'},
     [('Lib.roc', 'x + 1', 'x + 2')]),
    ('imported constant', {'Lib.roc': 'module [k]\nk = List.len([1, 2])\n', 'Main.roc': 'import Lib\nexpect Lib.k == 2\n'},
     [('Lib.roc', '[1, 2]', '[1, 2, 3]')]),
    ('stored closure', {'Lib.roc': 'module [g]\nmk = |x| |y| x + y\ng = mk(1)\n', 'Main.roc': 'import Lib\nexpect Lib.g(1) == 2\n'},
     [('Lib.roc', 'mk(1)', 'mk(2)')]),
    ('type a use elsewhere pinned', {'Main.roc': 'v = 300\nw : U64\nw = v\nexpect v.to_str() == "300"\n'},
     [('Main.roc', 'w : U64', 'w : Dec')]),
    ('alias', {'Lib.roc': 'module [T, f]\nT : U8\nf : U64 -> T\nf = |x| x.to_u8_wrap()\n', 'Main.roc': 'import Lib\nexpect Lib.f(300) == 44\n'},
     [('Lib.roc', 'T : U8', 'T : U16')]),
    ('field default', {'Lib.roc': 'Lib := { v : U64 ?? h() }.{}\nh = || 1\n', 'Main.roc': 'import Lib\nexpect Lib.{}.v == 1\n'},
     [('Lib.roc', '|| 1', '|| 2')]),
    ('associated item', {'Ty.roc': 'Ty := [X].{\n  k = 1\n  f = |x| x + k\n}\n', 'Main.roc': 'import Ty\nexpect Ty.f(1) == 2\n'},
     [('Ty.roc', 'k = 1', 'k = 2')]),
    ('imported file', {'a.txt': 'one', 'Lib.roc': 'module [k]\nimport "a.txt" as d : Str\nk = d\n', 'Main.roc': 'import Lib\nexpect Lib.k == "one"\n'},
     [('a.txt', 'one', 'two')]),
    ('inline expect in a helper', {'Lib.roc': 'module [f]\nf = |x| { expect x > 0\n x }\n', 'Main.roc': 'import Lib\nexpect Lib.f(1) == 1\n'},
     [('Lib.roc', 'x > 0', 'x > 5')]),
    ('local package', {'pkg/main.roc': 'package [Value] {}\n', 'pkg/Value.roc': 'Value := {}.{ value = 1 }\n',
                       'Main.roc': 'app [main!] { dep: "./pkg/main.roc" }\nimport dep.Value\nexpect Value.value == 1\nmain! = |_| Ok({})\n'},
     [('pkg/Value.roc', 'value = 1', 'value = 2')]),
]


def identity_checks(f):
    for index, (name, files, edits) in enumerate(IDENTITY_CASES):
        case = Fixture(f.root / f'identity-{index}', f.roc, f.specialize)
        case.cache, case.env = f.cache, f.env
        for file, source in files.items():
            (case.root / file).parent.mkdir(parents=True, exist_ok=True)
            case.write(source, file)
        case.check()
        complete_hit(case)
        before = case.run(cache=False)
        for file, old, new in edits:
            source = (case.root / file).read_text()
            assert old in source, name
            case.write(source.replace(old, new), file)
            fresh = case.run(cache=False)
            assert (transcript(before.stdout), transcript(before.stderr)) != (transcript(fresh.stdout), transcript(fresh.stderr)), name
            cached = case.run(timings=True, work=True)
            assert counter(cached, 'test_root_execution') >= 1, name
            cached = case.run()
            same_report(cached, fresh)
            case.write(source, file)
        # The earlier outcome is reusable as soon as its inputs return.
        complete_hit(case)


def command_checks(f):
    other = 'yes' if f.specialize == 'no' else 'no'
    passing = 'helper = |x| x + 1\nexpect helper(1) == 2\nexpect { dbg "kept"\n    True }\n'
    f.write(passing)
    f.check(2)
    complete_hit(f)
    same_report(f.run(), f.run(cache=False))

    # A complete hit replays diagnostics, failure values and the exit status.
    failing = ('import "input.txt" as data : Str\n'
               'unused_constant = { dbg "compile-time"\n    1 }\n'
               'expect { dbg "first"\n    data == "bytes" }\n'
               'expect { value = 2\n    dbg "second"\n    value == 3 }\n'
               'expect { crash "boom" }\n'
               'expect { bad : Try(U64, [Oops])\n    bad = Err(Oops)\n    bad? == 1 }\n')
    f.write('other', 'input.txt')
    f.write(failing)
    f.check(4)
    assert complete_hit(f).returncode == 1
    same_report(f.run(), f.run(cache=False))

    # Failure to success to failure: every state is its own durable command.
    fixed = failing.replace('data == "bytes"', 'data == "other"').replace('value == 3', 'value == 2')
    f.write(fixed)
    f.check(4)
    f.write(failing)
    complete_hit(f)
    same_report(f.run(), f.run(cache=False))
    f.write('bytes', 'input.txt')
    f.check(4)
    f.write('other', 'input.txt')
    complete_hit(f)

    # --no-cache neither reads nor publishes.
    bypass = f.run(cache=False, timings=True, work=True)
    assert counter(bypass, 'test_root_execution') == 4
    f.write('expect 11 == 11\n')
    fresh = f.run(cache=False, timings=True, work=True)
    assert counter(fresh, 'test_root_execution') == 1
    assert counter(f.run(cache=False, work=True), 'test_root_execution') == 1
    assert counter(f.run(timings=True, work=True), 'test_root_execution') == 1, 'a --no-cache run published'

    # Strategy and execution mode each own their outcomes and commands.
    f.write(passing)
    complete_hit(f)
    swapped = Fixture(f.root, f.roc, other)
    swapped.cache, swapped.env = f.cache, dict(f.env)
    swapped.env.pop('ROC_TRACE_BOXY_WORK', None)
    swapped.specialize = other
    assert counter(swapped.run(work=True), 'test_root_execution') == 2
    for mode in ('interpreter', 'size', 'speed'):
        run = f.run(timings=True, mode=mode)
        f.hits(run, 0)
        f.check(2, mode=mode)
    complete_hit(f)

    # Reported positions follow the current source with the outcome unchanged.
    moved = '# a comment moves everything\n\n' + failing
    f.write(failing)
    complete_hit(f)
    f.write(moved)
    run = f.run(timings=True, work=True)
    assert counter(run, 'test_root_execution') == 0
    f.check(4)

    # Identical bodies stay separate reports, each at its own source position.
    twice = 'expect { dbg "same"\n    1 == 2 }\n'
    f.write(twice + '\n' + twice)
    f.check(2)
    report = complete_hit(f).stderr
    assert report.count(b'Main.roc:1:1') == 1 and report.count(b'Main.roc:4:1') == 1, report

    # A selection is a root: a module tested alone reuses the outcomes of the
    # same roots tested through an importer, under its own command record.
    lib = 'module [f]\nf = |x| x + 1\nexpect f(1) == 2\n'
    f.write(lib, 'Lib.roc')
    f.write('import Lib\nexpect Lib.f(2) == 3\n')
    f.check(2)
    alone = Fixture(f.root, f.roc, f.specialize)
    alone.cache, alone.env, alone.path = f.cache, f.env, f.root / 'Lib.roc'
    run = alone.run(timings=True, work=True)
    assert counter(run, 'test_root_execution') == 0
    f.hits(run, 1)
    complete_hit(alone)
    complete_hit(f)


def resource_checks(f):
    # Running out of stack depends on the executing thread, which no identity
    # records, so that root always runs again; its neighbour's result is kept.
    f.write('deep : U64 -> U64\ndeep = |n| 1 + deep(n + 1)\nexpect deep(0) == 0\nexpect 1 == 1\n')
    for mode in ('dev', 'interpreter'):
        first = f.run(timings=True, mode=mode)
        assert first.returncode == 1 and b'overflowed its stack' in first.stderr, first.stderr
        again = f.run(timings=True, mode=mode)
        f.hits(again, 1)
        assert b'Completed test command' not in again.stderr
    assert counter(f.run(work=True), 'test_root_execution') == 1

    # A compile error is never an outcome, and fixing it runs every root.
    f.write('expect 1 == 1\nexpect missing == 1\n')
    for _ in range(2):
        assert f.run().returncode == 1
    f.write('missing = 1\nexpect 1 == 1\nexpect missing == 1\n')
    f.check(2)

    # An interrupted invocation publishes nothing for the root it was running.
    f.write('spin : U64 -> U64\nspin = |n| if n == 0 { 0 } else { spin(n) }\nexpect spin(1) == 0\n')
    args = [str(f.roc), 'test', str(f.path), f'--specialize={f.specialize}', '--opt=dev', '--no-color', '--jobs=1']
    process = subprocess.Popen(args, env=f.env, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    time.sleep(3)
    process.kill()
    process.wait()
    f.write('spin : U64 -> U64\nspin = |n| if n == 0 { 0 } else { spin(n - 1) }\nexpect spin(1) == 0\n')
    f.check(1)


def damage_checks(f):
    source = 'expect 21 == 21\nexpect { dbg "kept"\n    22 == 23 }\n'
    f.write(source)
    f.check(2)
    complete_hit(f)
    baseline = f.run(cache=False)
    records = [p for p in f.cache.rglob('*') if is_command_record(p)]
    assert len(records) >= 2, 'no completed command records published'

    def repaired():
        same_report(f.run(), baseline)
        complete_hit(f)

    originals = {path: path.read_bytes() for path in records}
    for damage in (lambda d: d[:len(d) // 2], lambda d: d[:len(COMMAND_MAGIC)], lambda d: b'',
                   lambda d: d[:-1] + bytes([d[-1] ^ 1]), lambda d: d + b'x'):
        for path, data in originals.items():
            path.write_bytes(damage(data))
            repaired()
            # Publication replaces the damaged entry at its own path.
            path.write_bytes(data)
    outcomes = {path: path.read_bytes() for path in f.cache.rglob('*') if is_outcome(path)}
    assert outcomes, 'no outcomes published'
    for damage in (lambda d: d[:len(d) // 2], lambda d: b'', lambda d: d[:40] + bytes([d[40] ^ 1]) + d[41:]):
        for path, data in outcomes.items():
            path.write_bytes(damage(data))
        repaired()
        for path, data in outcomes.items():
            path.write_bytes(data)
    # An evicted outcome invalidates every command that refers to it.
    for path in outcomes:
        path.unlink()
    repaired()


def concurrency_checks(f):
    """Racing invocations in one tree and in separate trees over one cache."""
    copies = [f.root / f'concurrent-{index}' for index in range(4)]
    sources = ['expect 31 == 31\nexpect { dbg "race"\n    32 == 33 }\n',
               'expect 31 == 31\nexpect { dbg "race"\n    32 == 32 }\n']
    for path, source in zip(copies, sources * 2):
        path.mkdir()
        (path / 'Main.roc').write_text(source)
    fixtures = []
    for path in copies:
        fixture = Fixture(path, f.roc, f.specialize)
        fixture.cache, fixture.env = f.cache, f.env
        fixtures.append(fixture)
    expected = [fixture.run(cache=False) for fixture in fixtures]
    with ThreadPoolExecutor(max_workers=8) as pool:
        for _ in range(3):
            work = [pool.submit(fixture.run) for fixture in fixtures * 2]
            results = [future.result() for future in work]
            for result, fresh in zip(results, expected * 2):
                same_report(result, fresh)
    for fixture, fresh in zip(fixtures, expected):
        complete_hit(fixture)
        same_report(fixture.run(), fresh)


def checkout_checks(f):
    """Checkouts, working directories and fetched roots are not identities."""
    first = f.root / 'checkout' / 'one'
    second = f.root / 'elsewhere' / 'much' / 'deeper' / 'two'
    (first / 'pkg').mkdir(parents=True)
    (first / 'Lib.roc').write_text('module [f]\nf = |x| { dbg "lib"\n    x + 1 }\n')
    (first / 'pkg' / 'main.roc').write_text('package [Value] {}\n')
    (first / 'pkg' / 'Value.roc').write_text('Value := {}.{ value = 1 }\nexpect value == 1\n')
    (first / 'input.txt').write_text('bytes')
    (first / 'Main.roc').write_text('app [main!] { dep: "./pkg/main.roc" }\nimport dep.Value\nimport Lib\n'
                                    'import "input.txt" as data : Str\n'
                                    'expect Lib.f(Value.value) == 2\nexpect data == "other"\nmain! = |_| Ok({})\n')
    one = Fixture(first, f.roc, f.specialize)
    one.cache, one.env = f.cache, f.env
    assert one.run().returncode == 1
    complete_hit(one)
    second.parent.mkdir(parents=True)
    shutil.copytree(first, second)
    two = Fixture(second, f.roc, f.specialize)
    two.cache, two.env = f.cache, f.env
    complete_hit(two)
    moved = two.run()
    assert str(first).encode() not in moved.stderr + moved.stdout, moved.stderr
    same_report(moved, two.run(cache=False))
    (first / 'Lib.roc').write_text('module [f]\nf = |x| { dbg "lib"\n    x + 2 }\n')
    run = one.run(timings=True, work=True)
    assert counter(run, 'test_root_execution') == 1
    complete_hit(two)


def lowering_policy_checks(f):
    # A Boxy test reuses outcomes without entering the native test artifact
    # cache, whichever of its runs populated the outcome.
    f.write('expect 41 == 41\n')
    f.check(1)
    f.write('expect 41 == 42\n')
    run = f.run(timings=True)
    assert b'Native test artifact cache' not in run.stderr, run.stderr


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--roc', type=Path, required=True)
    args = parser.parse_args()
    for specialize in ('yes', 'no'):
        with tempfile.TemporaryDirectory(prefix='roc-test-outcomes-') as temp:
            fixture = Fixture(Path(temp), args.roc.resolve(), specialize)
            exercise(fixture)
            relocation_checks(fixture)
            command_checks(fixture)
            resource_checks(fixture)
            identity_checks(fixture)
            damage_checks(fixture)
            concurrency_checks(fixture)
            checkout_checks(fixture)
            if specialize == 'no':
                lowering_policy_checks(fixture)
    print('focused test-outcome cache checks passed under both strategies')


if __name__ == '__main__':
    main()
