# Parser compiler issues: #13204, #13207, #13208, #13213

All four issues are assigned to `rtfeldman`. All four reported invariant
failures reproduce on the current worktree's `debug-90f1d6d6` compiler on
Linux x86-64, built with Zig 0.17.1:

```sh
zig build roc -Doptimize=Debug -Dstrip=false -j2
```

The traces below describe the pre-fix compiler. With the four candidate
changes, a clean Debug build passes the #13204 and #13213 unit tests.
The unmodified, checksum-verified #13204 release archive (`6.0.0-rc.3`)
checks without errors. The alternate checkout used by the driver no
longer crashes either, but reports the two AsciiDoc and one QIF typing
errors documented in #13207. The #13207 Text fuzz build and the #13208
optimized XML build both produce executables. The #13213 Records build
also produces its fuzz executable. After fixing checked row substitution,
it exposed a second failure at `(iterator.step)()`: the public field
projection and private backing shared the step's callable slot but not its
result row. A later join gave the procedure a 16-variant nested row where
the caller still expected 12. Relating the argument and return types at
the callable boundary keeps both call ABIs equal without merging the
enclosing backing records. A Lambda Solved unit test checks both properties.

## Pinned full-input reproductions

Download and extract these public source archives into separate directories:

- #13204: the published `6.0.0-rc.3` package, SHA-256
  `50384173e61afd07662dddb96bae8360f0589cf43aad03975454df29f7776265`:
  https://github.com/lukewilliamboswell/roc-parser/releases/download/6.0.0-rc.3/FTL1xt1rA9nXzoYkhFAW9yF75Kn52bS3cpBr7wZNfPq2.tar.zst
- #13207, #13208:
  https://codeload.github.com/lukewilliamboswell/roc-parser/tar.gz/0113bc9e9e8aee0277b67223ae1923b114849682
- #13213:
  https://codeload.github.com/lukewilliamboswell/roc-parser/tar.gz/faa9297b4419b26a5255e9ca7c39eb3d9f816f74

Pass the extracted package root for #13204 and the repository root for the other three:

```sh
python3 test/repros/parser-compiler-issues/reproduce.py 13204 --roc zig-out/bin/roc --source /path/to/rc3
python3 test/repros/parser-compiler-issues/reproduce.py 13207 --roc zig-out/bin/roc --source /path/to/original
python3 test/repros/parser-compiler-issues/reproduce.py 13208 --roc zig-out/bin/roc --source /path/to/original
python3 test/repros/parser-compiler-issues/reproduce.py 13213 --roc zig-out/bin/roc --source /path/to/records
```

The driver copies the input into a private temporary tree and applies only
the compatibility edits documented in the issues. It never edits the input
directory or a dependency cache. `--prepare-only --work-dir /new/path`
retains a prepared input for debugging. The compiler commands printed by the
driver were run directly against those prepared inputs; all four exited 134.
All four driver cases also failed their success assertions with SIGABRT:

| Issue | Phase | Failure |
| --- | --- | --- |
| #13204 | checking `XmlNsCore` | `storedValueVar`: non-scheme stored child has generalized rank |
| #13207 | Lambda Solved | `a scheme node was not a class root` |
| #13208 | SpecConstr | `record update changed the type of an unmodified field in SpecConstr` |
| #13213 | Monotype | `checked type key row normalization found duplicate tags` |

## #13208: unchanged record fields lose producer representations

The standalone `StreamRecordUpdate.roc` eliminates roc-parser and roc-fuzz:

```sh
zig-out/bin/roc build --no-cache --opt=speed test/repros/parser-compiler-issues/StreamRecordUpdate.roc --output=/tmp/stream-record-update
```

It reproduces the exact SpecConstr invariant. The same standalone program
builds without errors or warnings under `--opt=dev` and runs to completion.

The unchanged `chunks` field is a producer-minted `Stream` in the update's
base, but a checked-public `Stream` in the result. These have different
runtime backing types. In the full XML example the base field's Monotype
is `minted/from_iter`, with `generated_private` backing; the result field
has no iterator representation and has `checked_public` backing.

The malformed update already exists in the immutable input to SpecConstr.
In the standalone program, source expression 102 (the base) has type 91
whose `chunks` field is type 83, while source expression 106 (the update)
has type 87 whose `chunks` field is type 93. Cloning the base preserves
type 91. SpecConstr therefore detects, rather than creates, the mismatch.

`BodyContext.lowerRecordExprAtNode` in
`src/postcheck/monotype/lower.zig:53150-53185` starts its result fields from
the checked target and chooses producer witnesses only for explicitly
updated fields. Its `labels.values.get(field.name) orelse continue` leaves
unchanged fields at the checked-public representation. The sealed-type
update path at `52921-52979` also needs to obey the same rule.

**Recommended fix:** make record-update lowering explicitly preserve each
unchanged field's producer type from the lowered base. Relate those slots
to the checked result in the live graph before sealing it, and construct
the result witness from the unchanged base slots plus the replacement
values' slots. Preserve nominal identity and optional/defaulted-field
metadata. Both update-lowering paths must consume this one rule.
SpecConstr should retain its assertion: weakening equality there would
admit an update that copies bytes between incompatible representations.

## #13213: substituting a row tail manufactures duplicate labels

A focused test is in `src/check/checked_artifact.zig`:

```sh
zig build run-test-zig-module-check -j2 -Dtest-filter='issue 13213' -- --test-filter 'issue 13213'
```

Confirmed result: 0 passed, 1 crashed, with the exact duplicate-tags
invariant. It substitutes `[Oops]` for `e` in `[Oops, ..e]` and asserts
that the result's canonical key equals `[Oops]`'s key.

The full Records input fails while indexing synthesized checked types.
The offending synthesized row extends source row 3221, which extends
3572. The two occurrences of `SourceErr` have the same payload type
(503), so this is not a source type conflict.

`cloneCheckedTypeBuildPayload` at
`src/check/checked_artifact.zig:6247-6258` independently copies the head
tags and substitutes the extension. `cloneCheckedTypeRootSubstituting`
then publishes that payload at `6004-6007` without applying row-union
semantics. A valid substitution can therefore create a repeated label
that the original checked scheme did not contain.

**Recommended fix:** make checked-type substitution construct normalized
row unions. Compose the substituted head and extension by label,
preserving shared suffixes and recursive identities; compatible repeated
labels denote one tag with the checked payload relation. Normalize in
the substitution producer before publishing/keying the synthesized
graph, including mutually recursive substitutions. Do not silently drop
duplicates in the key writer: that would leave other checked-type
consumers seeing a different graph from its canonical identity.

## #13204: a field projection sees another annotation's generalized class

The failing stored child is `bound.entries` in `resolve_source_start`
(`XmlNsCore.roc:1730`), not `opened.next()`. Expression 10069 resolves to
generalized root 16317. Its presence in the rank-2 predeclaration pool for
`source_unbind_to` was not the erroneous write: after rank adjustment the
`List(Entry)` class and its `Entry` argument were both at **outermost**
rank, and the list was not in that annotation's retained body-owned set.

A watchpoint following descriptor 15923 across store reallocations caught
the actual transition from outermost (1) to generalized (0). It was in a
later definition's **rank adjustment**, not quantification:
`stepOverArgs -> settleRank`, called with `group_rank = outermost` while
adjusting a rank-2 binding. `stepOverArgs` starts the maximum of child
ranks at `generalized`; when the child contributes only zero it writes
zero to the enclosing, already escaped list. This uses the same number
for “no contribution from a child” and “this class is quantified.”
The subsequent projection still points to that same shared class.

**Recommended fix:** separate lexical rank adjustment from authorization
to quantify. An already escaped class must stay at a live rank; it
cannot become generalized as a side effect of reducing its children's
ranks. Carry an explicit “no live child rank” result during adjustment
and resolve it using the class's ownership: an escaped class remains at
its enclosing rank; an eligible current-frame class can go through
ordinary quantification. Preserve already-generalized children. Apply
the rule in `src/types/generalize.zig` at the shared structural-rank
reduction, not by disabling annotation caching or weakening
`storedValueVar`. Test both the escaped case and a genuinely owned
container of generalized children; flooring every container at outermost
would incorrectly prevent the latter from becoming polymorphic.

The focused regression in `src/types/generalize.zig` exercises the
same `stepOverArgs` path with two containers sharing one generalized
child. The enclosing-scope container must keep its rank and the
current-scope container must generalize. Reproduce with
`zig build run-test-zig-module-types -j2 -Dtest-filter='issue 13204' -- --test-filter 'issue 13204'`:
`expected .outermost, found .generalized`.

## #13207: a later body mutates a published lambda scheme

A watchpoint set immediately after `generalizeGroup` showed that group
258 published root 216940 in its four-node scheme. Later, while solving
function 1914, `expectExprSlot` related a reference to function 3197
(expression 801775) to expected type 9713. That is not a private type
of group 258: it is the registered type of the second argument of
function 1073, in the **preceding group 257**. `applyUnifyFinish`
redirected the published root to 235262.
Only afterwards did a separate reference to function 1155 try to
instantiate the scheme and find a link instead of its original content.

Group 257 stayed monomorphic because it shared expressions.
`generalizeGroup` returned at `solve.zig:1181`, before the ownership
walk at `1216-1251`. When group 258 finished solving, the preceding
group's argument 9713 already linked to root 216940, and root 216940
still had the sentinel owner `unowned`. `generalizeGroup` accepted the
root, assigned it owner 259 (group 258), and published it. The mistake
is already visible at that point, before any later reference mutates it.

**Recommended fix:** separate establishing a group's type ownership from
choosing whether to generalize it. When a group's signature becomes
available to callers, its reachable classes must have an owner even if
the group publishes an empty scheme. Claim the signature graph for its
group, retaining any older owner, including through captures and nested
instantiation images; lazy children must inherit the settled owner. Do
this before *any* monomorphic early return. Unification then preserves
the older owner, and group 258 cannot quantify group 257's argument.
Use the existing signature traversal and reusable visited storage, not
a rescan of all prior groups for each new scheme. Following the link in
`instantiate` would copy the later caller's solution and would not repair
the scheme's root-to-position map.
