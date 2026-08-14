# PR #9832 — Add list sort builtins using a new ordering API

- **Author:** jrrrp (Jonathan Paynter) · **Draft:** yes · **Base:** `main`
- **Size:** +115 / −93 across 8 files

Renames `List.sort_with` → `List.sort`, replaces the `[LT, EQ, GT]` ordering
tags with `[FirstBeforeSecond, Equivalent, SecondBeforeFirst]`, and adds
`List.sort_with_default` and `List.sort_by` constrained on a new
`item.default_cmp` method.

## Verdict

The motivating idea is good — decoupling "ordering" from "less than" is the
right call, and the Zulip reasoning holds up. But as it stands the PR **does not
compile** (#1), **the two new APIs are unusable because nothing implements
`default_cmp`** (#2), and it leaves the builtins with **two incompatible
ordering vocabularies** (#3).

It's a draft and the author explicitly asks for naming feedback, so most of this
is design input rather than merge blocking. I've answered the naming question at
the end.

---

## Findings

### 1. (Blocking) A stray `+` corrupts `parallel_cli_runner.zig`

```diff
-    .{ .id = 0, .suite = .subcommands, .name = "roc test issue 9388 List.sort_with top-level expect does not overflow", ... },
-    .{ .id = 0, .suite = .subcommands, .name = "roc test issue 9769 derived record equality is stable across expects", ... },
++    .{ .id = 0, .suite = .subcommands, .name = "roc test issue 9769 derived record equality is stable across expects", ... },
+    .{ .id = 0, .suite = .subcommands, .name = "roc test issue 9388 List.sort top-level expect does not overflow", ... },
```

Note the `++` on that line: the *added* line literally begins with a `+`
character, so the file gets

```zig
+    .{ .id = 0, .suite = .subcommands, .name = "roc test issue 9769 ...
```

which is a syntax error. This looks like a botched hand-edit or conflict
resolution. It's also why nothing else here has been validated — the test suite
can't have run.

(The reordering of those two entries appears to be incidental churn from the same
edit; worth reverting to minimize the diff.)

### 2. (Blocking) `sort_with_default` and `sort_by` can never be called — nothing implements `default_cmp`

Both new functions are constrained on a method that does not exist:

```roc
sort_with_default : List(item) -> List(item)
    where [item.default_cmp : item, item -> [FirstBeforeSecond, Equivalent, SecondBeforeFirst]]
```

`default_cmp` appears **exactly four times in the entire diff** — twice in these
two `where` clauses and twice in their bodies. It is not implemented on `U64`,
`I64`, `Str`, tuples, or anything else. So:

```roc
[3, 1, 2].sort_with_default()
```

— the doc example on `sort_with_default` itself — will not compile.

The PR body says integers and tuples "are two such primitives where it may make
sense to have a default ordering," which reads as intent rather than
implementation. Either add `default_cmp` to the numeric types (and tuples) in
this PR, or hold `sort_with_default`/`sort_by` back until the method exists.
Shipping two constrained functions with no inhabitants is worse than shipping
neither.

### 3. (Blocking, design) The builtins now have two incompatible ordering vocabularies

`Num.*.compare` still returns the old tags — for **every** numeric type:

```
src/build/roc/Builtin.roc:6508   compare : U8,   U8   -> [LT, EQ, GT]
src/build/roc/Builtin.roc:7169   compare : I8,   I8   -> [LT, EQ, GT]
…  (11 numeric types) …
src/build/roc/Builtin.roc:14550  compare : Dec,  Dec  -> [LT, EQ, GT]
src/build/roc/Builtin.roc:22344  numeric_compare : item, item -> [LT, EQ, GT]
```

Consequently the most obvious thing a user will try —

```roc
[3, 1, 2].sort(U64.compare)
```

— does not type-check, because `U64.compare` produces `[LT, EQ, GT]` and `sort`
wants `[FirstBeforeSecond, Equivalent, SecondBeforeFirst]`. The user is forced to
write the six-token adapter lambda that the PR's own examples show.

This also explains #2: `numeric_compare` is exactly the function
`default_cmp` should delegate to, and it's on the wrong vocabulary.

The conversion needs to cover `Num.*.compare` and `numeric_compare` too (30
occurrences of the old tags in `Builtin.roc`), or the PR needs a stated plan for
the two coexisting. Half-converting is the worst of both worlds.

### 4. (Bug) The descending-sort doc example is wrong

```roc
## # Sort in descending order by swapping the tags
## expect [3, 1, 2].sort(|a, b| if a < b SecondBeforeFirst else if a > b SecondBeforeFirst else Equivalent) == [3, 2, 1]
```

Both branches return `SecondBeforeFirst`. The comparator claims "b comes first"
whether `a < b` **or** `a > b`, which is not a valid ordering and will not
produce `[3, 2, 1]`.

Should be `if a < b SecondBeforeFirst else if a > b FirstBeforeSecond else Equivalent`.
(The updated tests and REPL snapshot get this right — e.g.
`if a > b FirstBeforeSecond else if a < b SecondBeforeFirst else Equivalent` — so
it's only the doc comment.)

Worth noting these `##` examples are run as doc tests in some setups; if they
are here, this one would fail.

### 5. (Bug) `sort_by`'s doc example is `sort_with_default`'s, copy-pasted

```roc
## Sort a list according to a key function.
## The key function must return a type that can be compared by default.
## ```roc
## expect [3, 1, 2].sort_with_default() == [1, 2, 3]
## ```
sort_by : List(item), (item -> key) -> List(item)
```

The example doesn't call `sort_by` and passes no key function. Something like
`expect [{n: 3}, {n: 1}].sort_by(|r| r.n) == [{n: 1}, {n: 3}]` is what belongs
here.

### 6. (Design) Collapsing three-way to two-way costs correctness and speed

```roc
sort = |list, order| {
    is_leq = |a, b| match order(a, b) {
        FirstBeforeSecond => True
        Equivalent => True
        SecondBeforeFirst => False
    }
    sort_impl(list, is_leq)
}
```

and then `sort_impl` partitions with:

```roc
less_or_equal = List.keep_if(rest, |item| is_leq(item, pivot))
greater       = List.keep_if(rest, |item| !is_leq(item, pivot))
```

Three consequences:

- **The user's comparator runs twice per element per level.** Two full
  `keep_if` traversals where one pass building both partitions would do. The old
  `sort_with` had the same shape, so this isn't a regression — but the PR is
  promoting this implementation to the primary `sort` name and layering two more
  entry points on it.
- **The three-way information is discarded.** Having just introduced a
  distinguished `Equivalent`, the implementation immediately folds it into
  `True`. A three-way partition (`before` / `equivalent` / `after`) would use the
  information, avoid re-running the comparator, and give correct handling of many
  duplicates — the classic quicksort weakness.
- **The sort is not stable.** Elements comparing `Equivalent` to the pivot land
  in `less_or_equal`, i.e. **before** the pivot, even when they came after it in
  the input. Stability is invisible for `sort` on numbers but very visible for
  `sort_by` on a key, which is exactly the API this PR adds. Either make it
  stable or document that it isn't.

### 7. (Design) First-element pivot is O(n²) on sorted input

`sort_impl` takes `List.first(list)` as the pivot, so an already-sorted or
reverse-sorted list degrades to quadratic time with two allocations per level.
The test suite includes "already sorted" and "reverse sorted" cases — but at
5 elements, so they pass without exercising the problem.

The TODO ("They will eventually be re-implemented in Zig") is the right answer
and I wouldn't ask for a better algorithm in Roc here. But `List.sort` becoming
the blessed name makes this more load-bearing than `sort_with` was, so it's
worth a `##` note that the current implementation is not suitable for large
lists, or a tracking issue.

### 8. (Should add) A rename this size needs a migration hint

`List.sort_with` is removed outright and the `LT`/`EQ`/`GT` tags with it. Every
existing user gets an unexplained "does not exist" error. PR #10410 is adding
exactly this kind of hint for `Num.to_str` / `Inspect.to_str`; the same treatment
for `List.sort_with` → `List.sort` (and a note about the tag rename, which is the
part people won't guess) would be consistent and cheap.

### 9. (Nit) Formatting and leftovers

- **Indentation switches from tabs to spaces** in all the new code
  (`sort`'s body, the `where` clauses, `sort_impl`). `Builtin.roc` is
  tab-indented throughout. This will fail a `roc fmt` check if one runs over
  builtins, and it makes the diff noisier than it is.
- **Double blank line** introduced before `## Returns \`True\` if the two lists…`.
- **`# TODO - is there syntax for looking up the function for the module?`** is
  left in shipped builtin source. Answer it or drop it — and if the answer is
  "no," `cmp = |a, b| a.default_cmp(b)` allocating a wrapper closure per call is
  worth a note.
- **`test/snapshots/repl/list_sort_with.md`** keeps its filename while its
  `description` becomes "List.sort". Rename the file to `list_sort.md`.

## Answering the naming question

The author asks directly, so:

**On the method names.** The proposed set inverts the usual convention:

```
List.sort(list, comparison_func)      # requires a comparator
List.sort_with_default(list)          # the zero-argument one
```

Almost every language makes the bare `sort()` the no-argument default and gives
the custom-comparator version the longer name. Suggested instead:

```
List.sort(list)                       # where [item.default_cmp : …]
List.sort_with(list, cmp)             # keeps the existing name, no migration
List.sort_by(list, key_fn)
```

That also **eliminates the rename** in #8 — `sort_with` keeps its name and
meaning, only its tag type changes.

For descending, `_desc` reads better than `_rev` (`rev` suggests "reverse the
result," `desc` says "order descending"), and it sidesteps the
`sort_with_default_rev` mouthful the author flagged: `List.sort_desc(list)`.

**On the tag names.** `FirstBeforeSecond` / `SecondBeforeFirst` are unambiguous
but 17 characters each, and an inline comparator becomes 100+ columns — the
PR's own examples demonstrate the problem. The names carry the "which argument"
information twice, since the argument order is already given by the function
signature. Relative-to-the-first-argument naming is just as clear and a third
the width:

```roc
[3, 1, 2].sort(|a, b| if a < b Before else if a > b After else Same)
```

`Before` / `Same` / `After` keeps the decoupling from `<`/`>` that motivated the
change (nothing about them implies numeric comparison), reads naturally left to
right, and makes `sort_by` comparators writable inline. If `Same` feels too
loose, `Before` / `Equivalent` / `After` is still a big improvement.
