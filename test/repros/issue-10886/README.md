# Issue 10886: result context reaches generic call arguments too late

Issue: https://github.com/roc-lang/roc/issues/10886

Reproduced with Zig 0.17.1 and the initial Debug compiler
(`debug-7c9ea425`). The two original checker reproductions now pass unchanged
after implementing expected-result guidance before argument checking.

## Reproduction

Two tests in `src/check/test/type_checking_integration.zig:1366` assert
successful checking:

- A `Set(Foo)` result annotation with mixed omitted/supplied defaulted
  fields in the argument to `Set.from_list`.
- The smaller equivalent: `items : List(Foo)` with
  `items = identity([{ a: 123 }, { a: 456, b: 789 }])`, where `identity`
  is polymorphic.

The nominal declaration follows the issue's reopening comment. The original
structural alias with defaults is no longer legal language syntax. The Set
case explicitly derives `is_eq` and `to_hash`, which current nominal types
need independently of this bug.

```sh
zig build -j2 run-test-zig-module-check \
  -Dtest-filter='issue 10886' -Dtest-filter='issue 13067' -- \
  --test-filter 'issue 10886' --test-filter 'issue 13067'
```

Confirmed result: 3 passed, 2 failed. Both new 10886 tests fail with:

```text
The two elements in this list have incompatible types.
Hint: This record is missing the field: `b`
```

The existing 10886 tests and the new 13067 test pass. Separate uncached CLI
controls pass when the records use explicit nominal constructors or pass
through a monomorphic `Foo -> Foo` identity. A direct monomorphic
`List(Foo) -> U64` parameter also accepts the implicit constructors.

## Root cause observed before the fix

`Check.resumeCallCheck` (`src/check/Check.zig:35421`) instantiates the callee
and establishes its argument/return shape, but checks arguments without
first relating that return shape to the call's expected result.
`forStatement` deliberately drops outer result context (`30246`), and
argument context is subsequently taken only from the callee's formal slots
(`35597`). For a generic call these slots still contain an unconstrained
element variable, even though the result annotation determines `Foo`.

`resumeListCheck` projects that unconstrained formal into its element
accumulator (`33224`), then compares the two freshly constructed records
(`33262`). No declared default kind has reached them, so rejecting unequal
widths is correct at that point. Relating the call's return after its
arguments (`35646`) is too late.

The violated rule is bidirectional construction context: known expected
result structure must constrain shared formal argument variables before
their constructors are checked. This is not a Set implementation problem
or a reason to relax record-width checking.

## Implemented rule

Expected-result propagation is part of ordinary call-shape checking.
After instantiating a callee and establishing arity, project the call's
expected result onto its return slot before checking arguments, preserving
the instantiated signature's shared variables. Use the existing
expected-shape/commit-probe discipline: copy structural context without
duplicating executable dispatch obligations, roll back rejected projections,
and leave the actual owning result relation responsible for diagnostics.
The same rule applies to resolved generic method calls, rather than
special-casing Set or record lists.

The rule preserves left-to-right argument relations, required-field rejection,
nominal opacity, and recorded default-omission identities. It is documented
in `design.md`'s expected-shape/rewrite contract. No new lazy inference system,
global cache, argument rechecking, or lowering-side default reconstruction
is involved.

Unprojected unknown/rigid leaves carry no structural guidance, so they incur
no copy or probe. Already-projected cells retain their shared equality, and
successful contextual relations are kept.

Callback construction must follow the same rule: the existing lambda
parameter-seeding relation also retains its return slot as context for
the body and explicit returns. Record literals retain their complete
projected row, including omitted defaults, before an enclosing return join
can commit their width. Returned calls are not directly unified with this
context: that would incorrectly feed a callback's error row into its callee.
This is not a synthetic annotation and does not replace the owning callable
relation. It preserves the existing issue-11271 omission-ownership regressions.

Additional coverage checks nested generic calls, resolved methods, early
returns, required-field rejection, and dispatch obligations.
`test/cli/issue_10886/Foo.roc` verifies the actual default values through Set
construction and callback returns under both specialization strategies.

See [combined validation and performance results](../record-codec-validation.md).
