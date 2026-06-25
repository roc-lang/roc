# Parser Optimization Experiments

This file lists parser optimization experiments that keep Roc's rich AST and
token model intact while trying to get closer to simdjson's hot-loop behavior.
The current parser already matches the important simdjson-style control-flow
constraint: one stack-safe token walk, no parser VM, no recursive grammar calls,
and no ReleaseFast `br xN` parser-state dispatch in the audited parser kernels.

The remaining opportunities are about reducing branch count, allocator calls,
and unpredictable output-side work without weakening parser output. Every
experiment below needs two proofs before it is accepted:

- ReleaseFast assembly for the changed parser slice must improve or preserve
  the no-indirect-dispatch shape.
- Parser-heavy benchmarks must improve or stay neutral versus the current
  branch and origin/main.

Do not accept an experiment just because source code looks cleaner. The relevant
evidence is generated code and benchmark behavior.

## 1. Token Discriminant Ordering

Reorder `Token.Tag` values so common parser classifications become simple range
tests. The current parser has many contexts that ask questions like:

- Is this a binary operator?
- Is this token a possible expression prefix?
- Is this token a possible pattern prefix?
- Is this token a possible type prefix?
- Is this token a statement-start keyword?
- Is this a closing delimiter?

If those groups are contiguous in the enum, the hot path can use one or two
integer comparisons instead of long equality chains or compiler-generated
lookup tables.

Example target shape:

```zig
const tag_int = @intFromEnum(tok);
if (tag_int >= @intFromEnum(Token.Tag.OpStar) and
    tag_int <= @intFromEnum(Token.Tag.OpOr))
{
    // binary operator
}
```

This is attractive because it keeps all parser output exactly the same. It only
changes internal token numbering. The main risk is that one enum order cannot
make every parser context contiguous at once. The likely best ordering is:

1. Hot expression prefix tokens.
2. Hot expression suffix/binary operator tokens.
3. Delimiters and closers.
4. Pattern/type-only starters.
5. Rare keywords and malformed tokens.

Audit requirements:

- Add tests or comptime assertions documenting each intended contiguous range.
- Disassemble the expression suffix/operator slice and confirm range checks are
  generated instead of equality ladders or jump tables.
- Benchmark a parse-heavy file such as `aoc_day2` and a broader stable corpus.

Reject if:

- The compiler still generates a table dispatch for the hot classification.
- Reordering creates fragile assumptions without comptime checks.
- Other hot contexts get worse enough to offset the intended win.

## 2. Compile-Time Token Classification Tables

Where enum ordering cannot satisfy all contexts, use compact static tables
indexed by token tag. Instead of branching over many tags, the parser performs
one load from a table and then one small decision.

Possible tables:

```zig
const binary_op_info: [token_count]BinaryOpInfo = ...;
const expr_prefix_class: [token_count]ExprPrefixClass = ...;
const pattern_prefix_class: [token_count]PatternPrefixClass = ...;
const type_prefix_class: [token_count]TypePrefixClass = ...;
const statement_start_class: [token_count]StatementStartClass = ...;
```

For binary operators, the table can carry all hot metadata:

```zig
const BinaryOpInfo = packed struct {
    is_op: bool,
    left_bp: u8,
    right_bp: u8,
    ast_tag: AstBinaryOpTag,
};
```

Then expression suffix parsing becomes:

```zig
const info = binary_op_info[@intFromEnum(tok)];
if (!info.is_op or info.left_bp < min_bp) {
    // expression is complete
}
```

This may be better than discriminant ordering when a token belongs to multiple
logical groups or when a group is sparse. The cost is a data load. On modern
hardware, one predictable load from a small hot table can beat a long branch
chain, but this must be measured.

Audit requirements:

- Verify the table is in read-only data and is small enough to remain cache-hot.
- Disassemble the expression suffix path and confirm it is load-plus-compare,
  not a hidden jump table.
- Measure branch misses and instruction count if available through `samply` or
  hardware counters.

Reject if:

- The table load is slower than the current direct comparisons.
- The table pushes more pressure onto cache than the branch chain saved.
- It makes malformed/recovery cases harder to reason about.

## 3. Bitset Token Classification

For sparse token groups with at most 64 or 128 possible tags, represent
membership as one or two integer masks. This keeps classification branchless
without a memory load from a table.

Example:

```zig
const binary_op_mask: u128 = ...;
const tag_int = @intFromEnum(tok);
if (((binary_op_mask >> @intCast(tag_int)) & 1) != 0) {
    // binary operator
}
```

This is useful when the token enum is stable enough that all relevant tags fit
inside a small mask. It is less useful if `Token.Tag` has too many values or if
the compiler lowers large shifts poorly on the target architecture.

Likely targets:

- Binary operator membership.
- Closing delimiter membership.
- Tokens that terminate an expression in recovery mode.
- Keywords that start statement-only forms.

Audit requirements:

- Add comptime assertions that each mask contains exactly the intended tags.
- Check ARM64 assembly for a reasonable shift/test sequence.
- Compare against both range checks and static byte tables.

Reject if:

- The target needs multiword helper code for the mask.
- The bitset is less readable and no faster in measured parser benchmarks.

## 4. Operator Metadata Encoding

The Pratt suffix loop should avoid switch-heavy operator handling. Binary
operator token tags can be mapped directly to binding power, associativity, and
AST operator kind.

There are two versions:

1. Contiguous operator tags with arithmetic derivation.
2. A static `binary_op_info` table.

The arithmetic version is ideal if token ordering allows it:

```zig
const op_index = @intFromEnum(tok) - @intFromEnum(Token.Tag.OpStar);
const info = binary_op_infos[op_index];
```

The table version is more flexible:

```zig
const info = binary_op_info[@intFromEnum(tok)];
if (!info.valid) {
    break;
}
```

This experiment is high value because operator parsing is usually one of the
hottest expression paths. It also simplifies branch prediction: most suffix
iterations perform the same load/check pattern instead of walking many token
cases.

Audit requirements:

- Confirm the suffix path has no switch jump table.
- Confirm the common non-operator expression terminator path remains cheap.
- Benchmark files with many binary operators separately from ordinary files.

Reject if:

- The common no-operator case gets slower.
- The AST operator mapping becomes indirect enough to offset the branch savings.

## 5. Hot/Cold Splitting for Diagnostics and Recovery

Roc's parser must keep rich diagnostics and malformed AST nodes, but those paths
do not need to live inline with the hot successful parse path. Move rare error
construction, recovery scanning, and diagnostic detail assembly into cold helper
functions.

Examples:

- Malformed expression construction after expected delimiters.
- String interpolation error cases.
- Type annotation recovery after missing close delimiter.
- Match branch missing-arrow diagnostics.
- Statement-level unexpected-token recovery.

The hot path should generally do:

```zig
if (tok != expected) {
    return self.coldExpectedCloseRound(...);
}
```

or continue to a cold label/helper that is unlikely to be placed in the middle
of the hot grammar body.

Audit requirements:

- Check that ReleaseFast places the cold path out of line or at least reduces
  hot block size.
- Compare instruction count and I-cache behavior around expression suffix and
  collection close handling.
- Ensure diagnostics remain byte-for-byte identical in snapshots.

Reject if:

- Helper calls appear on common valid syntax paths.
- Error snapshots change without a deliberate diagnostic improvement.
- The compiler inlines the cold helpers back into the hot path.

## 6. Scratch and Parent Stack Pre-Reservation

The remaining indirect calls in the audited parser kernels are allocator calls
in growth/copy paths, not parser-state dispatch. We can reduce their frequency
by reserving capacity for parser-owned scratch structures before the hot walk.

Possible heuristics, all parser-local and allowed:

- Reserve parent stacks based on token count divided by a conservative factor.
- Reserve expression/pattern/type scratch spans based on token count.
- For delimited collections, scan forward to the matching close token when
  already doing delimiter recovery and reserve child capacity for that local
  collection.
- Maintain high-water marks in parser scratch state and reuse capacity between
  parses in the same parser instance.

This does not change AST shape. It only tries to avoid `ArrayList` growth calls
while parsing normal input.

Audit requirements:

- Confirm `blr x8` allocator calls become rarer in sampled profiles.
- Benchmark memory usage as well as parse time.
- Stress deeply nested and very small files; pre-reservation should not waste
  significant memory for tiny parses.

Reject if:

- Pre-reservation increases total memory enough to hurt real workloads.
- The reservation logic adds more branches than the allocator calls cost.

## 7. More Parent-Specific Payload Storage

The parser already moved away from wide tagged frames, but some parent payload
flows can become even more concrete. A parent kind that always has one payload
type can use a dedicated stack with exactly that payload layout and no generic
copy helper.

Examples:

- Binary RHS state.
- Lambda body state.
- Collection state.
- Type record/tag union state.
- Match branch state.

The goal is not abstraction cleanup. The goal is predictable stores and loads:
append a fixed-size payload to a fixed stack, then pop it at the one lexical
continuation that consumes it.

Audit requirements:

- Compare generated code for push/pop before and after.
- Confirm payload copies are smaller or removed.
- Confirm no new parent dispatch or generic helper call appears.

Reject if:

- The source gets duplicated without a measurable codegen or benchmark win.
- Dedicated stacks increase cache footprint more than they reduce copies.

## 8. More Tape-Like Scratch Writes

Keep the final AST rich, but make child accumulation more simdjson-like. While
a collection is open, append compact child ids to scratch arrays. Only commit
the rich AST node once the closing token is consumed or recovery decides the
node is malformed.

Roc already does this in several places. The experiment is to make it more
consistent and reduce inline rich-node construction inside child loops.

Targets:

- Expression lists, tuples, and function application args.
- Pattern lists, tuples, tag args, and record fields.
- Type application args, tuple args, record fields, and tag union payloads.
- Match branches and block statements.

The best outcome is that inner loops mostly append ids and advance tokens, then
one close path performs the rich commit.

Audit requirements:

- Compare store count and call count in inner collection loops.
- Ensure scratch spans are cleared exactly on recovery paths.
- Run snapshot tests and parse coverage tests.

Reject if:

- It delays diagnostics in a way that worsens error regions.
- It requires later compiler stages to infer anything from scratch shape.
- It makes memory lifetime less explicit.

## 9. Token Buffer Side Tables for Hot Metadata

Instead of deriving everything from `Token.Tag` during parsing, tokenization can
optionally write compact side metadata for hot fields:

- token class
- binary operator info index
- delimiter class
- identifier class if known cheaply

The parser would still consume the token buffer, and the AST would not change.
This moves some classification work from the parser hot loop into tokenization,
which may be profitable if the same token classifications are queried multiple
times.

This is closer to simdjson's split between stage 1 and stage 2: stage 1
precomputes structural positions so stage 2 can do less discovery work.

Audit requirements:

- Measure tokenizer cost separately; do not hide parser wins by making
  tokenization slower overall.
- Confirm side metadata improves repeated parser classifications.
- Keep metadata compact enough not to hurt token-buffer cache locality.

Reject if:

- Tokenization cost increases more than parser cost decreases.
- The side table duplicates information used only once.
- It complicates incremental parsing or diagnostics.

## 10. Delimiter-Class Encoding

Many parser paths ask whether the current token closes the current construct.
Instead of checking exact token tags repeatedly, encode open and close delimiter
classes compactly.

Example classes:

```zig
const DelimClass = enum(u8) {
    none,
    round,
    square,
    curly,
    string_interpolation,
};
```

Then collection state can store an expected close class, and current-token
classification can be a table or range check. This helps list/tuple/record/type
collection code share a cheap close check without a generic parser-state loop.

Audit requirements:

- Confirm close checks in collection loops become load/compare or range tests.
- Confirm error recovery still reports the specific expected token.
- Verify no central delimiter-dispatch switch appears.

Reject if:

- Specific diagnostics require reintroducing exact-token branches on the hot
  path.
- Close-token classification is not hot enough to matter.

## Suggested Experiment Order

Start with the experiments that attack known remaining costs while preserving
the current architecture:

1. Operator metadata encoding.
2. Token discriminant ordering for hot ranges.
3. Static token classification tables where ordering is insufficient.
4. Scratch and parent stack pre-reservation to reduce allocator `blr x8` calls.
5. Hot/cold splitting for diagnostics and recovery.
6. More tape-like scratch writes in one collection family.

Each experiment should be a small, committed checkpoint with:

- source change
- focused parser checks
- ReleaseFast disassembly note
- benchmark result

Do not stack several of these before measuring. The goal is to know which
specific change moved the generated code and benchmark numbers.
