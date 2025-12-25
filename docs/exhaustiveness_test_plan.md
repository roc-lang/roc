# Exhaustiveness Checking Test Plan

This document outlines tests to add for exhaustiveness and redundancy checking, based on the Rust implementation in `crates/compiler/load/tests/test_reporting.rs` and `crates/compiler/exhaustive/src/lib.rs`.

## Current Coverage

We have basic tests in `test/snapshots/formatting/*/everything.md` that cover non-exhaustive list patterns like `Z4([a, b])`.

## Tests to Add

### 1. Basic Tag Union Exhaustiveness

- [ ] **Single missing tag** - `when x is Red -> ...` missing `Green`
- [ ] **Multiple missing tags** - RemoteData pattern with NotAsked, Loading, Failure, Success
- [ ] **All tags covered** - should pass without error
- [ ] **Boolean exhaustiveness** - `when b is Bool.true -> ...` missing `Bool.false`

### 2. Nested Pattern Exhaustiveness

- [ ] **Nested tag not exhaustive** - `when Record Nothing 1 is Record (Just 3) b -> ...` missing `Record (Just _) _`
- [ ] **Nested Result types** - `Result (Result [A, B] {}) {}` with missing inner cases
- [ ] **Deeply nested patterns** - 3+ levels of nesting

### 3. Record Pattern Exhaustiveness

- [ ] **Record field guard not exhaustive** - `{ a: 4 }` doesn't cover all values of `a`
- [ ] **Nested record guards** - `{ a: Nothing }` and `{ a: Just 3 }` missing `{ a: Just _ }`
- [ ] **Record with tag field** - mixing record and tag exhaustiveness

### 4. Tuple Exhaustiveness

- [ ] **Missing tuple combination** - `(Red, Red)` when only covering 3 of 4 color combinations
- [ ] **Complete tuple coverage** - all combinations present, no error
- [ ] **Nested tuples** - `((A, B), C)` patterns

### 5. List Pattern Exhaustiveness

- [ ] **Only empty list** - `[]` missing `[_, ..]`
- [ ] **Fixed lengths without rest** - `[]`, `[A]`, `[A, A]` missing `[_, _, _, ..]`
- [ ] **Spread exhaustive** - `[..]` covers all
- [ ] **Spread with required front/back** - `[A, ..]`, `[.., A]`, `[..]` covering all
- [ ] **Empty and rest with unary head** - `[]`, `[_, ..]` is exhaustive
- [ ] **Rest patterns with element matching** - `[A, ..]` vs `[.., A]`

### 6. List Pattern Redundancy

- [ ] **Spread redundant front/back** - `[A, ..]` followed by `[.., A]` when list element is single-tag
- [ ] **Exact after spread** - `[..]` followed by `[A]` is redundant
- [ ] **Subsumption** - `[_, _]` followed by `[A, B]` is redundant

### 7. Literal Pattern Exhaustiveness

- [ ] **Integer literals non-exhaustive** - `when 0x1 is 2 -> ...` needs wildcard
- [ ] **String literals** - always need wildcard
- [ ] **Float literals** - always need wildcard
- [ ] **Byte/U8 literals** - always need wildcard

### 8. Redundant Pattern Detection

- [ ] **Duplicate integer literal** - `2 -> ...; 2 -> ...`
- [ ] **Second wildcard redundant** - `_ -> ...; _ -> ...`
- [ ] **Tag after wildcard** - `_ -> ...; Red -> ...` is redundant
- [ ] **Covered by previous pattern** - `Red -> ...; Red -> ...` with same tag
- [ ] **Subsumption by wildcard** - constructor pattern after `_`

### 9. Guard Patterns

- [ ] **Guard makes pattern non-exhaustive** - `A if Bool.true -> ...` needs `A` without guard
- [ ] **Guard with complete coverage** - `A if condition -> ...; A -> ...` is exhaustive
- [ ] **Multiple guards on same pattern** - proper handling

### 10. Opaque Type Patterns

- [ ] **Opaque wrapping tag not exhaustive** - `@F A`, `@F B` missing `@F C`
- [ ] **Opaque wrapping int not exhaustive** - `@F 1`, `@F 2` needs `@F _`
- [ ] **Opaque with wildcard** - `@F _` covers all

### 11. Uninhabited Types (Empty Tag Unions)

- [ ] **Trivially exhaustive** - `Result {} []` only needs `Ok {}` branch
- [ ] **Nested uninhabited** - `Result (Result [A, B] []) []` only needs Ok branches
- [ ] **Uninhabited Err branch redundant** - `Err _` on `Result {} []` is unmatchable
- [ ] **Nested uninhabited redundancy** - `Ok (Err _)` on nested Result with empty Err

### 12. Function Argument Patterns

- [ ] **Non-exhaustive function pattern** - `\Left v -> v` on `Either` type
- [ ] **Let binding not exhaustive** - `(Left y) = x` where x can be Right

### 13. Complex/Edge Cases

- [ ] **Flip-flop catch-all not exhaustive** - `A B _` and `A _ C` missing `A _ _`
- [ ] **Issue 2778 regression** - specialization is not a redundant pattern
- [ ] **Polymorphic type variables** - flex vars shouldn't mark wildcards redundant
- [ ] **Open tag unions** - extension variables
- [ ] **Recursive types** - like RBTree with Node/Empty

### 14. Error Message Quality

- [ ] **Multiple missing patterns listed** - show all missing, not just first
- [ ] **Nested pattern in error** - `Ok (Ok B)` not just `Ok _`
- [ ] **Readable tag names** - proper identifier formatting
- [ ] **Guard note in error** - "(note the lack of an if clause)"

### 15. Performance/Stress Tests

- [ ] **Many branches** - 50+ pattern branches
- [ ] **Wide union** - 20+ tag variants
- [ ] **Deep nesting** - 5+ levels of pattern nesting
- [ ] **Combination explosion** - multiple nested unions

## Implementation Priority

1. **High Priority** (core functionality):
   - Basic tag union exhaustiveness (#1)
   - Nested patterns (#2)
   - List patterns (#5, #6)
   - Redundancy detection (#8)

2. **Medium Priority** (common use cases):
   - Record patterns (#3)
   - Tuple patterns (#4)
   - Guard patterns (#9)
   - Literal patterns (#7)

3. **Lower Priority** (edge cases):
   - Opaque types (#10)
   - Uninhabited types (#11)
   - Function arguments (#12)
   - Complex edge cases (#13)

## Test File Locations

Tests should be added to:
- `test/snapshots/check/exhaustive/` - new directory for exhaustiveness test snapshots
- `src/check/exhaustive.zig` - unit tests for the core algorithm
- Integration with existing `test/snapshots/formatting/*/` for error message formatting

## Notes

- The Rust implementation has ~30 distinct test cases for exhaustiveness/redundancy
- We should aim for at least this level of coverage, plus additional edge cases
- Focus on error message quality - users should understand what patterns are missing
- Consider adding property-based tests for the core algorithm
