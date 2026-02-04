# Codebase Concerns

**Analysis Date:** 2026-01-20

## Tech Debt

**Legacy Rust Compiler:**
- Issue: Two compiler implementations exist - Zig (`src/`) and Rust (`crates/`)
- Files: `crates/` directory (repository root)
- Why: Rust compiler is legacy prototype, Zig is active development
- Impact: Confusion for contributors, maintenance overhead
- Fix approach: Rust compiler is read-only, clearly documented in `CLAUDE.md`

**Very Large Files:**
- Issue: Several files exceed 10,000 lines, making review and refactoring difficult
- Files:
  - `src/eval/interpreter.zig` (~20,000 lines)
  - `src/backend/llvm/Builder.zig` (~15,000 lines)
  - `src/canonicalize/Can.zig` (~12,700 lines)
  - `src/cli/main.zig` (~6,500 lines)
  - `src/check/Check.zig` (~6,200 lines)
- Impact: Slow compilation, hard to understand, high cognitive overhead
- Fix approach: Decompose into smaller modules with clear responsibilities

**Panic-based Error Handling:**
- Issue: Many `@panic` calls used for "not implemented" or OOM instead of error propagation
- Files:
  - `src/backend/dev/mod.zig` - "No free general registers - spilling not implemented"
  - `src/build/modules.zig` - `catch @panic("OOM")`
  - `src/llvm_compile/Builder.zig` - "TODO: query data layout"
  - `src/layout/store.zig` - "Cycle detected in layout computation"
- Impact: Crashes instead of user-friendly errors
- Fix approach: Convert to error returns, add feature gates for unimplemented paths

## Known Bugs

**Race Condition in Subscription Updates (from TODO):**
- Symptoms: Incomplete cross-module function call handling
- Trigger: Cross-module lookups in REPL
- Files: `src/repl/repl_test.zig`
- Workaround: None documented
- Root cause: `e_lookup_external` implementation incomplete
- TODO text: "Fix e_lookup_external implementation to support cross-module function calls"

**Skipped Tests Due to Segfaults:**
- Symptoms: Some comptime eval tests cause segfaults
- Trigger: Running specific comptime evaluation scenarios
- Files: `src/eval/test/comptime_eval_test.zig`
- Workaround: Tests skipped
- Impact: Unknown memory safety or invariant issues

## Security Considerations

**Bundle/Unbundle Path Handling:**
- Risk: Package extraction could be vulnerable to path traversal
- Files: `src/unbundle/unbundle.zig`, `src/unbundle/download.zig`, `src/bundle/bundle.zig`
- Current mitigation: Hash validation present
- Recommendations: Add explicit path traversal tests, symlink handling verification

**URL-based Package Fetching:**
- Risk: Remote package fetching could download malicious content
- Files: `src/cli/main.zig` (handles `http`/`https` platform specs)
- Current mitigation: Hash verification for downloads
- Recommendations: Verify TLS, add timeout handling, document trust model

## Performance Bottlenecks

**Large File Compilation:**
- Problem: Very large modules slow incremental compilation
- Files: `src/eval/interpreter.zig`, `src/canonicalize/Can.zig`, `src/backend/llvm/Builder.zig`
- Measurement: Not measured (anecdotally slow)
- Cause: Zig compiler processes entire file even for small changes
- Improvement path: Split into smaller files to enable better incremental compilation

**Hardcoded Tokenization Parameters:**
- Problem: Tab width hardcoded, affects parse reproducibility
- Files: `src/parse/tokenize.zig` - `tab_width: u8 = 4`
- Cause: Configuration not exposed
- Improvement path: Make configurable via CLI/LSP options

## Fragile Areas

**Cross-Module Import Resolution:**
- Files: `src/canonicalize/Can.zig`, `src/canonicalize/ModuleEnv.zig`
- Why fragile: Many `.orelse unreachable` expects builtin names; cross-module resolution has TODOs
- Common failures: Unknown when builtin names change
- Safe modification: Add tests for builtin auto-imports, document invariants
- Test coverage: Limited cross-module tests

**Parser Parentheses/Arrow Handling:**
- Files: `src/parse/Parser.zig`
- Why fragile: TODO comments indicate incomplete handling
- TODOs: "Parenthesized expressions", "Handle thin vs fat arrow"
- Safe modification: Add snapshot tests before changes
- Test coverage: Snapshots exist but edge cases missing

**Type Checker Mutually Recursive Functions:**
- Files: `src/check/Check.zig`
- Why fragile: TODO indicates incomplete handling
- TODO text: "Handle mutually recursive functions"
- Safe modification: Add regression tests
- Test coverage: Basic recursion tested, mutual recursion gaps

## Scaling Limits

**Memory Usage:**
- Current capacity: Not measured
- Limit: Large projects may exhaust memory (arena allocators)
- Symptoms at limit: OOM panics
- Scaling path: Profile memory, add limits, consider streaming for large files

## Dependencies at Risk

**Zig 0.15.x (Development Version):**
- Risk: Using development Zig version, API may change
- Impact: Build may break on Zig updates
- Migration plan: Track Zig releases, update build.zig as needed

**LLVM:**
- Risk: LLVM API changes between versions
- Impact: Backend may need updates
- Migration plan: Pin LLVM version, test with CI on multiple versions

## Missing Critical Features

**Register Spilling (Backend):**
- Problem: "No free general registers - spilling not implemented"
- Files: `src/backend/dev/mod.zig`
- Current workaround: Compilation fails (panic)
- Blocks: Complex functions with many variables
- Implementation complexity: Medium-High

**Target Data Layout (LLVM):**
- Problem: "TODO: query data layout" / "TODO: implement targetLayoutType"
- Files: `src/llvm_compile/Builder.zig`
- Current workaround: Panic on affected paths
- Blocks: Cross-compilation, some target architectures
- Implementation complexity: Medium

**Cross-Module Calls in REPL:**
- Problem: External module lookups incomplete
- Files: `src/repl/repl_test.zig`
- Current workaround: Feature doesn't work
- Blocks: Multi-module REPL workflows
- Implementation complexity: Medium

## Test Coverage Gaps

**Backend/Codegen Unit Tests:**
- What's not tested: Fine-grained LLVM builder and native backend logic
- Files: `src/backend/llvm/Builder.zig`, `src/backend/dev/`
- Risk: Codegen bugs go unnoticed until runtime
- Priority: High
- Difficulty: Need to emit and verify generated code

**Cross-Module Integration:**
- What's not tested: Full cross-module compilation and linking
- Files: `src/compile/`, `src/check/test/cross_module_test.zig`
- Risk: Module boundary issues
- Priority: Medium
- Difficulty: Test infrastructure exists, need more scenarios

**Error Recovery:**
- What's not tested: Graceful handling of malformed input
- Risk: Crashes instead of helpful errors
- Priority: Medium
- Difficulty: Add fuzz testing or error injection

## Documentation Gaps

**Backend README:**
- Issue: `src/backend/llvm/` lacks comprehensive README
- Impact: Hard for contributors to understand codegen
- Fix: Add README documenting architecture, LLVM version requirements, how to add targets

**Inline TODO Documentation:**
- Issue: Many TODOs without linked issues or context
- Files: Throughout codebase (see Tech Debt section)
- Impact: Hard to prioritize or understand scope
- Fix: Convert high-impact TODOs to tracked issues with reproduction steps

## Prioritized Next Steps

**High Priority:**
1. Convert panic-based OOM handling to error propagation
2. Fix or gate register spilling panic in backend
3. Triage skipped/segfaulting tests
4. Add tests for cross-module function calls

**Medium Priority:**
1. Split large files (interpreter, canonicalizer, builder)
2. Add backend unit tests
3. Document LLVM integration in README
4. Make tokenization tab_width configurable

**Low Priority:**
1. Consolidate duplicated panic messages
2. Add path traversal tests to bundle/unbundle
3. Profile and document memory usage patterns

---

*Concerns audit: 2026-01-20*
*Update as issues are fixed or new ones discovered*
