# LSP Completion Issues - Investigation Results

**Investigation Date**: 2026-01-21  
**Issue**: Record field completion only shows subset of fields  
**Status**: Root cause identified, solution path clear, implementation needed

---

## 📋 Documentation Index

- **README.md** (this file) - Quick reference and navigation
- **visual-guide.md** - Diagrams and visual explanation of the issue
- **type-system-record-fields-issue.md** - Comprehensive analysis and findings
- **implementation-snippet.md** - Code snippets for implementing the fix
- **changes-made.md** - Complete list of changes made to codebase

**Start here**: Read this file, then `visual-guide.md`, then `implementation-snippet.md`

---

## Current Status (2026-01-21)

### ✅ Implemented Fixes
1. **Snapshot environment prioritization** - LSP now uses snapshot from last clean build
2. **Name-based lookup for incomplete code** - Skips CIR position-based analysis when using snapshots
3. **Always search top-level defs** - Finds definitions by name instead of relying on local binding indices

### ❌ Blocked Issue
**Record field completion only shows subset of fields**

Example:
```roc
my_record : { foo : Str, bar : I64 }  # Declares 2 fields
my_record = { foo: "hello", bar: 42 }

result = my_record.  # <-- Only suggests 'bar', not 'foo'
```

**Root Cause**: Type system only stores fields that were accessed due to row polymorphism, even when there's an explicit type annotation with all fields.

**Evidence**: 
- `record.fields.count = 1` (only 1 field stored in type variable)
- `def.annotation` AST shows `.record = { .fields = ... }` (both fields present in annotation)
- Only the LAST accessed field is available for completion

### 🔧 Solution Path

Extract fields from `def.annotation` (type annotation AST) instead of from inferred type variable:

```zig
if (def.annotation) |anno_idx| {
    const annotation = module_env.store.getAnnotation(anno_idx);
    const type_anno = module_env.store.getTypeAnno(annotation.anno);
    
    // TODO: Parse type_anno.record.fields to extract ALL field names
    // Need to study src/canonicalize/TypeAnnotation.zig API
}
```

See `type-system-record-fields-issue.md` for detailed analysis.

## Files Modified

### src/lsp/syntax.zig
- `getCompletionsAtPosition` (lines 2978-3022): Snapshot prioritization
- `addRecordFieldCompletions` (lines 3242-3330): Name-based def lookup
- Added debug print statements (remove after fix)

### src/lsp/test/syntax_test.zig  
- Tests now use `module []` instead of `app []`
- Tests create snapshots with `checker.check(uri, clean_contents, null)`

## Test Status

**Failing**: 3 tests in `zig build test-lsp`
- `test.record field completion works for modules`
- `test.record completion uses snapshot env when builds fail`
- `test.record field completion with partial field name`

**Issue**: All expect both `foo` and `bar` fields, but only get `bar`.

## Next Actions

1. Study `src/canonicalize/TypeAnnotation.zig` to understand field extraction
2. Implement `addFieldsFromTypeAnnotation` helper function
3. Use annotation AST when `def.annotation != null`
4. Remove debug print statements
5. Verify all tests pass

## Quick Build & Test

```bash
# Build compiler
zig build roc

# Run LSP tests
zig build test-lsp

# Watch specific test output
zig build test-lsp 2>&1 | grep -A 10 "record field completion"
```

## References

- Detailed analysis: `.llm/type-system-record-fields-issue.md`
- Original plan: `.llm/plans/completion-issues-analysis.md` (if exists)
- Type annotations: `src/canonicalize/TypeAnnotation.zig`
- Type system: `src/types/`
