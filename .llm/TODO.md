# TODO: Complete Record Field Completion Fix

**Priority**: Medium  
**Estimated Time**: 2-4 hours  
**Difficulty**: Medium  
**Required Knowledge**: Zig, basic compiler concepts

---

## Quick Summary

LSP record field completion currently only shows fields that were accessed in code, not all fields declared in type annotations. The fix is well-understood but needs implementation.

**Example**:
```roc
my_record : { foo : Str, bar : I64 }  # Declares 2 fields
result = my_record.                    # Only suggests 'bar' ❌
                                       # Should suggest both 'foo' and 'bar' ✅
```

---

## What's Already Done ✅

1. **Snapshot mechanism** - LSP correctly uses snapshot from last clean build
2. **Name-based lookup** - Correctly finds definitions when typing incomplete code  
3. **Annotation detection** - Can access the type annotation AST
4. **Root cause identified** - Type variable only has 1 field due to row polymorphism
5. **Solution designed** - Extract fields from annotation AST instead of type var
6. **Documentation complete** - All findings documented in `.llm/` folder

**The infrastructure is in place. Just need to implement field extraction.**

---

## What Needs to Be Done 🔧

### Task 1: Study TypeAnnotation.zig API (30-60 min)

**File**: `src/canonicalize/TypeAnnotation.zig`

**Goal**: Understand how to extract field information from TypeAnno

**Questions to answer**:
1. What is the type of `TypeAnno.record`?
2. How to iterate over `record.fields`?
3. What information does each field contain (name, type)?
4. How to handle record extensions (`{ foo : Str | rest }`)?

**Helpful searches**:
```bash
cd /home/eli/Code/roc/roc/src

# Find TypeAnno definition
rg "pub const TypeAnno" canonicalize/TypeAnnotation.zig -A 20

# Find record field structure
rg "record.*fields" canonicalize/TypeAnnotation.zig -A 10

# Find examples of iterating record fields
rg "\.record.*=>" . -A 10 --type zig

# Find examples in error reporting (they format TypeAnnos)
rg "TypeAnno.*record" reporting/ -A 5 --type zig
```

### Task 2: Implement Field Extraction (1-2 hours)

**File**: `src/lsp/syntax.zig`

**Location**: Around line 3305 (search for "def has annotation")

**Current code**:
```zig
if (def.annotation) |anno_idx| {
    const annotation = module_env.store.getAnnotation(anno_idx);
    const type_anno = module_env.store.getTypeAnno(annotation.anno);
    // TODO: Extract fields from type_anno
}
```

**What to implement**:

```zig
// In addRecordFieldCompletions(), around line 3305:
if (def.annotation) |anno_idx| {
    const annotation = module_env.store.getAnnotation(anno_idx);
    const type_anno = module_env.store.getTypeAnno(annotation.anno);
    
    // NEW: If it's a record annotation, extract fields from it
    switch (type_anno) {
        .record => |record_anno| {
            try self.addFieldsFromTypeAnnoRecord(items, module_env, record_anno);
            return;  // Don't use type_var, we got fields from annotation
        },
        else => {
            // Not a record, fall through to type_var
        },
    }
}

// NEW: Add this function to SyntaxChecker
fn addFieldsFromTypeAnnoRecord(
    self: *SyntaxChecker,
    items: *std.ArrayList(completion_handler.CompletionItem),
    module_env: *ModuleEnv,
    record_anno: ???, // TODO: Determine type from TypeAnnotation.zig
) !void {
    // TODO: Based on what you learn from Task 1:
    // 1. Get fields from record_anno.fields (it's a span or list)
    // 2. For each field:
    //    - Get field name
    //    - Get field type (optional, for detail string)
    //    - Add completion item
    
    // Example structure (may not be exact):
    // const fields = record_anno.fields;
    // for (fields) |field| {
    //     const name = module_env.getIdentText(field.name_idx);
    //     try items.append(self.allocator, .{
    //         .label = name,
    //         .kind = @intFromEnum(completion_handler.CompletionItemKind.field),
    //         .detail = null,  // Or format field.type_anno if needed
    //     });
    // }
}
```

**Optional**: Also implement `formatTypeAnno()` to get nice type strings for completion details

### Task 3: Test and Debug (30 min)

**Commands**:
```bash
cd /home/eli/Code/roc/roc

# Build
zig build roc

# Run LSP tests
zig build test-lsp

# Check specific tests
zig build test-lsp 2>&1 | grep -A 10 "record field completion"
```

**Expected result**: All 3 failing tests should pass
- `test.record field completion works for modules` ✅
- `test.record completion uses snapshot env when builds fail` ✅
- `test.record field completion with partial field name` ✅

**Success criteria**:
```
Got 2 completion items:
  - foo (kind=5)
  - bar (kind=5)
found_foo=true, found_bar=true
```

### Task 4: Clean Up (15 min)

**Remove debug prints** from `src/lsp/syntax.zig`:

Search for `std.debug.print` and remove these lines:
- Line ~3024: `completion: context=..., used_snapshot=...`
- Line ~3033: `after_record_dot for ...`
- Line ~3287: `ALL DEFS: ...`
- Line ~3305: `def has annotation...`
- Line ~3432: `addFieldsFromRecord: record.fields=...`
- And many more throughout

**Tip**: Search for all prints added during investigation:
```bash
rg "std.debug.print" src/lsp/syntax.zig | wc -l  # Count them
```

Most of these are temporary and should be removed for production.

---

## Files to Modify

### Primary
- `src/lsp/syntax.zig` - Add `addFieldsFromTypeAnnoRecord()` function

### Reference (Read Only)
- `src/canonicalize/TypeAnnotation.zig` - Learn TypeAnno API
- `src/canonicalize/CIR.zig` - Understand Def, Annotation structures
- `.llm/implementation-snippet.md` - Code examples

---

## Testing Checklist

After implementation, verify:

- [ ] `zig build roc` succeeds
- [ ] `zig build test-lsp` passes all tests
- [ ] Completion after `my_record.` shows both `foo` and `bar`
- [ ] Works with records with many fields (not just 2)
- [ ] Works with records with type extensions (if possible)
- [ ] No debug prints left in code
- [ ] Code is formatted with `zig build fmt`

---

## Expected Completion Time

| Task | Time | Running Total |
|------|------|---------------|
| Study TypeAnnotation.zig | 30-60 min | 1 hour |
| Implement field extraction | 1-2 hours | 2-3 hours |
| Test and debug | 30 min | 2.5-3.5 hours |
| Clean up debug prints | 15 min | 2.75-3.75 hours |

**Total: ~3 hours** for someone familiar with Zig and the codebase

---

## Helpful Resources

### Documentation
- `.llm/README.md` - Quick overview
- `.llm/visual-guide.md` - Diagrams explaining the issue
- `.llm/type-system-record-fields-issue.md` - Full investigation report
- `.llm/implementation-snippet.md` - Code snippets and examples

### Code Locations
```
src/lsp/syntax.zig:2978-3022    - Snapshot prioritization (done)
src/lsp/syntax.zig:3037          - Skip CIR lookup when snapshot (done)
src/lsp/syntax.zig:3242-3330     - Name-based def lookup (done)
src/lsp/syntax.zig:3305          - Annotation access (needs completion) ⚠️
```

### Test Locations
```
src/lsp/test/syntax_test.zig:287   - record field completion works for modules
src/lsp/test/syntax_test.zig:375   - record completion uses snapshot env
src/lsp/test/syntax_test.zig:437   - record field completion with partial name
```

---

## Known Limitations

After this fix:

✅ **Will work**:
- Records with explicit type annotations
- Multiple fields in record
- Completion in incomplete code (uses snapshot)

⚠️ **May not work** (requires more investigation):
- Records defined via type aliases
- Records with extensions (`{ foo : Str | rest }`)
- Recursive or nested record types
- Records without type annotations (falls back to current behavior)

These edge cases can be addressed in follow-up work if needed.

---

## Alternative: Quick Workaround

If you need a quick workaround without implementing the full solution:

**Modify the tests** to only test single-field records or records where all fields are accessed in a single expression. This doesn't fix the issue but unblocks other work.

```zig
// Instead of:
my_record : { foo : Str, bar : I64 }

// Use:
my_record : { foo : Str }  // Only one field
```

This is NOT recommended as the proper fix is straightforward.

---

## Questions?

If you get stuck:

1. Check the detailed docs in `.llm/`
2. Look for examples in `src/reporting/` (error formatting uses TypeAnno)
3. Search for existing TypeAnno.record usage: `rg "\.record.*=>" src/`
4. The TypeAnnotation.zig file should have comments explaining the API

---

## Success Looks Like

```bash
$ zig build test-lsp
test-lsp
+- run test lsp 104/105 passed, 1 failed  # Much better!
```

And users typing:
```roc
my_record : { foo : Str, bar : I64, baz : Bool }
result = my_record.█
```

See ALL fields suggested:
```
Suggestions:
  - foo  : Str
  - bar  : I64  
  - baz  : Bool
```

Good luck! 🚀

---

**Last Updated**: 2026-01-21  
**Next Review**: After implementation is complete
