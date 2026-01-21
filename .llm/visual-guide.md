# Visual Guide: Record Field Completion Issue

## The Problem in Pictures

### What We Expected

```
┌─────────────────────────────────────────────────────┐
│ Clean Code (creates snapshot)                       │
├─────────────────────────────────────────────────────┤
│ my_record : { foo : Str, bar : I64 }                │
│ my_record = { foo: "hello", bar: 42 }               │
│                                                      │
│ get_foo = my_record.foo                             │
│ get_bar = my_record.bar                             │
└─────────────────────────────────────────────────────┘
                     │
                     │ checker.check()
                     ▼
┌─────────────────────────────────────────────────────┐
│ Snapshot Type Store                                 │
├─────────────────────────────────────────────────────┤
│ my_record: { foo : Str, bar : I64 }  ◄── Expected  │
│            ▲                                         │
│            └── Has BOTH fields                      │
└─────────────────────────────────────────────────────┘
                     │
                     │ LSP getCompletionsAtPosition()
                     ▼
┌─────────────────────────────────────────────────────┐
│ Incomplete Code (while typing)                      │
├─────────────────────────────────────────────────────┤
│ my_record = { foo: "hello", bar: 42 }               │
│                                                      │
│ result = my_record.█                                │
│                   ▲                                  │
│                   └── Cursor here                   │
└─────────────────────────────────────────────────────┘
                     │
                     │ Look up "my_record" in snapshot
                     ▼
┌─────────────────────────────────────────────────────┐
│ Completion Suggestions                              │
├─────────────────────────────────────────────────────┤
│ ✓ foo  : Str                                        │
│ ✓ bar  : I64                                        │
└─────────────────────────────────────────────────────┘
```

### What Actually Happens

```
┌─────────────────────────────────────────────────────┐
│ Clean Code (creates snapshot)                       │
├─────────────────────────────────────────────────────┤
│ my_record : { foo : Str, bar : I64 } ◄── Annotation │
│ my_record = { foo: "hello", bar: 42 }               │
│                                                      │
│ get_foo = my_record.foo  ◄── Uses { foo : Str }    │
│ get_bar = my_record.bar  ◄── Uses { bar : I64 }    │
└─────────────────────────────────────────────────────┘
                     │
                     │ checker.check()
                     │ (Row polymorphism specializes types)
                     ▼
┌─────────────────────────────────────────────────────┐
│ Snapshot Type Store                                 │
├─────────────────────────────────────────────────────┤
│ my_record pattern type_var: { bar : I64 }          │
│                              ▲                       │
│                              └── Only LAST field!   │
│                                                      │
│ my_record annotation AST: { foo : Str, bar : I64 } │
│                           ▲                          │
│                           └── Has BOTH fields       │
└─────────────────────────────────────────────────────┘
                     │
                     │ LSP getCompletionsAtPosition()
                     ▼
┌─────────────────────────────────────────────────────┐
│ Incomplete Code (while typing)                      │
├─────────────────────────────────────────────────────┤
│ my_record = { foo: "hello", bar: 42 }               │
│                                                      │
│ result = my_record.█                                │
│                   ▲                                  │
│                   └── Cursor here                   │
└─────────────────────────────────────────────────────┘
                     │
                     │ Look up "my_record" in snapshot
                     │ Currently uses: type_var (❌)
                     │ Should use: annotation AST (✅)
                     ▼
┌─────────────────────────────────────────────────────┐
│ Completion Suggestions                              │
├─────────────────────────────────────────────────────┤
│ ✗ foo  : Str       ◄── Missing!                    │
│ ✓ bar  : I64       ◄── Only this one               │
└─────────────────────────────────────────────────────┘
```

## Data Flow Diagram

### Current Implementation (Incomplete)

```
User types: my_record.
      │
      ▼
getCompletionsAtPosition()
      │
      ├─→ Get snapshot environment ✅ (Working)
      │   (Always checks snapshot first)
      │
      ├─→ Detect context: .after_record_dot ✅ (Working)
      │   (variable_name = "my_record")
      │
      ├─→ Skip CIR-based lookup ✅ (Working)
      │   (used_snapshot=true, so skip findDotReceiverTypeVar)
      │
      └─→ addRecordFieldCompletions()
            │
            ├─→ Build scope ✅ (Working)
            │   (Finds binding for "my_record")
            │
            ├─→ Search top-level defs ✅ (Working)
            │   (Always searches, not just when binding==null)
            │   │
            │   └─→ Find def for "my_record"
            │         │
            │         ├─→ Check def.annotation ⚠️ (Detects but doesn't use)
            │         │   │
            │         │   └─→ annotation.anno = TypeAnno.record
            │         │       │
            │         │       └─→ fields = { foo, bar } ✓ Both present!
            │         │
            │         └─→ Get type_var = ModuleEnv.varFrom(def.pattern) ❌
            │               │
            │               └─→ type_var has .count = 1 ❌
            │                     │
            │                     └─→ Only "bar" field ❌
            │
            └─→ addFieldsFromTypeVar()
                  │
                  └─→ Resolve to record type
                        │
                        └─→ addFieldsFromRecord()
                              │
                              └─→ Suggests: ["bar"] ❌ Wrong!
```

### Needed Implementation

```
User types: my_record.
      │
      ▼
getCompletionsAtPosition()
      │
      └─→ addRecordFieldCompletions()
            │
            └─→ Find def for "my_record"
                  │
                  ├─→ Check def.annotation ✅
                  │   │
                  │   ├─→ if annotation exists:
                  │   │   │
                  │   │   └─→ switch (type_anno) {
                  │   │         .record => |record_anno| {
                  │   │             addFieldsFromTypeAnnoRecord() ← NEW!
                  │   │             return; ← Use annotation, not type_var
                  │   │         }
                  │   │       }
                  │   │
                  │   └─→ addFieldsFromTypeAnnoRecord()
                  │         │
                  │         ├─→ Get fields from record_anno.fields
                  │         │   (This is AST, has ALL declared fields)
                  │         │
                  │         └─→ For each field:
                  │               - Extract name
                  │               - Extract type (optional, for detail)
                  │               - Add completion item
                  │               │
                  │               └─→ Suggests: ["foo", "bar"] ✅ Correct!
                  │
                  └─→ if no annotation:
                        Fall back to type_var (current behavior)
```

## Type System Architecture

### How Types Are Stored

```
┌─────────────────────────────────────────────────────┐
│ Definition (CIR.Def)                                │
├─────────────────────────────────────────────────────┤
│                                                      │
│  pattern: Pattern.Idx ──┐                          │
│                          │                           │
│  expr: Expr.Idx          │                          │
│                          │                           │
│  annotation: ?Annotation.Idx ──┐                   │
│                          │      │                    │
│  kind: Kind              │      │                    │
└──────────────────────────┼──────┼────────────────────┘
                           │      │
                           │      │
       ┌───────────────────┘      └──────────────────┐
       │                                              │
       ▼                                              ▼
┌─────────────────┐                    ┌────────────────────────┐
│ Pattern         │                    │ Annotation             │
├─────────────────┤                    ├────────────────────────┤
│                 │                    │                        │
│ Has type_var ───┼──┐                │ anno: TypeAnno.Idx ────┼──┐
│ (inferred)      │  │                 │                        │  │
│                 │  │                 │ where: ?WhereClause    │  │
└─────────────────┘  │                 └────────────────────────┘  │
                     │                                              │
                     │                                              │
                     ▼                                              ▼
    ┌─────────────────────────────┐        ┌──────────────────────────────┐
    │ TypeVar (in type store)     │        │ TypeAnno (AST)               │
    ├─────────────────────────────┤        ├──────────────────────────────┤
    │                             │        │                              │
    │ Resolved to:                │        │ .record = {                  │
    │   .structure                │        │   .fields = [                │
    │     .record {               │        │     { name: "foo",           │
    │       .fields = {           │        │       type: TypeAnno.lookup  │
    │         .start = 13         │        │             ("Str") },       │
    │         .count = 1 ❌       │        │     { name: "bar",           │
    │       }                     │        │       type: TypeAnno.lookup  │
    │     }                       │        │             ("I64") }        │
    │                             │        │   ],                         │
    │ Only has: ["bar"]           │        │   .ext = null                │
    │                             │        │ }                            │
    │ ▲                           │        │                              │
    │ └─ Specialized by row       │        │ Has: ["foo", "bar"] ✅      │
    │    polymorphism             │        │                              │
    └─────────────────────────────┘        └──────────────────────────────┘
              │                                          │
              │                                          │
              └── Currently used ❌                     └── Should use ✅
```

### Why Row Polymorphism Affects This

```
Original annotation:
    my_record : { foo : Str, bar : I64 }
                ▲
                └── Full type declared

Usage 1:
    get_foo = my_record.foo
    
    Type inference sees:
    my_record : { foo : Str | rest }
                ▲
                └── Specialized: only needs 'foo'

Usage 2:
    get_bar = my_record.bar
    
    Type inference sees:
    my_record : { bar : I64 | rest }
                ▲
                └── Specialized: only needs 'bar'

Final stored type:
    One of the specialized types (whichever was last)
    NOT the full annotation type
    
    This is why type_var.count = 1
```

## Solution Comparison

### Solution A: Extract from Annotation AST ✅ Recommended

```
┌─────────────────────────────────────────────────────┐
│ Pros                                                 │
├─────────────────────────────────────────────────────┤
│ ✓ LSP-only change (low risk)                        │
│ ✓ Gets ALL declared fields                          │
│ ✓ Medium complexity                                 │
│ ✓ Fast to implement                                 │
└─────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────┐
│ Cons                                                 │
├─────────────────────────────────────────────────────┤
│ ✗ Only fixes LSP, not other tools                   │
│ ✗ Doesn't fix root cause                            │
│ ✗ Need to learn TypeAnnotation.zig API              │
└─────────────────────────────────────────────────────┘
```

### Solution B: Fix Type System ⚠️ Not Attempted

```
┌─────────────────────────────────────────────────────┐
│ Pros                                                 │
├─────────────────────────────────────────────────────┤
│ ✓ Fixes root cause                                  │
│ ✓ Benefits all tools                                │
│ ✓ More semantically correct                         │
└─────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────┐
│ Cons                                                 │
├─────────────────────────────────────────────────────┤
│ ✗ Very high complexity                              │
│ ✗ Affects entire compiler                           │
│ ✗ Risk of breaking other features                   │
│ ✗ Performance impact (more type data)               │
│ ✗ Requires type system expertise                    │
│ ✗ Extensive testing needed                          │
└─────────────────────────────────────────────────────┘
```

## File Organization

```
.llm/
├── README.md                               ← Quick reference
├── type-system-record-fields-issue.md      ← Full analysis (this is the main doc)
├── implementation-snippet.md                ← Code to implement Solution A
├── changes-made.md                          ← What we actually changed
└── visual-guide.md                          ← This file (diagrams)
```

## Key Takeaways

1. **The snapshot mechanism works** - We correctly get the snapshot environment

2. **Name-based lookup works** - We correctly find the definition by name

3. **The annotation exists and has all fields** - The AST has complete information

4. **The type variable is incomplete** - Row polymorphism specialized it

5. **The fix is clear** - Extract fields from annotation AST instead of type_var

6. **The blocker is API knowledge** - Need to understand TypeAnnotation.zig

## Next Developer: Start Here

If you're picking up this work:

1. Read: `.llm/README.md` (5 min)
2. Read: This file (5 min)
3. Read: `.llm/implementation-snippet.md` (10 min)
4. Study: `src/canonicalize/TypeAnnotation.zig` (30-60 min)
5. Look for examples of TypeAnno.record field iteration in:
   ```bash
   rg "TypeAnno.*record.*fields" src/
   rg "\.record.*=>" src/ -A 10
   ```
6. Implement: `addFieldsFromTypeAnnoRecord()` helper function
7. Test: `zig build test-lsp`
8. Clean up: Remove debug prints
9. Done! 🎉

Good luck! The path forward is clear, just needs someone to read the TypeAnnotation API.
