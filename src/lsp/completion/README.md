# LSP Completion System

This document describes how code completion works in the Roc LSP, covering the
request flow, context detection, completion types, key data structures, and
testing.

## File Map

All paths are relative to `src/lsp/`.

| File | Role |
|---|---|
| `handlers/completion.zig` | LSP protocol handler. Parses the JSON-RPC `textDocument/completion` request, extracts URI/line/character, calls into `SyntaxChecker`, and serialises the response. Defines `CompletionItem`, `CompletionItemKind`, and `CompletionResult`. |
| `syntax.zig` | **Orchestrator.** `SyntaxChecker.getCompletionsAtPosition()` is the main entry point. Builds the file, obtains a `ModuleEnv`, detects the completion context, creates a `CompletionBuilder`, and dispatches to the context-specific branch. Also contains helper resolution functions (`resolveModuleAlias`, `resolveLocalBindingTypeVar`, `resolveAccessChainTypeVar`, `extractReturnType`). |
| `completion/context.zig` | Pure text analysis. `detectCompletionContext()` scans backwards from the cursor to decide which `CompletionContext` variant applies. Also provides `computeOffset()` for converting LSP line/character to a byte offset. |
| `completion/builder.zig` | `CompletionBuilder` struct. Accumulates `CompletionItem`s with deduplication (via `seen_labels` hash map). All the `add*` methods live here. |
| `completion/builtins.zig` | `BUILTIN_TYPES` list (21 entries: `Str`, `List`, `Dict`, …, `Num`) and `isBuiltinType()` helper. |
| `completion/mod.zig` | Re-exports `CompletionContext`, `detectCompletionContext`, `computeOffset`, and `CompletionBuilder`. |
| `scope_map.zig` | Reconstructs lexical scopes from CIR so the builder can answer "which local variables are visible at byte offset X?" |
| `cir_queries.zig` | Offset-based CIR queries. `findDotReceiverTypeVar()` finds the type of the expression to the left of a dot. `findTypeAtOffset()` returns the type at an arbitrary offset. Used as a first-pass type resolver before falling back to name-based lookup. |
| `module_lookup.zig` | `findDefinitionByName()` — searches a `ModuleEnv`'s defs and statements for a name and returns its pattern index. Used when resolving access chains. |
| `build_env_handle.zig` | Reference-counted wrapper around `BuildEnv`. Ensures a `BuildEnv` stays alive while snapshot or previous-build references exist. |
| `build_session.zig` | `BuildSession` — manages a single build invocation (used at the top of `getCompletionsAtPosition`). |

### Supporting compiler modules (outside `src/lsp/`)

| Module | Purpose in completions |
|---|---|
| `canonicalize/ModuleEnv.zig` | Per-module compilation state: CIR node store, type store, method idents, ident table, etc. |
| `compile/BuildEnv` | Root compilation environment. Contains `schedulers` (one per package/job) each holding a list of `ModuleState`. Also holds `builtin_modules`. |
| `types/` | Type inference results: `Var`, `Content`, `TypeStore`, records, aliases, nominals, functions, static dispatch constraints. |
| `can/` (CIR) | Canonical Intermediate Representation — `Statement`, `Def`, `Pattern`, `Expr`, `TypeAnno`. |

---

## Request Flow

```
Editor sends textDocument/completion
        │
        ▼
handlers/completion.zig  handler().call()
  ├─ parse JSON params → (uri, line, character)
  ├─ look up document text from doc_store
  └─ call syntax_checker.getCompletionsAtPosition(uri, text, line, character)
        │
        ▼
syntax.zig  SyntaxChecker.getCompletionsAtPosition()
  1. Lock mutex (completions are single-threaded)
  2. createFreshBuildEnv() → BuildEnv
  3. BuildSession.init() → builds the file
  4. Obtain a ModuleEnv (see "Module Env Priority" below)
  5. detectCompletionContext(source, line, character) → CompletionContext
  6. computeOffset(source, line, character) → cursor_offset
  7. Create CompletionBuilder
  8. switch (context) → call the appropriate builder methods
  9. Return CompletionResult { items, is_incomplete }
        │
        ▼
handlers/completion.zig
  └─ serialise CompletionResult as JSON-RPC response
```

### Module Env Priority

The system tries three sources for a `ModuleEnv`, in order:

1. **Snapshot env** — cached from the last successful build of this file.
   Best for completions because the user is usually mid-edit and the current
   build may fail.
2. **Previous build env** — the last globally-successful build. Fallback when
   no snapshot exists for this file.
3. **Current build** — used only when the build just succeeded with no reports.

When a snapshot or previous env is used, the flag `used_snapshot` is set to
`true`.  This matters because CIR byte offsets from a snapshot do not match the
current source text, so CIR-based type lookups (`findDotReceiverTypeVar`) are
skipped in favour of name-based lookups.

The `module_lookup_env` variable tracks which `BuildEnv` backs the chosen
`ModuleEnv`, keeping module member lookups consistent.

---

## Completion Context Detection

`completion/context.zig :: detectCompletionContext(source, line, character)`

The function converts line/character to a byte offset, then scans backward:

1. Skip any partial identifier the user is currently typing.
2. Skip whitespace.
3. Inspect the character immediately before the cursor:

| Character | Result | Example |
|---|---|---|
| `.` preceded by an **uppercase** ident (no dots in chain) | `after_module_dot` | `Str.` → module name `"Str"` |
| `.` preceded by a **lowercase** ident (or any chain with dots) | `after_value_dot` | `myRecord.`, `myrec.subrec.` |
| `.` preceded by `)` (function call result) | `after_receiver_dot` | `getValue().` |
| `:` | `after_colon` | `x : ` |
| anything else | `expression` | `x = `, beginning of line |

### CompletionContext union

```zig
pub const CompletionContext = union(enum) {
    after_module_dot: []const u8,              // the module name
    after_value_dot: struct {
        access_chain: []const u8,              // e.g. "myrec.subrec"
        chain_start: u32,                      // byte offset of chain start
        member_start: u32,                     // byte offset of segment before dot
        dot_offset: u32,                       // byte offset of the '.'
    },
    after_receiver_dot: struct {
        dot_offset: u32,
        call_chain: ?[]const u8,               // e.g. "Record2.SubVal" or "testFunc"
        chain_start: u32,
    },
    after_colon,
    expression,
};
```

---

## Completion Types

Each completion context triggers a different set of builder methods. Here is
every kind of completion the system can produce, grouped by context.

### 1. Module Member Completions — `after_module_dot`

**Trigger:** `Str.`, `List.`, `Json.`, or `MyNominalType.`

**What happens:**
1. `resolveModuleAlias(module_env, name)` — if the module was imported with
   `import Json as J`, this maps `J` back to `Json`.
2. `builder.addModuleMemberCompletions(env, resolved_name, module_env_opt)` —
   walks all `exposed_items` from the target module's `ModuleEnv`. For builtin
   type names (recognised via `isBuiltinType`), it uses the special
   `builtin_module.env`. Items are classified as `function` (lowercase) or
   `class` (uppercase).
3. `builder.addTagCompletionsForNominalType(module_env, name, null)` — if the
   name is also a nominal type in the current module, adds its tag constructors
   (e.g., `Color.Red`, `Color.Green`).

**CompletionItemKind:** `function`, `class`, `enum_member`

### 2. Record Field & Method Completions — `after_value_dot`

**Trigger:** `myRecord.`, `myrec.subrec.`

**What happens (simplified):**
1. Try `resolveAccessChainTypeVar()` to resolve dotted chains like
   `Module.value.subfield` by walking segment-by-segment through the type
   system.
2. If that fails (common with snapshots), fall back:
   - CIR-based: `cir_queries.findDotReceiverTypeVar()` or `findTypeAtOffset()`
   - Name-based: `addRecordFieldCompletions(name, offset)` +
     `addMethodCompletions(name, offset)`
3. Once a `types.Var` is in hand:
   - `addFieldsFromTypeVar()` — unwraps aliases/nominals up to 8 levels, then
     iterates record fields. Each field gets a `detail` string showing its type.
   - `addMethodsFromTypeVar()` — looks up the type's identity (alias ident or
     nominal ident), then searches `method_idents` for matching
     `(type_ident, method_ident)` pairs. Builtin types are routed through the
     `builtin_module_env` so `Str.contains`, `List.map` etc. resolve correctly.

**CompletionItemKind:** `field`, `method`

### 3. Receiver Dot Completions (call chaining) — `after_receiver_dot`

**Trigger:** `getValue().`, `Str.join(items).`, `testFunc("hi").`

**What happens:**
1. Try CIR-based lookup: `findDotReceiverTypeVar()` at `cursor_offset`, with
   fallback to `findTypeAtOffset(dot_offset - 1)`.
2. If using a snapshot or CIR failed, parse the `call_chain` textually and
   resolve via `resolveAccessChainTypeVar()`, then call
   `extractReturnType()` to unwrap the function return type.
3. Add fields and methods from the resolved type.

**CompletionItemKind:** `field`, `method`

### 4. Type Annotation Completions — `after_colon`

**Trigger:** `x : `, `foo : `, a function signature parameter annotation

**What happens:**
1. `addTypeCompletions(module_env)` — iterates `all_statements` for
   `s_alias_decl` and `s_nominal_decl`, adding each type name.
2. `addModuleNameCompletions(module_env)` — iterates `all_statements` for
   `s_import` and adds imported module names/aliases.
3. `addTypeCompletionsFromEnv(env)` — same as (1) but across all modules
   in the `BuildEnv`.
4. `addModuleNameCompletionsFromEnv(env)` — adds all module names from
   `BuildEnv` schedulers **and** calls `addBuiltinModuleNameCompletions()`
   which adds every entry in `BUILTIN_TYPES`.

This means typing `x : ` suggests: user-defined type aliases and nominals,
imported module names, all loaded module names, and all 21 builtin types
(`Str`, `List`, `Bool`, `U8`–`U128`, `I8`–`I128`, `F32`, `F64`, `Dec`, `Num`,
`Dict`, `Set`, `Box`, `Try`).

**CompletionItemKind:** `class` (types), `module` (module names)

### 5. Expression Completions — `expression`

**Trigger:** beginning of a line, after `=`, after `if`, any general expression
position.

**What happens:**
1. `addLocalCompletions(module_env, cursor_offset)` — builds a `ScopeMap` from
   CIR, then adds every binding visible at the cursor. Also adds top-level
   defs (from `all_defs`) and statement-level defs (from `all_statements`).
   Closures/lambdas get kind `function`; everything else gets `variable`.
2. `addModuleNameCompletions(module_env)` — imported module names.
3. `addAmbientTagCompletions(module_env)` — structural tags from the type
   store (e.g. `Ok`, `Err` if they appear in the module).
4. `addTypeCompletions(module_env)` — alias and nominal type names.
5. `addModuleNameCompletionsFromEnv(env)` — all module names + builtins.

**CompletionItemKind:** `variable`, `function`, `module`, `enum_member`, `class`

### 6. Tag Completions (nominal)

Added in the `after_module_dot` context when the dot-prefix is a nominal type.
`addTagCompletionsForNominalType()` finds the `s_nominal_decl` matching the
name, checks opaqueness, then extracts tags from the backing `tag_union` type
annotation. Each tag gets a formatted signature detail like `SubVal(Str)`.

**CompletionItemKind:** `enum_member`

### 7. Ambient/Structural Tag Completions

Added in the `expression` context. `addAmbientTagCompletions()` iterates
`module_env.types.tags` and adds every tag name. These are structural tags that
appear without a module prefix.

**CompletionItemKind:** `enum_member`

---

## Key Types

### CompletionItem

Defined in `handlers/completion.zig:175`:

```zig
pub const CompletionItem = struct {
    label: []const u8,                // Display text (e.g. "concat", "Str", "name")
    kind: ?u32 = null,                // CompletionItemKind as u32
    detail: ?[]const u8 = null,       // Type signature shown beside label
    documentation: ?[]const u8 = null,
    sortText: ?[]const u8 = null,     // Custom sort key
    insertText: ?[]const u8 = null,   // Text inserted on accept (if different from label)
};
```

### CompletionItemKind

Matches the LSP spec (subset actually used by the builder):

| Name | Value | Used for |
|---|---|---|
| `method` | 2 | Static dispatch methods |
| `function` | 3 | Top-level functions, closures/lambdas |
| `field` | 5 | Record fields |
| `variable` | 6 | Local bindings, non-function top-level defs |
| `class` | 7 | Type names (aliases, nominals), uppercase module members |
| `module` | 9 | Module names (imported and builtin) |
| `enum_member` | 20 | Tag constructors (nominal and structural) |

### CompletionBuilder

Defined in `completion/builder.zig:33`. Key fields:

```zig
pub const CompletionBuilder = struct {
    allocator: Allocator,
    items: *std.ArrayList(CompletionItem),
    seen_labels: std.StringHashMap(void),   // deduplication
    builtin_module_env: ?*ModuleEnv,         // for Str.concat, List.map, etc.
};
```

### ModuleEnv (abbreviated)

From `src/canonicalize/ModuleEnv.zig`. Fields used by completions:

| Field | Purpose |
|---|---|
| `store: NodeStore` | Access CIR nodes (defs, statements, patterns, exprs, type annos) |
| `types: TypeStore` | Resolve type variables, unwrap aliases/nominals, iterate record fields |
| `common: CommonEnv` | Ident table (`idents`), exposed items, node index lookup |
| `all_defs: CIR.Def.Span` | All top-level definitions |
| `all_statements: CIR.Statement.Span` | All top-level statements (including imports, type decls) |
| `module_name: []const u8` | This module's name |
| `method_idents: MethodIdents` | Maps `(type_ident, method_ident) → qualified_ident` |

### BuildEnv (abbreviated)

| Field | Purpose |
|---|---|
| `schedulers` | `HashMap<JobQueueId, Scheduler>` — each scheduler has a `.modules` list of `ModuleState` |
| `builtin_modules.builtin_module.env` | The `ModuleEnv` for the Builtin module (Str, List, etc.) |

### ScopeMap and Binding

`scope_map.zig`. Reconstructs lexical scopes from CIR:

```zig
pub const Binding = struct {
    ident: Ident.Idx,
    pattern_idx: CIR.Pattern.Idx,
    visible_from: u32,
    visible_to: u32,
    is_parameter: bool,
};
```

`ScopeMap.build(module_env)` traverses all defs, statements, and nested
expressions to populate a list of `Binding`s sorted by `visible_from`.
`isVisibleAt(binding, offset)` checks if a binding is in scope at a cursor
position.

---

## Builder Method Reference

Every public `add*` method on `CompletionBuilder`:

| Method | What it adds |
|---|---|
| `addItem(item)` | Core: appends one item with deduplication. Returns `true` if added. |
| `addModuleNameCompletionsFromEnv(env)` | All module names from `BuildEnv` schedulers + `addBuiltinModuleNameCompletions()`. |
| `addModuleNameCompletions(module_env)` | Imported module aliases from `s_import` statements. |
| `addModuleMemberCompletions(env, name, opt)` | Exposed items from a named module (handles builtins and imports). |
| `addModuleMemberCompletionsFromModuleEnv(env, name)` | Exposed items from a specific `ModuleEnv`. |
| `addTypeCompletions(module_env)` | Alias and nominal type names from one module. |
| `addTypeCompletionsFromEnv(env)` | Type names from all modules in `BuildEnv`. |
| `addLocalCompletions(module_env, cursor_offset)` | Local variables in scope + top-level defs + statement-level defs. |
| `addRecordFieldCompletions(module_env, name, offset)` | Record fields by variable name lookup. |
| `addFieldsFromTypeVar(module_env, type_var)` | Record fields from a resolved type variable (unwraps aliases/nominals). |
| `addRecordFieldsForModuleMember(module_env, name)` | Record fields for `Module.value.` access patterns. |
| `addMethodCompletions(module_env, name, offset)` | Methods by variable name lookup. |
| `addMethodsFromTypeVar(module_env, type_var)` | Methods from a resolved type variable (alias ident → `method_idents` lookup). |
| `addTagCompletionsForNominalType(env, name, req_mod)` | Tag constructors for a nominal type, with opaqueness checks. |
| `addAmbientTagCompletions(module_env)` | Structural tags from the type store. |

---

## BUILTIN_TYPES

Defined in `completion/builtins.zig`. These 21 names are added as module-kind
completions by `addBuiltinModuleNameCompletions()`:

```
Str  List  Dict  Set  Box
Bool Try
U8  U16  U32  U64  U128
I8  I16  I32  I64  I128
F32  F64
Dec  Num
```

`isBuiltinType(name)` does a case-sensitive linear scan of this list. It is
used to:
- Route method lookups through the builtin `ModuleEnv` instead of the user's.
- Add builtin module names to completion lists.
- Recognise when `addModuleMemberCompletions` should use `builtin_module.env`.

---

## Testing

### Test files

| File | Contents |
|---|---|
| `test/handler_tests.zig` | Full end-to-end integration tests. Simulates an LSP session (initialize → didOpen → completion request → shutdown → exit) and parses the JSON-RPC response. |
| `test/syntax_test.zig` | Mid-level tests. Creates a `SyntaxChecker`, builds real Roc source, and calls `getCompletionsAtPosition()` directly. |
| `completion/context.zig` (inline) | Unit tests for `detectCompletionContext` and `computeOffset`. |
| `completion/builder.zig` (inline) | Unit tests for helper functions (`stripModulePrefix`, `firstSegment`, `lastSegment`). |
| `completion/builtins.zig` (inline) | Unit tests for `isBuiltinType` and `BUILTIN_TYPES.len`. |

### Handler test structure (handler_tests.zig)

Each test follows this pattern:

1. Create a temp directory and a `.roc` file path + URI.
2. Build a sequence of framed JSON-RPC messages:
   - `initialize` (id: 1)
   - `initialized`
   - `textDocument/didOpen` with Roc source as the `text` field
   - `textDocument/completion` (id: 2) with a specific line/character
   - `shutdown` (id: 3)
   - `exit`
3. Feed all messages into a `fixedBufferStream` reader.
4. Create a `Server` and call `server.run()`.
5. Collect all JSON-RPC responses from the writer buffer.
6. Find the response with `id: 2`.
7. Assert `result.isIncomplete` and `result.items` exist.
8. Scan `items` for expected labels (e.g., `found_str`, `found_u64`).

### Existing handler test cases

| Test name | Source code | Position | Verifies |
|---|---|---|---|
| `returns module definitions` | `module []\n\nfoo = 42\nbar = \|x\| x + 1` | 3:0 | items array exists |
| `returns module members after dot` | `app [...]\nimport Str\nresult = Str.` | 2:13 | Str member completions |
| `returns module names in expression context` | `app [...]\nimport Json\nresult = ` | 2:9 | `Json` appears |
| `returns types after colon` | `app [...]\nMyList:List(Str)\nx : ` | 2:4 | `Str`, `U64`, `Bool` appear |
| `returns List module members after List dot` | `app [...]\nresult = List.` | 1:14 | List member completions |
| `returns local variables in block scope` | block with `inner =` | inner scope | local vars visible |
| `returns lambda parameters` | `foo = \|x, y\| ...` | inside body | `x`, `y` visible |
| `returns top-level definitions` | `add = \|a, b\| ...\nresult = ` | after `=` | `add` appears |
| `returns record fields after dot` | `rec = { name: "hi" }\nresult = rec.` | after `rec.` | `name` field appears |

### Syntax test structure (syntax_test.zig)

These tests call `getCompletionsAtPosition()` directly on a `SyntaxChecker`:

1. Create a `SyntaxChecker` with a temp working directory.
2. Write a `.roc` file with specific source.
3. Call `syntax_checker.getCompletionsAtPosition(uri, source, line, char)`.
4. Assert on the returned `CompletionResult.items` — check specific labels,
   kinds, and detail strings.

### Running tests

```bash
# All completion-related tests
zig build test -- --test-filter "completion"

# Just context detection unit tests
zig build test -- --test-filter "detectCompletionContext"

# Just builder helper tests
zig build test -- --test-filter "stripModulePrefix"

# Just builtin type tests
zig build test -- --test-filter "isBuiltinType"

# Full test suite
zig build test
```

### Writing a new completion test

For a handler-level integration test, follow the pattern in `handler_tests.zig`:

```zig
test "completion handler returns X in Y context" {
    // 1. Setup: allocator, tmp dir, file path, URI
    // 2. Build JSON-RPC message sequence with your Roc source
    //    - The source goes in the didOpen "text" field
    //    - The completion position goes in the completion "position" field
    // 3. Run server
    // 4. Find response with your request id
    // 5. Scan items for expected labels
}
```

For a mid-level test in `syntax_test.zig`, call
`getCompletionsAtPosition(uri, source, line, character)` and inspect the
returned items directly.

For a unit test of context detection, add a test to `completion/context.zig`:

```zig
test "detectCompletionContext: your new case" {
    const source = "your source text here";
    const ctx = detectCompletionContext(source, line, character);
    try std.testing.expect(ctx == .your_expected_variant);
}
```

---

## Common Tasks

### Adding a new completion source to an existing context

Find the `switch (context)` in `syntax.zig:1890` and add your
`builder.add*()` call in the appropriate branch. If no suitable builder method
exists, add one to `completion/builder.zig`.

### Adding a new completion context

1. Add a variant to `CompletionContext` in `completion/context.zig`.
2. Update `detectCompletionContext()` to recognise the new trigger.
3. Add a branch in the `switch (context)` in `syntax.zig`.
4. Add context detection unit tests and at least one handler integration test.

### Adding a new completion item kind

The `CompletionItemKind` enum in `handlers/completion.zig` already has all 25
LSP kinds. Pick the appropriate one when creating items in the builder.
