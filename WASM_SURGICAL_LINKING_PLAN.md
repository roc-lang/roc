# WASM Surgical Linking Plan

## Status

| Phase | Description | Status |
|-------|-------------|--------|
| 1 | Padded LEB128 Helpers | Done |
| 2 | Linking Data Structures | Done |
| 3 | WASM Module Parser | Done |
| 4 | Surgical Linking — `linkHostToAppCalls()` | Done |
| 5 | Memory, Table, and Stack Pointer Ownership | Done |
| 6 | WASM Function Pointer Representation & RocOps Layout | Done |
| 7a | Entrypoint ABI Migration | Done |
| 7b | CodeBuilder & WasmCodeGen Refactor | Done |
| 8 | Builtins Migration | Done |
| 9 | Hosted Call Lowering | Done |
| 10 | Dead Code Elimination | Done |
| 11 | Serialization Updates | Done |
| 12 | CLI Integration — `roc build --target=wasm32` | Done |
| 13 | PIC Support & Eval Builtins | Done |
| 14 | Rebase & Integration Fixes | Done |
| 15 | Remaining Test Failures | Done |

## Overview

This document specifies the design and implementation plan for surgical linking of WebAssembly
modules in the Roc compiler. Surgical linking enables fast compilation by starting with a
**prebuilt relocatable host module** from the platform, then appending Roc app code and builtins
directly into that module, and finally resolving cross-references by patching relocation entries
in-place — no external linker required.

### Why Surgical Linking?

1. **Speed**: The host platform is compiled once. On every `roc build`, only app code is
   generated and "surgically" inserted. The linker does not re-process the host — it only
   patches the specific relocation sites that reference app symbols.

2. **Self-contained**: No dependency on `wasm-ld` or any external toolchain. The Roc compiler
   handles the entire pipeline from source to final `.wasm`.

3. **Proven approach**: The old Rust compiler (`crates/wasm_module/`) shipped this successfully.
   We are porting the same architecture to Zig, adapted to the new compiler's IR pipeline.

4. **Minimal output**: Dead code elimination after linking removes unused host functions,
   producing smaller `.wasm` files.

### The Model

```
Platform host.wasm (prebuilt, relocatable)
    │
    │  parse into in-memory WasmModule
    ▼
┌─────────────────────────────────────────┐
│  WasmModule (host code + relocations)   │
│  ├─ types, imports, functions, code     │
│  ├─ linking section (symbol table)      │
│  └─ reloc.CODE / reloc.DATA sections   │
└──────────────────┬──────────────────────┘
                   │
    merge roc_builtins.o (relocatable WASM object)
                   │
                   ▼
┌─────────────────────────────────────────┐
│  WasmModule (host + builtins merged)    │
│  ├─ builtin functions appended          │
│  ├─ symbol tables merged                │
│  └─ relocation entries merged           │
└──────────────────┬──────────────────────┘
                   │
    WasmCodeGen appends app function bodies
                   │
                   ▼
┌─────────────────────────────────────────┐
│  WasmModule (host + builtins + app)     │
│  ├─ app functions appended to code      │
│  ├─ app symbols added to symbol table   │
│  └─ app relocation entries added        │
└──────────────────┬──────────────────────┘
                   │
    linkHostToAppCalls(): resolve host→app imports
    via relocation patching
                   │
                   ▼
┌─────────────────────────────────────────┐
│  WasmModule (fully resolved)            │
│  ├─ host imports replaced with          │
│  │   defined app functions              │
│  ├─ all call sites patched via relocs   │
│  └─ dummy functions preserve indices    │
└──────────────────┬──────────────────────┘
                   │
    eliminateDeadCode(): trace call graph,
    stub unreachable functions
                   │
                   ▼
┌─────────────────────────────────────────┐
│  WasmModule (trimmed)                   │
│  ├─ dead function bodies → unreachable  │
│  └─ dead imports removed                │
└──────────────────┬──────────────────────┘
                   │
    serialize(): strip linking metadata,
    emit standard WASM binary
                   │
                   ▼
         final.wasm (ready to execute)
```

### Key Terminology

- **Relocatable WASM module**: A `.wasm` file (or `.o` file) that contains custom `linking`
  and `reloc.*` sections per the
  [WebAssembly Tool Conventions](https://github.com/WebAssembly/tool-conventions/blob/main/Linking.md).
  All mutable indices are encoded as 5-byte padded LEB128 so they can be patched in-place.
- **Surgical linking**: Resolving symbol references by overwriting padded LEB128 slots at
  known relocation offsets, rather than re-encoding the entire module.
- **Host module**: The platform's prebuilt relocatable WASM, containing host code with
  imports for Roc app functions (e.g. `roc__main_for_host_1_exposed`).
- **App functions**: Roc application code compiled by `WasmCodeGen`, appended into the host module.
- **Builtins**: `roc_builtins.o` — Roc's standard library functions (str ops, dec math, list ops)
  compiled from `src/builtins/` to a relocatable WASM object. These are internal to the app
  side, not provided by the host.

---

## Phase 1: Padded LEB128 Helpers

### What

Add two utility functions for working with 5-byte padded LEB128, the encoding format that
makes surgical linking possible.

### Why

In standard WASM, LEB128 values use variable-length encoding (1–5 bytes for a u32). If we
changed a function index from `3` (1 byte) to `300` (2 bytes), every byte offset after that
point would shift, invalidating all subsequent relocation offsets. Padded LEB128 solves this
by always using exactly 5 bytes, regardless of value. This means we can overwrite any index
in-place without affecting surrounding code.

### Implementation

Add to `WasmModule.zig`:

```zig
/// Overwrite 5 bytes at `buffer[offset..offset+5]` with a u32 in padded LEB128.
/// The buffer must already have 5 bytes reserved at that position.
/// This is the core primitive for surgical relocation patching.
pub fn overwritePaddedU32(buffer: []u8, offset: u32, value: u32) void {
    var x = value;
    const off = @as(usize, offset);
    for (0..4) |i| {
        buffer[off + i] = @as(u8, @truncate(x & 0x7f)) | 0x80;
        x >>= 7;
    }
    buffer[off + 4] = @as(u8, @truncate(x));
}

/// Overwrite 5 bytes with a signed i32 in padded LEB128.
/// Used for signed memory address relocations.
pub fn overwritePaddedI32(buffer: []u8, offset: u32, value: i32) void {
    var x = value;
    const off = @as(usize, offset);
    for (0..4) |i| {
        buffer[off + i] = @as(u8, @truncate(@as(u32, @bitCast(x)) & 0x7f)) | 0x80;
        x >>= 7;
    }
    buffer[off + 4] = @as(u8, @truncate(@as(u32, @bitCast(x)) & 0x7f));
}

/// Append a u32 as exactly 5 bytes of padded LEB128 to an output buffer.
/// Used when emitting new relocatable instructions (call, global.get/set).
pub fn appendPaddedU32(output: *std.ArrayList(u8), value: u32) !void {
    var x = value;
    for (0..4) |_| {
        try output.append(@as(u8, @truncate(x & 0x7f)) | 0x80);
        x >>= 7;
    }
    try output.append(@as(u8, @truncate(x)));
}
```

### Rust Reference

- `crates/wasm_module/src/serialize.rs` lines 149–156: `overwrite_padded_u32()`
- `crates/wasm_module/src/serialize.rs` lines 140–147: `overwrite_padded_i32()`
- `crates/wasm_module/src/serialize.rs` lines 235–240: `encode_padded_u32()` (trait method)

### Tests

```
test "overwritePaddedU32 — value 0 encodes as [0x80, 0x80, 0x80, 0x80, 0x00]"
test "overwritePaddedU32 — value 1 encodes as [0x81, 0x80, 0x80, 0x80, 0x00]"
test "overwritePaddedU32 — value 0x7F encodes as [0xFF, 0x80, 0x80, 0x80, 0x00]"
test "overwritePaddedU32 — value 128 encodes as [0x80, 0x81, 0x80, 0x80, 0x00]"
test "overwritePaddedU32 — max u32 (0xFFFFFFFF) encodes correctly"
test "overwritePaddedU32 — round-trip: write then decode matches original value"
test "overwritePaddedI32 — negative value (-1) encodes correctly"
test "overwritePaddedI32 — positive value round-trips correctly"
test "appendPaddedU32 — appends exactly 5 bytes"
test "appendPaddedU32 — output is decodable as standard LEB128"
```

---

## Phase 2: Linking Data Structures

### What

Define the data structures for WASM relocatable module metadata: symbol table entries,
relocation entries, and the linking section container.

### Why

These structures are the "map" that makes surgical linking possible. The **symbol table**
records what symbols exist (functions, data, globals) and whether they are defined or imported.
The **relocation entries** record where in the code/data sections each symbol is referenced.
Together they form a bidirectional index: given a symbol, we can find every instruction that
references it and patch it.

Without these structures, changing a function's index would require scanning the entire code
section for `call` instructions — slow and error-prone. With relocations, we jump directly
to the exact byte offsets that need patching.

### Implementation

Create a new file `src/backend/wasm/WasmLinking.zig`:

#### Relocation Types

```zig
/// Index-based relocation types (no addend).
/// These patch indices in instructions like `call`, `global.get`, `call_indirect`.
pub const IndexRelocType = enum(u8) {
    function_index_leb = 0,   // R_WASM_FUNCTION_INDEX_LEB — function index in `call`
    table_index_sleb = 1,     // R_WASM_TABLE_INDEX_SLEB — signed table index in `i32.const`
    table_index_i32 = 2,      // R_WASM_TABLE_INDEX_I32 — table index as raw u32 in data
    type_index_leb = 6,       // R_WASM_TYPE_INDEX_LEB — type index in `call_indirect`
    global_index_leb = 7,     // R_WASM_GLOBAL_INDEX_LEB — global index in `global.get/set`
    event_index_leb = 10,     // R_WASM_EVENT_INDEX_LEB
    global_index_i32 = 13,    // R_WASM_GLOBAL_INDEX_I32
    table_number_leb = 20,    // R_WASM_TABLE_NUMBER_LEB
};

/// Offset-based relocation types (have an addend).
/// These patch memory addresses in load/store instructions and data segments.
pub const OffsetRelocType = enum(u8) {
    memory_addr_leb = 3,      // R_WASM_MEMORY_ADDR_LEB — unsigned addr in load/store
    memory_addr_sleb = 4,     // R_WASM_MEMORY_ADDR_SLEB — signed addr in `i32.const`
    memory_addr_i32 = 5,      // R_WASM_MEMORY_ADDR_I32 — raw u32 addr in data segment
    function_offset_i32 = 8,  // R_WASM_FUNCTION_OFFSET_I32
    section_offset_i32 = 9,   // R_WASM_SECTION_OFFSET_I32
};
```

#### Relocation Entry

```zig
/// A single relocation entry. Describes one site in the code or data section
/// that references a symbol and needs patching when that symbol's value changes.
pub const RelocationEntry = union(enum) {
    /// Index relocations: the value at `offset` is a symbol index (function, type, global).
    /// No addend — the patched value is the symbol's resolved index directly.
    index: struct {
        type_id: IndexRelocType,
        offset: u32,         // byte offset within the target section body
        symbol_index: u32,   // index into the linking section's symbol table
    },

    /// Offset relocations: the value at `offset` is a memory address.
    /// The patched value is the symbol's address + addend.
    offset: struct {
        type_id: OffsetRelocType,
        offset: u32,
        symbol_index: u32,
        addend: i32,
    },

    pub fn getSymbolIndex(self: RelocationEntry) u32 {
        return switch (self) {
            .index => |i| i.symbol_index,
            .offset => |o| o.symbol_index,
        };
    }

    pub fn getOffset(self: RelocationEntry) u32 {
        return switch (self) {
            .index => |i| i.offset,
            .offset => |o| o.offset,
        };
    }
};
```

#### Symbol Info

```zig
/// Flags for symbol table entries.
pub const SymFlag = struct {
    pub const BINDING_WEAK: u32      = 0x01;
    pub const BINDING_LOCAL: u32     = 0x02;
    pub const VISIBILITY_HIDDEN: u32 = 0x04;
    pub const UNDEFINED: u32         = 0x10;
    pub const EXPORTED: u32          = 0x20;
    pub const EXPLICIT_NAME: u32     = 0x40;
    pub const NO_STRIP: u32          = 0x80;
};

/// Symbol kinds in the linking section's symbol table.
pub const SymKind = enum(u8) {
    function = 0,
    data = 1,
    global = 2,
    section = 3,
    event = 4,
    table = 5,
};

/// A symbol table entry. Each symbol has a kind, flags, and an index
/// into the relevant index space (function index, global index, etc.).
///
/// Function/global symbols can be **explicitly named** (name stored in the linking
/// section) or **implicitly named** (undefined symbols that inherit their name from
/// the import section entry they reference). The old Rust parser distinguishes these
/// as `ExplicitlyNamed` vs `ImplicitlyNamed` variants (`crates/wasm_module/src/linking.rs`
/// lines 354–385). We model this with an optional name field.
///
/// Parsing rule: a function/global symbol gets a name from the linking section if
/// `(flags & WASM_SYM_EXPLICIT_NAME) != 0` OR `(flags & WASM_SYM_UNDEFINED) == 0`
/// (i.e. defined symbols always have names). Undefined symbols without EXPLICIT_NAME
/// have `name = null` — their name must be looked up from the import section at the
/// symbol's `index`.
pub const SymInfo = struct {
    kind: SymKind,
    flags: u32,
    /// Explicit name from the linking section, or null for implicitly-named
    /// imported symbols (whose name comes from the import section).
    name: ?[]const u8,
    /// For function symbols: the function index (import or defined).
    /// For global symbols: the global index.
    /// For data symbols: segment index (stored here, offset/size stored separately).
    index: u32,
    /// Data symbols only: offset within segment.
    data_offset: u32 = 0,
    /// Data symbols only: size in bytes.
    data_size: u32 = 0,

    pub fn isUndefined(self: SymInfo) bool {
        return (self.flags & SymFlag.UNDEFINED) != 0;
    }

    pub fn isImplicitlyNamed(self: SymInfo) bool {
        return self.name == null;
    }

    pub fn isLocal(self: SymInfo) bool {
        return (self.flags & SymFlag.BINDING_LOCAL) != 0;
    }

    pub fn isFunction(self: SymInfo) bool {
        return self.kind == .function;
    }

    /// Resolve this symbol's name when one is available.
    ///
    /// Explicitly named symbols return their stored name.
    /// Implicitly named undefined function/global/event/table symbols inherit
    /// their name from the import section.
    /// Section symbols and other unnamed non-import symbols return null.
    pub fn resolveName(self: SymInfo, imports: []const Import) ?[]const u8 {
        if (self.name) |n| return n;

        if (!self.isUndefined()) return null;

        return switch (self.kind) {
            .function, .global, .event, .table => imports[self.index].field_name,
            else => null,
        };
    }
};
```

#### Relocation Section

```zig
/// Holds all relocation entries for one section (either "reloc.CODE" or "reloc.DATA").
pub const RelocationSection = struct {
    /// Name of this reloc section (e.g. "reloc.CODE").
    name: []const u8,
    /// Index of the target section these relocations apply to.
    target_section_index: u32,
    /// The relocation entries, sorted by offset.
    entries: std.ArrayList(RelocationEntry),

    /// Patch all sites in `section_bytes` that reference `sym_index` with `value`.
    /// This is the core surgical linking primitive.
    pub fn applyRelocsU32(
        self: *const RelocationSection,
        section_bytes: []u8,
        sym_index: u32,
        value: u32,
    ) void {
        for (self.entries.items) |entry| {
            if (entry.getSymbolIndex() != sym_index) continue;
            switch (entry) {
                .index => |idx| {
                    switch (idx.type_id) {
                        .function_index_leb,
                        .type_index_leb,
                        .global_index_leb,
                        .event_index_leb,
                        .table_number_leb,
                        => overwritePaddedU32(section_bytes, idx.offset, value),
                        .table_index_sleb => overwritePaddedI32(
                            section_bytes, idx.offset, @as(i32, @intCast(value)),
                        ),
                        .table_index_i32, .global_index_i32 => {
                            const off = @as(usize, idx.offset);
                            std.mem.writeInt(u32, section_bytes[off..][0..4], value, .little);
                        },
                    }
                },
                .offset => |off| {
                    const patched = @as(i64, value) + @as(i64, off.addend);
                    switch (off.type_id) {
                        .memory_addr_leb => overwritePaddedU32(
                            section_bytes, off.offset, @intCast(patched),
                        ),
                        .memory_addr_sleb => overwritePaddedI32(
                            section_bytes, off.offset, @intCast(patched),
                        ),
                        .memory_addr_i32,
                        .function_offset_i32,
                        .section_offset_i32,
                        => {
                            const o = @as(usize, off.offset);
                            std.mem.writeInt(u32, section_bytes[o..][0..4], @intCast(patched), .little);
                        },
                    }
                },
            }
        }
    }
};
```

#### Linking Section

```zig
pub const LINKING_VERSION: u32 = 2;

/// Linking subsection types (within the "linking" custom section).
pub const LinkingSubsection = enum(u8) {
    segment_info = 5,
    init_funcs = 6,
    comdat_info = 7,
    symbol_table = 8,
};

/// Container for all linking metadata from a relocatable WASM module.
pub const LinkingSection = struct {
    symbol_table: std.ArrayList(SymInfo),
    segment_info: std.ArrayList(SegmentInfo),
    init_funcs: std.ArrayList(InitFunc),

    /// Find a symbol by name. For implicitly-named imported symbols, resolves
    /// the name from the import section. Returns the symbol index, or null.
    pub fn findSymbolByName(
        self: *const LinkingSection,
        name: []const u8,
        imports: []const Import,
    ) ?u32 {
        for (self.symbol_table.items, 0..) |sym, i| {
            if (sym.resolveName(imports)) |sym_name| {
                if (std.mem.eql(u8, sym_name, name)) return @intCast(i);
            }
        }
        return null;
    }

    /// Find the symbol table index for an imported function at the given function index.
    pub fn findImportedFnSymIndex(self: *const LinkingSection, fn_index: u32) ?u32 {
        for (self.symbol_table.items, 0..) |sym, i| {
            if (sym.kind == .function and sym.isUndefined() and sym.index == fn_index) {
                return @intCast(i);
            }
        }
        return null;
    }

    /// Find the symbol for an imported function at `old_fn_index` and update it
    /// to point to `new_fn_index`. Returns the symbol index.
    pub fn findAndReindexImportedFn(
        self: *LinkingSection,
        old_fn_index: u32,
        new_fn_index: u32,
    ) ?u32 {
        for (self.symbol_table.items, 0..) |*sym, i| {
            if (sym.kind == .function and sym.isUndefined() and sym.index == old_fn_index) {
                sym.index = new_fn_index;
                return @intCast(i);
            }
        }
        return null;
    }
};

pub const SegmentInfo = struct {
    name: []const u8,
    alignment: u32,
    flags: u32,
};

pub const InitFunc = struct {
    priority: u32,
    symbol_index: u32,
};
```

### Rust Reference

- `crates/wasm_module/src/linking.rs` lines 18–42: `IndexRelocType` enum
- `crates/wasm_module/src/linking.rs` lines 64–87: `OffsetRelocType` enum
- `crates/wasm_module/src/linking.rs` lines 105–118: `RelocationEntry` enum
- `crates/wasm_module/src/linking.rs` lines 321–352: symbol flag constants
- `crates/wasm_module/src/linking.rs` lines 445–453: `SymInfo` enum
- `crates/wasm_module/src/linking.rs` lines 557–562: `LinkingSection` struct
- `crates/wasm_module/src/linking.rs` lines 169–208: `apply_relocs_u32()`
- `crates/wasm_module/src/linking.rs` lines 576–621: symbol lookup/reindex functions

### Tests

```
test "RelocationSection.applyRelocsU32 — patches function_index_leb at correct offset"
test "RelocationSection.applyRelocsU32 — patches multiple sites for same symbol"
test "RelocationSection.applyRelocsU32 — ignores entries for different symbols"
test "RelocationSection.applyRelocsU32 — memory_addr_leb adds addend correctly"
test "RelocationSection.applyRelocsU32 — memory_addr_sleb handles negative addend"
test "LinkingSection.findSymbolByName — finds existing symbol"
test "LinkingSection.findSymbolByName — returns null for missing symbol"
test "LinkingSection.findImportedFnSymIndex — finds undefined function symbol"
test "LinkingSection.findAndReindexImportedFn — updates index and returns sym index"
```

---

## Phase 3: WASM Module Parser

### What

Add a `preload()` function to `WasmModule.zig` that parses a relocatable WASM binary into
the in-memory `WasmModule` representation. This is the reverse of the existing `encode()`.

### Why

The surgical linking pipeline starts by loading the platform's prebuilt host module. We need
to parse it into a mutable structure so we can:

1. Inspect its imports (to find which ones are app function stubs)
2. Append new function bodies (app code and builtins)
3. Modify its symbol table and relocation entries
4. Re-serialize the final combined module

The parser must handle both standard WASM sections and the custom linking/relocation sections
that are specific to relocatable objects. It must validate that the module is actually
relocatable (has symbol table, has relocations, no internally-defined globals).

### Data Structure Changes

The `WasmModule` struct needs to be extended significantly. The current struct is serialization-only
and stores high-level types (function signatures, bodies, exports). For surgical linking, we
need to also store:

- Raw code section bytes (for in-place relocation patching)
- Function byte offsets within the code section (to map function indices to code ranges)
- The linking section (symbol table + segment info)
- Relocation sections (reloc.CODE + reloc.DATA)
- Raw data section bytes
- Import details including module/field names
- A `dead_import_dummy_count` counter for index stability

The extended struct should look like:

```zig
pub const WasmModule = struct {
    allocator: Allocator,

    // --- Standard sections ---
    func_types: std.ArrayList(FuncType),
    func_type_results: std.ArrayList(?ValType),

    /// Import entries. Function imports occupy indices 0..import_fn_count-1.
    imports: std.ArrayList(Import),
    import_fn_count: u32,

    /// Function section: type index for each locally-defined function.
    func_type_indices: std.ArrayList(u32),

    // Table
    has_table: bool,
    table_func_indices: std.ArrayList(u32),

    // Memory
    has_memory: bool,
    memory_min_pages: u32,

    // Globals
    has_stack_pointer: bool,
    stack_pointer_init: u32,

    // Exports
    exports: std.ArrayList(Export),

    // Element section (for call_indirect table)
    // ... (existing fields)

    // --- Code section (raw bytes for surgical patching) ---
    /// Raw bytes of all function bodies in the code section.
    /// Relocation offsets refer to positions within this buffer.
    code_bytes: std.ArrayList(u8),
    /// Byte offset of each function body within code_bytes.
    /// Length: func_type_indices.items.len (locally-defined functions only).
    function_offsets: std.ArrayList(u32),
    /// Number of dummy functions prepended during linking to maintain index stability.
    dead_import_dummy_count: u32,

    // --- Data section ---
    data_segments: std.ArrayList(DataSegment),
    data_offset: u32,

    // --- Linking metadata (from custom sections) ---
    linking: WasmLinking.LinkingSection,
    reloc_code: WasmLinking.RelocationSection,
    reloc_data: WasmLinking.RelocationSection,

    // ... (methods follow)
};
```

### Parser Implementation

Add a `preload()` method:

```zig
/// Parse a relocatable WASM binary into a WasmModule.
/// The input bytes must contain `linking` and `reloc.*` custom sections.
/// Returns error if the module is not relocatable or is malformed.
pub fn preload(allocator: Allocator, bytes: []const u8, require_relocatable: bool) !WasmModule {
    // 1. Validate magic ("\0asm") and version (1)
    // 2. Iterate sections in Wasm binary order:
    //    - For each standard section: parse into the appropriate field
    //    - Consume optional DataCount if present (used by Zig-built objects)
    //    - For custom sections: check name, parse linking/reloc.CODE/reloc.DATA,
    //      skip all other custom sections unchanged
    // 3. If require_relocatable: validate symbol table and reloc.CODE exist
    // 4. Return populated WasmModule
}
```

**Section parsing order** (section IDs per WASM spec):

| ID | Section | What to parse |
|----|---------|---------------|
| 1  | Type | Function signatures (param types, result types) |
| 2  | Import | Module name, field name, import descriptor (func/memory/table/global) |
| 3  | Function | Type index for each locally-defined function |
| 4  | Table | Table type and limits |
| 5  | Memory | Memory limits (min pages, optional max pages) |
| 6  | Global | Global type + init expression (must be empty for relocatable modules) |
| 7  | Export | Name, kind, index |
| 8  | Start | Start function index (optional) |
| 9  | Element | Element segments (function table initialization) |
| 12 | DataCount | Consume and ignore (present in some relocatable objects, including the shipped wasm builtins) |
| 10 | Code | **Store raw bytes + record function offsets** |
| 11 | Data | Data segments with memory offsets |
| 0  | Custom | Check name: "linking", "reloc.CODE", "reloc.DATA", "name"; skip all other custom sections such as debug metadata |

**Code section parsing detail**: Do NOT parse individual instructions. Store the entire section
body as raw bytes in `code_bytes`. Walk through function entries only to record each function's
byte offset in `function_offsets`. This is critical — we need the raw bytes for relocation
patching, and parsing/re-encoding instructions would lose the padded LEB128 encoding.

**Linking section parsing** (custom section named "linking"):
1. Read version (must be 2)
2. Loop over subsections:
   - Subsection 8 (WASM_SYMBOL_TABLE): Parse symbol count, then for each symbol: kind, flags,
     and kind-specific fields (function index + optional name, data segment/offset/size, etc.)
   - Subsection 5 (WASM_SEGMENT_INFO): Parse segment metadata
   - Subsection 6 (WASM_INIT_FUNCS): Parse init function list
   - Subsection 7 (WASM_COMDAT_INFO): Parse COMDAT groups (can skip for now)

**Relocation section parsing** (custom sections named "reloc.CODE" / "reloc.DATA"):
1. Read target section index
2. Read relocation count
3. For each entry: read type byte, decode as index or offset relocation, read fields

**Validation for relocatable modules**:
- Symbol table must exist and be non-empty
- `reloc.CODE` must exist and be non-empty (`reloc.DATA` remains optional)
- No internally-defined globals (the `__stack_pointer` global must come from an import,
  because its index needs to be relocatable)

### Rust Reference

- `crates/wasm_module/src/lib.rs` lines 118–234: `preload()` — the complete parser
- `crates/wasm_module/src/lib.rs` lines 146–151: magic/version validation
- `crates/wasm_module/src/lib.rs` lines 153–232: section dispatch loop
- `crates/wasm_module/src/lib.rs` lines 189–201: relocatable validation
- `crates/wasm_module/src/linking.rs` lines 520–543: linking subsection parsing
- `crates/wasm_module/src/linking.rs` lines 355–445: symbol table entry parsing
- `crates/wasm_module/src/linking.rs` lines 119–152: relocation section parsing
- `crates/wasm_module/src/sections.rs` lines 1410–1449: code section parsing (function offset tracking)

### Tests

```
test "preload — rejects bytes without WASM magic number"
test "preload — rejects wrong version"
test "preload — parses type section with multiple signatures"
test "preload — parses import section with function and memory imports"
test "preload — records correct function_offsets for code section"
test "preload — parses linking section symbol table"
test "preload — parses reloc.CODE section entries"
test "preload — parses reloc.DATA section entries"
test "preload — accepts relocatable object with optional DataCount section"
test "preload — require_relocatable rejects module without linking section"
test "preload — require_relocatable rejects module without reloc sections"
test "preload — parsed module has correct function count, import count, symbol count"
```

Note: there is **no round-trip requirement**. The parser reads relocatable objects; the
serializer emits final (non-relocatable) modules with linking sections stripped. These are
different formats by design. To validate the parser independently, check that parsed field
counts and symbol names match expected values from known test fixtures.

**Test fixture**: Use `clang --target=wasm32 -c -o test.o test.c` to produce a minimal
relocatable WASM object file with known symbols and relocations. Alternatively, hand-craft
a minimal relocatable module in a byte array literal.

---

## Phase 4: Surgical Linking — `linkHostToAppCalls()`

### What

Implement the core surgical linking operation: given a mapping of
`(app_fn_name, app_fn_index)` pairs, find the host's imports for those names and replace
them with calls to the defined app functions, patching all relocation sites.

### Why

The host module has imports like `roc__main_for_host_1_exposed` — these are stubs that the
host calls but doesn't define. The app compiler generates the actual implementations and
appends them to the module at known function indices. Surgical linking bridges the gap:
it removes the import and redirects all call sites to the defined function.

The tricky part is **index stability**. In WASM, function indices are a single global
namespace: imports occupy indices 0..N-1, locally-defined functions occupy N..M-1. Removing
an import shifts every defined function's index down by 1, which would require patching
every reference to every defined function. Instead, we use the **dummy function trick**:
insert a 3-byte `unreachable; end` stub at the vacated position so the total function count
stays the same and only the specific swapped indices need updating.

### Algorithm

For each `(app_fn_name, app_fn_index)` in the host-to-app map:

```
1. FIND the import:
   Walk imports, counting only function imports.
   Find the one whose field name matches app_fn_name.
   Record: host_import_index (position in imports array)
            host_fn_index (its function index = position among fn imports)

2. FIND the last JS/env function import:
   This is the import we'll move into the vacated slot.
   Record: swap_import_index, swap_fn_index

3. SWAP imports:
   Remove the swap import from its position.
   If swap_import_index != host_import_index:
       Insert swap import at host_import_index position.
   (This puts the swap import where the app import was,
    and removes one import from the total count.)

4. INSERT dummy function:
   Increment dead_import_dummy_count.
   Insert a dummy type signature at position 0 in func_type_indices.
   (Dummy functions are prepended to the code section during serialization.)

5. UPDATE symbol table and apply relocations for the host function:
   sym_index = linking.findAndReindexImportedFn(host_fn_index, app_fn_index)
   reloc_code.applyRelocsU32(code_bytes, sym_index, app_fn_index)

6. UPDATE symbol table and apply relocations for the swapped function:
   (If swap_fn_index != host_fn_index)
   sym_index = linking.findAndReindexImportedFn(swap_fn_index, host_fn_index)
   reloc_code.applyRelocsU32(code_bytes, sym_index, host_fn_index)
```

**Why the swap?** We can't just remove the app import and leave a gap — that would shift
all subsequent import indices. Instead we move the last import into the gap, and only two
symbols need relocation updates (the removed app import → now points to defined function,
the moved last import → now at the removed import's index). Everything else is untouched.

### The Dummy Function

```zig
const DUMMY_FUNCTION = [3]u8{
    0x00,                    // zero local variable declarations
    Op.unreachable,          // trap if called (means DCE was wrong)
    Op.end,                  // end of function body
};
```

During serialization, `dead_import_dummy_count` dummy functions are prepended before the
real code section functions. This means:

- Function index `import_count + 0` → first dummy
- Function index `import_count + dead_import_dummy_count` → first real defined function
- Original defined-function indices remain unchanged overall: the import count goes down by
  exactly the same amount that dummy functions are inserted at the front of the code section.

### Rust Reference

- `crates/wasm_module/src/lib.rs` lines 524–627: `link_host_to_app_calls()` — the complete algorithm
- `crates/wasm_module/src/lib.rs` lines 544–570: finding host and swap imports
- `crates/wasm_module/src/lib.rs` lines 572–586: swapping imports
- `crates/wasm_module/src/lib.rs` lines 588–627: updating symbols and applying relocations
- `crates/wasm_module/src/lib.rs` lines 900–904: `DUMMY_FUNCTION` constant

### Tests

```
test "linkHostToAppCalls — single app function: import removed, dummy inserted"
test "linkHostToAppCalls — verifies call instruction patched to app function index"
test "linkHostToAppCalls — last import swapped into vacated slot"
test "linkHostToAppCalls — swap import's call sites updated to new index"
test "linkHostToAppCalls — multiple app functions linked in sequence"
test "linkHostToAppCalls — dead_import_dummy_count incremented correctly"
test "linkHostToAppCalls — func_type_indices has dummy signature at position 0"
test "linkHostToAppCalls — total function count unchanged after linking"
```

**Test approach**: Construct a minimal WasmModule in memory with:
- 3 function imports: `js_foo`, `roc__main_exposed`, `js_bar`
- 2 defined functions with `call` instructions referencing the imports
- Proper symbol table and relocation entries
- Then call `linkHostToAppCalls` with `roc__main_exposed → fn_index_5`
- Verify: import count decreased by 1, `js_bar` moved to slot 1, dummy inserted,
  call instructions patched to correct indices.

---

## Phase 5: Memory, Table, and Stack Pointer Ownership

### What

Define and implement the ownership rules for WASM linear memory, the function reference
table, and the `__stack_pointer` global. These must be set up correctly before any code
generation or linking can proceed.

### Why

In a relocatable WASM object, memory, table, and `__stack_pointer` are **imported** — the
object doesn't own them. But in the final linked module, they must be **defined** (owned by
the module). The surgical linking pipeline must handle this transition:

1. The host module imports memory and table from the environment
2. During setup, we **remove** those imports (the final module will define them)
3. We define memory with the correct page count and table with the correct size
4. The `__stack_pointer` global is imported in relocatable objects (so its index can be
   relocated) but must become a defined mutable global in the final module

The old Rust compiler handled this explicitly in `WasmBackend::new()` at
`crates/compiler/gen_wasm/src/backend.rs` lines 94–99 (import removal) and in `finalize()`
at lines 296–304 (memory layout and table sizing).

### Design

**During host module setup** (after `preload()`, before code generation):

```
1. REMOVE only Memory and Table imports from the host module's import section:
   host_module.imports.retain(|imp| imp is not Memory and not Table)
   Update import_fn_count if any non-function imports were before function imports.
   KEEP the __stack_pointer global import — it is needed during code generation
   and linking because relocation entries reference it by its global index.
```

**During finalization** (after all code generation and surgical linking, before serialization):

```
2. REPLACE __stack_pointer global import with a defined global:
   Remove the __stack_pointer import from the import section.
   Define it as: type=i32, mutable=true, init=i32.const(stack_pointer_init)
   stack_pointer_init = memory_pages * 65536  (top of memory)
   The old Rust compiler does this in set_memory_layout() at
   crates/compiler/gen_wasm/src/backend.rs lines 151–174.

3. DEFINE memory:
   host_module.has_memory = true
   host_module.memory_min_pages = calculated from stack_bytes and data size
   Memory is exported as "memory" for host access.

4. DEFINE table:
   host_module.has_table = true
   Table size = 1 + max element index (computed after all functions are registered)
   Table type = funcref, limits = MinMax(size, size)
```

**Important**: The split between setup (step 1) and finalization (steps 2–4) is critical.
During the linking phase, `__stack_pointer` references are resolved via its global import
index and relocation entries. Only after all linking is complete can we replace the import
with a defined global at the correct initial value.

**Memory layout**:

```
┌────────────────────────────────┐  ← memory_pages * 65536
│  Stack (grows downward)        │
│  ← __stack_pointer starts here│
├────────────────────────────────┤
│  (free space)                  │
├────────────────────────────────┤
│  Data segments                 │
│  (constants, string literals)  │
├────────────────────────────────┤
│  Reserved (offset 0–1023)      │
│  RocOps struct at offset 0     │
└────────────────────────────────┘  ← offset 0
```

### Platform Host Artifact Type

The CLI target selection at `src/cli/main.zig` lines 3989–4017 prefers `.exe` over
`.static_lib`. For WASM, the host artifact should be declared under the **`exe`** target
key in the platform's `TargetsConfig`:

```roc
targets: {
    files: "targets/",
    exe: {
        wasm32: ["host.wasm", app],
    }
}
```

The `host.wasm` is a **relocatable WASM object** (produced with `clang --target=wasm32 -c`
or equivalent), not a final executable. Despite being listed under `exe`, it's an object file
that the surgical linker will combine with app code to produce the final executable `.wasm`.
This matches the pattern used by native targets where hosts provide `.o` files under `exe`.

**Important CLI requirement**: the existing generic `LinkItem.file_path` handling in
`src/cli/main.zig` cannot treat `host.wasm` as an ordinary linker input. For wasm32
executable builds, the CLI must resolve one host-module path from the link spec and hand
its bytes directly to `WasmModule.preload()`. It must NOT append `host.wasm` to
`platform_files_pre` / `platform_files_post`, and it must NOT pass it through the normal
`wasm-ld` input collection path.

This can be implemented either by:

1. adding a dedicated `LinkItem.host_wasm`, or
2. adding a wasm32-specific branch in `src/cli/main.zig` that extracts the first pre-`app`
   host `.wasm` path from `targets.exe.wasm32`.

The plan does not require one specific encoding in `TargetsConfig`; it does require that the
resolved host module path be consumed by the surgical-linking pipeline rather than by the
generic linker-input pipeline.

### Rust Reference

- `crates/compiler/gen_wasm/src/backend.rs` lines 94–99: removing Memory/Table imports
- `crates/compiler/gen_wasm/src/backend.rs` lines 296–304: `finalize()` — `set_memory_layout()`,
  `export_globals()`, table sizing

### Tests

```
test "setup — memory and table imports removed from host module"
test "setup — import_fn_count unchanged after removing non-function imports"
test "setup — __stack_pointer global defined with correct initial value"
test "setup — memory section has correct minimum pages"
test "setup — table size matches element count after finalization"
test "setup — memory exported as 'memory'"
```

---

## Phase 6: WASM Function Pointer Representation & RocOps Layout

### What

Define exactly how function pointers are represented in WASM for the RocOps struct and
HostedFunctions array. WASM cannot use raw function pointers — all indirect calls must go
through a `funcref` table via `call_indirect`.

### Why

The generic `RocOps` struct (`src/builtins/host_abi.zig` lines 88–179) stores raw function
pointers on native targets:

```zig
// Native (64-bit): RocOps is 72 bytes
pub const RocOps = extern struct {
    env: *anyopaque,                    // offset 0  (8 bytes)
    roc_alloc: *const fn(...),          // offset 8  (8 bytes)
    roc_dealloc: *const fn(...),        // offset 16 (8 bytes)
    roc_realloc: *const fn(...),        // offset 24 (8 bytes)
    roc_dbg: *const fn(...),            // offset 32 (8 bytes)
    roc_expect_failed: *const fn(...),  // offset 40 (8 bytes)
    roc_crashed: *const fn(...),        // offset 48 (8 bytes)
    hosted_fns: HostedFunctions,        // offset 56 (count: u32 + pad + fns: *[])
};
```

On WASM, function pointers don't exist in linear memory. Instead, functions are referenced
by **table indices** — integers that index into a `funcref` table. The `call_indirect`
instruction takes a table index from the stack and dispatches to the corresponding function.

The current WASM backend already uses this pattern (`WasmCodeGen.zig` lines 214–240):
RocOps functions are added to the function table, and their table indices are stored in
a 36-byte struct in linear memory. But this was designed for the standalone eval mode — it
needs to be formalized for the surgical linking pipeline.

### WASM RocOps Memory Layout (36 bytes on wasm32)

```
Offset  Field                       Size  Contents
──────  ──────────────────────────  ────  ────────────────────────────────
 0      env_ptr                      4    Host environment pointer (i32)
 4      roc_alloc_table_idx          4    Table index for roc_alloc
 8      roc_dealloc_table_idx        4    Table index for roc_dealloc
12      roc_realloc_table_idx        4    Table index for roc_realloc
16      roc_dbg_table_idx            4    Table index for roc_dbg
20      roc_expect_failed_table_idx  4    Table index for roc_expect_failed
24      roc_crashed_table_idx        4    Table index for roc_crashed
28      hosted_fns_count             4    Number of hosted functions
32      hosted_fns_ptr               4    Pointer to table index array in memory
```

This is **NOT** the same layout as the native `RocOps` struct. It is a WASM-specific
encoding where function pointers are replaced by `u32` table indices. This is acceptable
because the builtins compiled to `roc_builtins.o` for wasm32 will use the WASM calling
convention, and the `roc_ops` parameter they receive is already expected to contain
table indices (the `dev_wrappers.zig` functions receive `*RocOps` and call through it).

**Critical detail**: The builtins in `roc_builtins.o` receive a `*RocOps` pointer and call
`roc_ops.roc_alloc(...)` etc. On native targets, these are direct function pointer calls.
On WASM, the builtins `.o` must be compiled with a WASM-specific `RocOps` definition that
stores table indices, and the call sequences must use `call_indirect`. This is handled by
the Zig compiler when targeting wasm32 — function pointers in `extern struct` fields become
table-index-based `call_indirect` calls.

### How the Host Populates RocOps

The host module's entry point receives a pointer to the RocOps struct in linear memory.
The host is responsible for:

1. Writing the `env_ptr` field
2. Importing the RocOps functions (`roc_alloc`, etc.) from the embedding environment
3. Adding them to the function table
4. Writing their table indices into the struct at the correct offsets

In practice, the host's initialization code does something like:

```wasm
;; Store roc_alloc's table index at RocOps offset 4
i32.const 0          ;; RocOps base address
i32.const <table_idx_of_roc_alloc>
i32.store offset=4
```

The surgical linker must ensure that the host's RocOps function imports are in the function
table (element section) so they have valid table indices.

### HostedFunctions Representation

`HostedFunctions.fns` on native targets is a pointer to an array of function pointers.
On WASM, it is a pointer to an array of **`u32` table indices** in linear memory:

```
Memory at hosted_fns_ptr:
  [0]: table_index_of_hosted_fn_0  (u32)
  [4]: table_index_of_hosted_fn_1  (u32)
  [8]: table_index_of_hosted_fn_2  (u32)
  ...
```

The host writes these table indices at initialization time. To call hosted function N:

```wasm
;; Load table index from hosted_fns array
local.get $roc_ops_ptr
i32.load offset=32           ;; Load hosted_fns_ptr
i32.const N
i32.const 4
i32.mul
i32.add
i32.load                     ;; Load table index of hosted fn N
;; Push args: (roc_ops, ret_ptr, args_ptr, table_idx)
;; call_indirect with RocCall type signature
call_indirect (type $roc_call) 0
```

### Two Distinct Indirect Call ABIs

There are **two different `call_indirect` type signatures** in use, and they must not
be conflated:

**1. RocOps core callbacks** — 2-argument ABI: `(i32 args_struct_ptr, i32 env_ptr) → void`

The 6 core RocOps callbacks (`roc_alloc`, `roc_dealloc`, `roc_realloc`, `roc_dbg`,
`roc_expect_failed`, `roc_crashed`) each take a pointer to a **specific args struct**
and the host `env` pointer. This matches `host_abi.zig` lines 88–106 where each field
has signature `fn(*SpecificStruct, *anyopaque) callconv(.c) void`.

```wasm
(type $roc_ops_callback (func (param i32 i32)))  ;; (args_struct_ptr, env_ptr) -> void
```

This is what the current WASM backend registers at `WasmCodeGen.zig` line 215 and what
builtins depend on when calling `roc_ops.alloc()` (`dev_wrappers.zig` line 405). The args
struct is laid out in linear memory (e.g. `RocAlloc` at lines 186–190: alignment, length,
answer), the callback reads inputs and writes its answer into the struct, and the caller
reads the answer back.

Example — `roc_alloc` call:
```wasm
;; Write RocAlloc struct to stack slot
local.get $fp
local.get $alignment
i32.store offset=0              ;; RocAlloc.alignment
local.get $fp
local.get $length
i32.store offset=4              ;; RocAlloc.length

;; Push args: (alloc_struct_ptr, env_ptr)
local.get $fp                   ;; args_struct_ptr
local.get $roc_ops_ptr
i32.load offset=0               ;; env_ptr (RocOps offset 0)

;; Load table index, dispatch
local.get $roc_ops_ptr
i32.load offset=4               ;; roc_alloc table index
call_indirect (type $roc_ops_callback) 0

;; Read result
local.get $fp
i32.load offset=8               ;; RocAlloc.answer
```

**2. Hosted functions** — 3-argument RocCall ABI: `(i32 roc_ops_ptr, i32 ret_ptr, i32 args_ptr) → void`

Hosted functions (platform-provided) use the `RocCall` ABI from `host_abi.zig` line 15.
This is a different signature — 3 arguments, not 2.

```wasm
(type $roc_call (func (param i32 i32 i32)))  ;; (roc_ops, ret_ptr, args_ptr) -> void
```

Both type signatures must be registered in the module's type section, and callers must
use the correct one. Using the wrong type in `call_indirect` will trap at runtime
(WASM validates the type signature of indirect calls).

### Codegen Must Register Both Types

```zig
// Register the 2-arg RocOps callback type: (args_struct_ptr, env_ptr) → void
self.roc_ops_callback_type_idx = try self.module.addFuncType(&.{ .i32, .i32 }, &.{});

// Register the 3-arg RocCall type: (roc_ops, ret_ptr, args_ptr) → void
self.roc_call_type_idx = try self.module.addFuncType(&.{ .i32, .i32, .i32 }, &.{});
```

| Call target | Type signature | Type index field |
|-------------|---------------|-----------------|
| `roc_alloc` | `(i32, i32) → void` | `roc_ops_callback_type_idx` |
| `roc_dealloc` | `(i32, i32) → void` | `roc_ops_callback_type_idx` |
| `roc_realloc` | `(i32, i32) → void` | `roc_ops_callback_type_idx` |
| `roc_dbg` | `(i32, i32) → void` | `roc_ops_callback_type_idx` |
| `roc_expect_failed` | `(i32, i32) → void` | `roc_ops_callback_type_idx` |
| `roc_crashed` | `(i32, i32) → void` | `roc_ops_callback_type_idx` |
| Hosted function N | `(i32, i32, i32) → void` | `roc_call_type_idx` |

### Reference

- `src/builtins/host_abi.zig` lines 88–106: RocOps callback field signatures (2-arg)
- `src/builtins/host_abi.zig` lines 186–242: RocAlloc/RocDealloc/etc. argument structs
- `src/builtins/host_abi.zig` lines 15–30: RocCall signature (3-arg, for hosted fns)
- `src/builtins/host_abi.zig` lines 37–44: HostedFn type definition
- `src/builtins/dev_wrappers.zig` lines 147–162: `alloc()` wrapper calling `self.roc_alloc(&args, self.env)`
- `src/backend/wasm/WasmCodeGen.zig` lines 215–219: current 2-arg type registration
- `src/backend/wasm/WasmCodeGen.zig` lines 4886–4908: current `call_indirect` for roc_alloc

### Tests

```
test "RocOps struct — correct field offsets for wasm32 (36 bytes total)"
test "call_indirect — roc_alloc uses 2-arg callback type, not RocCall type"
test "call_indirect — hosted function uses 3-arg RocCall type"
test "call_indirect — mismatched type index would trap (validate type separation)"
test "function table — all RocOps functions have valid table entries after linking"
test "function table — hosted functions added to table with correct indices"
```

---

## Phase 7a: Entrypoint ABI Migration

### What

Migrate app-exposed functions from the current standalone eval ABI
`(i32 env_ptr) → result_val_type` to the real `RocCall` ABI
`(i32 roc_ops_ptr, i32 ret_ptr, i32 args_ptr) → void`.

### Why

The current WASM backend (`WasmCodeGen.zig` line 376) generates a main function with
signature `(i32 env_ptr) → <result_type>` and synthesizes its own RocOps struct in linear
memory at offset 0 (line 428). This was designed for the standalone eval/REPL mode where
the host is the bytebox test harness.

In the surgical linking model, the **host** owns the RocOps struct and passes it to the app.
The app-exposed function must accept `RocCall` ABI:

```zig
// host_abi.zig line 15
pub const RocCall = fn (*RocOps, *anyopaque, *anyopaque) callconv(.c) void;
```

On wasm32 this becomes:

```wasm
(func $roc__main_for_host_1_exposed (param $roc_ops i32) (param $ret_ptr i32) (param $args_ptr i32))
```

The host module imports this function. After surgical linking, the import is replaced with
the app's defined function. The host calls it with a pointer to its own RocOps struct,
a return buffer, and an arguments buffer.

### Design Decision

**App-exposed functions use RocCall ABI directly.** The host does NOT provide wrappers.

This means:
- The app's main function signature changes from `(i32) → <result>` to `(i32, i32, i32) → void`
- The app reads arguments from `args_ptr` instead of receiving them as direct parameters
- The app writes its return value to `ret_ptr` instead of returning it on the WASM stack
- The app receives `roc_ops_ptr` from the host instead of building its own RocOps struct

### Changes to WasmCodeGen

The `generateModule()` method (current line 376) must be restructured:

**Before** (standalone eval ABI):
```zig
// Create main: (i32 env_ptr) → result_vt
const main_type = try self.module.addFuncType(&.{.i32}, &.{result_vt});
// ... generate body ...
// Return result on stack
```

**After** (RocCall ABI):
```zig
// Create main: (i32 roc_ops_ptr, i32 ret_ptr, i32 args_ptr) → void
const main_type = try self.module.addFuncType(&.{ .i32, .i32, .i32 }, &.{});
// roc_ops_ptr is parameter 0 — store as self.roc_ops_local
// args_ptr is parameter 2 — read arguments from memory at this address
// ... generate body ...
// Write result to ret_ptr (parameter 1) instead of returning on stack
```

**Key changes**:
- `roc_ops_local` comes from parameter 0 (no longer synthesized at offset 0)
- Arguments are loaded from `args_ptr` using `i32.load` with appropriate offsets
- Return value is stored to `ret_ptr` using `i32.store` / `i64.store` / `f64.store`
- No RocOps struct initialization in the app — the host provides it

### Backward Compatibility for Eval

The eval pipeline can either:
1. Build a tiny host shim (precompiled) that creates a RocOps struct and calls the app
   with RocCall ABI — this is the cleanest approach.
2. Keep a separate `generateModuleForEval()` path with the old ABI — pragmatic but
   maintains two code paths.

Option 1 is preferred. The eval host shim would be ~50 lines of WAT that:
- Imports `roc_alloc`, `roc_dealloc`, etc. from the embedding environment
- Builds a RocOps struct in linear memory
- Calls the app's main function with `(roc_ops_ptr, ret_ptr, args_ptr)`
- Returns the result to the embedder

### Rust Reference

- `src/builtins/host_abi.zig` lines 15–30: `RocCall` function signature
- `src/backend/wasm/WasmCodeGen.zig` lines 376–452: current main function generation
- `src/backend/dev/LirCodeGen.zig` lines 4145–4241: dev backend hosted call lowering
  (shows the same `(roc_ops, ret_ptr, args_ptr)` pattern)

### Tests

```
test "app entrypoint — exports both roc__main_for_host_1_exposed and main"
test "app entrypoint — roc__main_for_host_1_exposed has RocCall type (i32, i32, i32) → void"
test "app entrypoint — main (eval wrapper) returns a value, not void"
test "app entrypoint — roc__main and main are distinct functions"
```

Structural tests parse the encoded wasm bytes with `WasmModule.preload` — no bytebox
execution needed. Behavioral correctness is validated by 1289 eval tests that exercise
the eval wrapper end-to-end. Full integration tests (calling `roc__main_for_host_1_exposed`
from a surgically-linked host) will be added in Phase 12.

### Implementation Notes (Done)

`generateModule()` now produces two exported functions in the same module:

1. **`<entrypoint_name>`** — RocCall ABI `(i32, i32, i32) → void`.
   The export name is passed as a parameter to `generateModule()`, derived from the
   platform's `provides` section (e.g. `roc__main_for_host_1_exposed`). Parameter 0
   is `roc_ops_ptr` (provided by host, not synthesized). The expression body runs and
   the result is written to `ret_ptr` via `emitStoreResultToRetPtr()`, which handles
   both primitives (scalar store) and composites (unrolled 4-byte copy).

2. **`main`** — Eval wrapper `(i32 env_ptr) → result_vt`. Built by `emitEvalWrapper()`.
   Allocates its own stack frame for a return buffer, constructs the RocOps struct at
   memory offset 0 (same as before), calls the RocCall function via `call`, loads the
   result from the return buffer, and returns it on the wasm stack.

Helper methods extracted: `emitStackPrologue()`, `emitStackEpilogue()`.

---

## Phase 7b: CodeBuilder & WasmCodeGen Refactor

### What

Introduce a `CodeBuilder` pattern for accumulating function bodies, and refactor
`WasmCodeGen` to append completed functions into the parsed host module via an explicit
`insertIntoModule()` step.

### Why

The previous plan (v1) naively suggested recording relocation offsets during instruction
emission (`emitRelocatableCall` at the point of each `call` instruction). This is wrong.
The final byte offset of a relocation within the code section depends on:

1. **Where the function lands in the code section** — the function's base offset within
   `code_bytes`, which isn't known until insertion time
2. **The function body length prefix** — a LEB128-encoded size prepended to each function
   body, whose byte length varies
3. **The locals declaration preamble** — encoded before the instruction bytes
4. **Deferred insertions** — the Rust code builder uses an insertion mechanism where chunks
   of code are generated out-of-order and spliced in during finalization

The old Rust compiler solved this with a `CodeBuilder` (`crates/compiler/gen_wasm/src/code_builder.rs`
lines 48–76) that tracks relocations as **(code_position, function_index)** pairs relative
to the function's own instruction stream. Only during `insert_into_module()` (lines 211–256)
are these converted to absolute code section offsets:

```rust
// Rust: offset computation at insertion time
let offset = reloc_code_pos + code_offset + insertion_bytes;
```

We must follow the same pattern.

### Design

**CodeBuilder struct** — accumulates one function's body:

```zig
const CodeBuilder = struct {
    /// Main instruction bytes for the current function.
    code: std.ArrayList(u8),
    /// Locals declaration preamble (local count groups + types).
    preamble: std.ArrayList(u8),
    /// Relocations within this function: (code_pos, symbol_index).
    /// code_pos is relative to the start of self.code, NOT the module's code section.
    import_relocations: std.ArrayList(struct { code_pos: u32, symbol_index: u32 }),

    /// Emit a relocatable call instruction.
    /// Records the relocation position relative to this function's code buffer.
    pub fn emitRelocatableCall(self: *CodeBuilder, symbol_idx: u32) !void {
        try self.code.append(Op.call);
        const code_pos: u32 = @intCast(self.code.items.len);
        try self.import_relocations.append(.{
            .code_pos = code_pos,
            .symbol_index = symbol_idx,
        });
        try appendPaddedU32(&self.code, 0); // 5-byte placeholder
    }

    /// Finalize this function and insert it into the module's code section.
    /// Computes final relocation offsets based on actual position in code_bytes.
    pub fn insertIntoModule(self: *const CodeBuilder, module: *WasmModule) !u32 {
        const fn_offset: u32 = @intCast(module.code_bytes.items.len);
        module.function_offsets.append(fn_offset);

        // Encode body length (LEB128)
        const body_len = self.preamble.items.len + self.code.items.len;
        try leb128WriteU32(&module.code_bytes, @intCast(body_len));

        // Append preamble (locals declaration)
        try module.code_bytes.appendSlice(self.preamble.items);

        // Record the code start offset (after length prefix + preamble)
        const code_start: u32 = @intCast(module.code_bytes.items.len);

        // Append instruction bytes
        try module.code_bytes.appendSlice(self.code.items);

        // Create relocation entries with absolute offsets
        for (self.import_relocations.items) |reloc| {
            try module.reloc_code.entries.append(.{ .index = .{
                .type_id = .function_index_leb,
                .offset = code_start + reloc.code_pos,
                .symbol_index = reloc.symbol_index,
            }});
        }

        // Compute and return the global function index
        const func_idx = module.import_fn_count +
                         module.dead_import_dummy_count +
                         @as(u32, @intCast(module.func_type_indices.items.len));
        return func_idx;
    }

    pub fn clear(self: *CodeBuilder) void {
        self.code.clearRetainingCapacity();
        self.preamble.clearRetainingCapacity();
        self.import_relocations.clearRetainingCapacity();
    }
};
```

**WasmCodeGen changes**:

Replace the current `body: std.ArrayList(u8)` with a `CodeBuilder`. The codegen emits
instructions into `self.code_builder.code`. After each function is complete:

```zig
// After generating a procedure body:
fn finalizeProcedure(self: *Self) !void {
    // Build preamble (locals declaration)
    try self.code_builder.encodePreamble(self.storage.local_types.items);
    // Insert completed function into module
    const func_idx = try self.code_builder.insertIntoModule(self.module);
    // Add to function section
    try self.module.func_type_indices.append(type_idx);
    // Add symbol
    try self.module.linking.symbol_table.append(.{ ... });
    // Clear for next function
    self.code_builder.clear();
    self.storage.reset();
}
```

This ensures relocation offsets are **always** computed at insertion time, never during
instruction emission.

### Rust Reference

- `crates/compiler/gen_wasm/src/code_builder.rs` lines 48–76: `CodeBuilder` struct definition
- `crates/compiler/gen_wasm/src/code_builder.rs` lines 211–256: `insert_into_module()` with
  relocation offset computation: `offset = reloc_code_pos + code_offset + insertion_bytes`
- `crates/compiler/gen_wasm/src/code_builder.rs` lines 348–349: `call_import()` recording
  relocation as `(code_pos, imported_fn_id)` pair
- `crates/compiler/gen_wasm/src/backend.rs` lines 390–396: `reset()` calls
  `self.code_builder.insert_into_module(&mut self.module)` after each procedure

### Tests

```
test "CodeBuilder.insertIntoModule — relocation offset accounts for body length prefix"
test "CodeBuilder.insertIntoModule — relocation offset accounts for preamble size"
test "CodeBuilder.insertIntoModule — multiple relocations in one function"
test "CodeBuilder.insertIntoModule — function appended at correct code_bytes position"
test "CodeBuilder — two functions inserted sequentially have non-overlapping offsets"
test "CodeBuilder.emitRelocatableCall — records code_pos relative to function start"
test "CodeBuilder — clear resets state for next function without leaking relocations"
```

---

## Phase 8: Builtins Migration — Symbol Mapping, ABI Rewrites, Import Removal

### What

Parse and merge `roc_builtins.o` into the host module, rewrite all builtin call sites
in WasmCodeGen to use the correct symbol names and decomposed C ABI, and verify that
no legacy `env.roc_*` builtin imports survive in the final module.

### Why

This phase addresses three intertwined problems:

1. **The current backend registers ~40 builtin imports** (`WasmCodeGen.zig` lines 182–308)
   with names like `roc_str_trim`, `roc_dec_mul`, etc. These are imported from the `"env"`
   module namespace and implemented by the host (bytebox test harness).

2. **The real builtins in `roc_builtins.o` export different symbol names** with different
   ABIs. For example:
   - Current import: `roc_str_trim(str_ptr: i32, result_ptr: i32) → void` (pointer-to-struct)
   - Real builtin export: `roc_builtins_str_trim(out: *RocStr, bytes: ?[*]u8, len: usize, cap: usize, roc_ops: *RocOps) → void` (decomposed C ABI)

3. **If we merge `roc_builtins.o` without rewriting call sites**, the app code would still
   emit `call roc_str_trim` with the old ABI, but the module would contain
   `roc_builtins_str_trim` with the new ABI — symbol mismatch and ABI mismatch.

All three must be addressed together. The merge alone is not enough.

### Sub-phase 8a: Merge `roc_builtins.o`

Parse `roc_builtins.o` as a relocatable WASM module and merge it into the host module.
This is the same merge algorithm described in the previous plan version:

```
1. MERGE TYPE SECTIONS (with deduplication)
2. MERGE FUNCTION SECTIONS (remap type indices)
3. MERGE CODE SECTION (append bytes, track offsets)
4. MERGE DATA SECTION (adjust memory offsets)
5. MERGE SYMBOL TABLE (resolve shared symbols like roc_alloc)
6. MERGE RELOCATION ENTRIES (remap offsets and symbol indices)
```

After merging, the module contains the builtin function bodies and their symbols
(e.g. `roc_builtins_str_trim` as a defined function symbol).

### Sub-phase 8b: Symbol Name Mapping

Build a lookup table mapping the builtin operation to its symbol index in the merged module:

```zig
const BuiltinSymbols = struct {
    // Decimal/math
    dec_mul: u32,          // → "roc_builtins_dec_mul_saturated"
    dec_div: u32,          // → "roc_builtins_dec_div"
    dec_div_trunc: u32,    // → "roc_builtins_dec_div_trunc"
    dec_to_str: u32,       // → "roc_builtins_dec_to_str"
    // String operations
    str_eq: u32,           // → "roc_builtins_str_equal"
    str_concat: u32,       // → "roc_builtins_str_concat"
    str_trim: u32,         // → "roc_builtins_str_trim"
    str_trim_start: u32,   // → "roc_builtins_str_trim_start"
    str_trim_end: u32,     // → "roc_builtins_str_trim_end"
    // ... etc for all ~40 builtins
};
```

Populated after merge by looking up each symbol name in the merged module's symbol table:
```zig
builtin_syms.str_trim = module.linking.findSymbolByName("roc_builtins_str_trim") orelse
    return error.MissingBuiltinSymbol;
```

The exact symbol names come from `src/builtins/static_lib.zig` lines 28–112
and `src/builtins/dev_wrappers.zig`.

### Sub-phase 8c: ABI Rewrite at Call Sites

Every place WasmCodeGen currently emits a host import call must be rewritten to:
1. Use the builtin symbol index (not an import function index)
2. Decompose struct arguments into individual fields
3. Pass `roc_ops_ptr` as the last argument (where required)
4. Use `emitRelocatableCall()` (via CodeBuilder) instead of direct `call` instruction

**ABI categories** (from `src/builtins/dev_wrappers.zig`):

| Category | Old ABI (pointer-to-struct) | New ABI (decomposed C) | Examples |
|----------|---------------------------|----------------------|----------|
| **Str unary** | `(str_ptr, result_ptr)` | `(result_ptr, bytes, len, cap, roc_ops)` | trim, trim_start, trim_end, lowercased, uppercased |
| **Str binary** | `(a_ptr, b_ptr, result_ptr)` | `(result_ptr, a_bytes, a_len, a_cap, b_bytes, b_len, b_cap, roc_ops)` | concat, drop_prefix, split, join_with |
| **Str equality** | `(a_ptr, b_ptr) → i32` | `(a_bytes, a_len, a_cap, b_bytes, b_len, b_cap) → i32` | str_eq, caseless_ascii_equals |
| **i128 binop** | `(lhs_ptr, rhs_ptr, result_ptr)` | `(result_ptr, lhs_lo, lhs_hi, rhs_lo, rhs_hi)` | i128_div_s, u128_div, dec_mul |
| **Dec to str** | `(dec_ptr, buf_ptr) → i32` | `(result: *RocStr, lo: u64, hi: u64, roc_ops: *RocOps)` | dec_to_str |
| **List ops** | `(list_ptr, ...)` | `(result_ptr, bytes, len, cap, ..., roc_ops)` | list_append, list_reverse |
| **Callback-bearing** | `(list_ptr, cmp_fn, cmp_data, ...)` | `(result_ptr, bytes, len, cap, cmp_fn_ptr, cmp_data, alignment, elem_width, roc_ops)` | list_sort_with |

**Example rewrite** — `str_trim`:

```zig
// BEFORE (old ABI):
// str_ptr and result_ptr are i32 pointers to 12-byte RocStr structs in memory
try self.emitCall(self.str_trim_import.?, &.{str_ptr, result_ptr});

// AFTER (decomposed C ABI):
// Decompose the RocStr at str_ptr into (bytes, len, cap)
try self.emitLocalGet(result_ptr_local);  // arg 0: output pointer
try self.emitI32Load(str_ptr_local, 0);   // arg 1: bytes (offset 0)
try self.emitI32Load(str_ptr_local, 4);   // arg 2: length (offset 4)
try self.emitI32Load(str_ptr_local, 8);   // arg 3: capacity (offset 8)
try self.emitLocalGet(self.roc_ops_local); // arg 4: roc_ops pointer
try self.code_builder.emitRelocatableCall(self.builtin_syms.str_trim);
```

**Callback-bearing builtins** require special treatment. `list_sort_with` is the primary
example. Its wrapper signature (`dev_wrappers.zig` lines 382–424):

```zig
pub fn roc_builtins_list_sort_with(
    out: *RocList,
    list_bytes: ?[*]u8, list_len: usize, list_cap: usize,
    cmp_fn_ptr: ?*const anyopaque,  // comparator function pointer
    cmp_data: ?[*]u8,               // comparator closure data
    alignment: u32,
    element_width: usize,
    roc_ops: *RocOps,
) callconv(.c) void
```

On WASM, function pointers don't exist in linear memory. The `cmp_fn_ptr` parameter must
be a **function table index** (a `u32` that indexes into the `funcref` table). The current
WASM backend already handles this: `WasmCodeGen.zig` line 11994 compiles the comparator
as a separate procedure and passes its function index. The dev backend does the equivalent
via PC-relative addressing (`LirCodeGen.zig` line 3366).

The call-site rewrite for `list_sort_with` must:
1. Compile the comparator procedure and get its `func_idx` (already done by `compileAllProcSpecs`)
2. Pass `func_idx` as `cmp_fn_ptr` (it will be interpreted as a table index by the builtin)
3. Pass the closure data pointer as `cmp_data`
4. Decompose the list struct and pass remaining args in the new ABI

Any future callback-bearing builtins (e.g. `list_map`, `list_keep_if`) will follow the
same pattern: compile the callback as a proc, pass its table index.

### Sub-phase 8d: Remove Legacy Imports

After all call sites are rewritten:

1. **Remove** the ~40 `?u32` import fields from `WasmCodeGen` (`dec_mul_import`, `str_trim_import`, etc.)
2. **Remove** their registration in `registerHostImports()` — only keep the 6 RocOps imports
3. **Remove** the ~45 host function implementations from `src/eval/test/helpers.zig`
   (`hostDecMul`, `hostStrTrim`, `hostListEq`, etc.)
4. **Remove** corresponding implementations from `src/repl/wasm_runner.zig`

### Sub-phase 8e: Verification

Add a verification pass that runs after surgical linking and before serialization:

```zig
/// Verify that no builtin roc_* imports remain in the final module.
/// Only the 6 RocOps functions should be imported.
fn verifyNoBuiltinImports(module: *const WasmModule) !void {
    const allowed = [_][]const u8{
        "roc_alloc", "roc_dealloc", "roc_realloc",
        "roc_dbg", "roc_expect_failed", "roc_crashed",
    };
    for (module.imports.items) |imp| {
        if (imp.kind != .function) continue;
        var is_allowed = false;
        for (allowed) |name| {
            if (std.mem.eql(u8, imp.field_name, name)) { is_allowed = true; break; }
        }
        if (!is_allowed and std.mem.startsWith(u8, imp.field_name, "roc_")) {
            return error.UnresolvedBuiltinImport;
        }
    }
}
```

### Rust Reference

- `src/builtins/static_lib.zig` lines 28–112: exported builtin symbol names
- `src/builtins/dev_wrappers.zig` line 134: `roc_builtins_str_trim` signature
- `src/builtins/dev_wrappers.zig` line 950: `roc_builtins_dec_mul_saturated` signature
- `src/backend/wasm/WasmCodeGen.zig` lines 182–308: current import registrations to remove
- `crates/wasm_module/src/lib.rs` lines 267–393: `eliminate_dead_code()` — index remapping
  patterns reusable for merge
- `crates/compiler/gen_wasm/src/code_builder.rs` lines 211–256: `insert_into_module()` —
  appending code with correct relocation offsets

### Tests

```
test "mergeModule — type deduplication: identical signatures share index"
test "mergeModule — function indices remapped correctly"
test "mergeModule — code bytes appended at correct offset"
test "mergeModule — undefined symbol in builtins resolved to host's roc_alloc import"
test "mergeModule — relocation offsets shifted by base_code_offset"
test "BuiltinSymbols — all symbols found after merge"
test "str_trim ABI — decomposed args match roc_builtins_str_trim signature"
test "dec_mul ABI — decomposed i128 args match roc_builtins_dec_mul_saturated"
test "str_eq ABI — returns i32 with decomposed RocStr args"
test "verifyNoBuiltinImports — passes when only RocOps imports remain"
test "verifyNoBuiltinImports — fails if roc_str_trim import still present"
test "end-to-end: app using Str.trim produces correct output via builtins"
```

---

## Phase 9: Hosted Call Lowering

### What

Implement `hosted_call` expression lowering in WasmCodeGen so that app code can call
platform-provided hosted functions via the `RocOps.hosted_fns` array.

### Why

The WASM backend currently panics on `.hosted_call` expressions (`WasmCodeGen.zig` line 1016)
and hardcodes `hosted_fns` count to zero (`WasmCodeGen.zig` line 449). Hosted functions are
how platforms expose capabilities to Roc apps (e.g. HTTP requests, file I/O, rendering).
Without this, no real platform can work.

The runtime contract already exists:
- Hosted functions are indexed during build (`src/compile/compile_build.zig` line 721)
- The dev backend lowers them by marshalling args and calling
  `roc_ops.hosted_fns.fns[hc.index]` (`src/backend/dev/LirCodeGen.zig` line 4146)
- All hosted functions follow the `RocCall` ABI: `fn(roc_ops, ret_ptr, args_ptr) → void`

### WASM Implementation

Given a `hosted_call` expression with `index`, `args`, and `ret_layout`:

```zig
fn generateHostedCall(self: *Self, hc: HostedCallInfo) !void {
    // 1. Allocate return slot on stack frame
    const ret_size = self.layout_store.layoutSize(hc.ret_layout);
    const ret_slot = self.allocStackSlot(@max(ret_size, 4));

    // 2. Marshal arguments into contiguous buffer on stack
    var total_args_size: u32 = 0;
    for (hc.args) |arg| {
        const arg_size = self.layout_store.layoutSize(arg.layout);
        const arg_align = self.layout_store.layoutAlign(arg.layout);
        total_args_size = std.mem.alignForward(u32, total_args_size, arg_align);
        total_args_size += arg_size;
    }
    const args_slot = self.allocStackSlot(@max(total_args_size, 4));

    // Copy each argument into the args buffer
    var offset: u32 = 0;
    for (hc.args) |arg| {
        const arg_size = self.layout_store.layoutSize(arg.layout);
        const arg_align = self.layout_store.layoutAlign(arg.layout);
        offset = std.mem.alignForward(u32, offset, arg_align);
        self.copyToStackSlot(arg.loc, args_slot + offset, arg_size);
        offset += arg_size;
    }

    // 3. Load hosted function's table index from RocOps struct:
    //    roc_ops_ptr → load hosted_fns_ptr at offset 32
    //                → load table_index at hosted_fns_ptr + (hc.index * 4)
    self.emitLocalGet(self.roc_ops_local);
    self.emitI32Load(32);               // hosted_fns_ptr
    self.emitI32Const(hc.index * 4);
    self.emitOp(.i32_add);
    self.emitI32Load(0);                // table index of hosted fn

    // 4. Push RocCall args: (roc_ops_ptr, ret_ptr, args_ptr)
    //    Note: table index is already on stack from step 3, but call_indirect
    //    expects it LAST. So we need to push args first, then the table index.
    //    Reorder: save table index to temp local, push args, reload table index.
    const table_idx_local = self.storage.allocAnonymousLocal(.i32);
    self.emitLocalSet(table_idx_local);

    self.emitLocalGet(self.roc_ops_local);        // arg 0: roc_ops
    self.emitFpOffset(ret_slot);                   // arg 1: ret_ptr
    self.emitFpOffset(args_slot);                  // arg 2: args_ptr

    self.emitLocalGet(table_idx_local);            // table index (consumed by call_indirect)

    // 5. call_indirect with RocCall type signature, table 0
    self.emitOp(.call_indirect);
    self.emitLeb128U32(self.roc_call_type_idx);   // type index
    self.emitLeb128U32(0);                         // table index

    // 6. Result is now at ret_slot — load it according to ret_layout
    self.loadFromStackSlot(ret_slot, hc.ret_layout);
}
```

### Integration with Build System

The `hosted_fns_count` in the RocOps struct must be set to the actual count of hosted
functions discovered during compilation (`compile_build.zig` line 721). The host is
responsible for populating the `hosted_fns_ptr` array in linear memory with the correct
table indices before calling the app's entry point.

The `roc_call_type_idx` must be registered during codegen initialization:
```zig
// Register the RocCall function type: (i32, i32, i32) → void
self.roc_call_type_idx = try self.module.addFuncType(&.{ .i32, .i32, .i32 }, &.{});
```

### Rust Reference

- `src/backend/dev/LirCodeGen.zig` lines 4145–4241: dev backend `generateHostedCall()`
  — the native implementation we're porting to WASM
- `src/backend/dev/LirCodeGen.zig` lines 4201–4218: loading function pointer from
  `RocOps.hosted_fns.fns[index]`
- `src/compile/compile_build.zig` lines 721–739: hosted function indexing
- `src/builtins/host_abi.zig` lines 37–44: `HostedFn` type definition
- `src/builtins/host_abi.zig` lines 67–84: `HostedFunctions` struct

### Tests

```
test "hosted_call — marshals arguments into contiguous stack buffer"
test "hosted_call — loads table index from RocOps.hosted_fns at correct offset"
test "hosted_call — emits call_indirect with RocCall type signature"
test "hosted_call — reads return value from ret_ptr after call"
test "hosted_call — handles zero-sized return type (no read from ret_ptr)"
test "hosted_call — multiple hosted calls use correct indices (0, 1, 2, ...)"
test "hosted_call — argument alignment respected in args buffer"
test "end-to-end: app calling hosted function through platform"
```

---

## Phase 10: Dead Code Elimination

### What

After surgical linking, trace the call graph from exported/live functions and replace
unreachable function bodies with `unreachable; end` stubs.

### Why

The host module may contain functions that are never called by the app (e.g. host helper
functions for features the app doesn't use). Similarly, the merged builtins module contains
all Roc builtins, but the app may only use a subset. Dead code elimination reduces the
final `.wasm` size by stubbing out unreachable functions.

Importantly, we do NOT remove dead functions — we replace their bodies with 3-byte stubs.
This preserves all function indices so no relocation updates are needed. The WASM runtime
will trap if a dead function is somehow called (which indicates a bug in the DCE).

### Algorithm

```
1. INITIALIZE live set:
   - Mark all exported functions as live.
   - Mark all functions referenced in element sections (indirect call targets) as live.
   - Mark all init functions as live.

2. TRACE call graph (iterate until stable):
   For each newly-live function:
       Find its byte range in code_bytes via function_offsets.
       Find all relocation entries within that byte range.
       For each relocation of type function_index_leb:
           Look up the symbol → get target function index.
           Mark target function as live.
       For each relocation of type type_index_leb (call_indirect):
           Mark all functions with matching type signature as potentially live.
           (Conservative: indirect calls could target any function of that type.)

3. ELIMINATE dead imports:
   Iterate imports, but only over the FUNCTION-import index space:
       If a function import is not live:
           Remove it from imports array.
           Increment dead_import_dummy_count.
       If an import is memory/table/global:
           Keep it. The live set is indexed by function index only.
   Reindex remaining imported-function symbols and patch their relocations
   before mutating code_bytes.

4. ELIMINATE dead defined functions:
   Rebuild the code section body:
       For each defined function:
           If live: copy its serialized body bytes verbatim.
           If dead: serialize DUMMY_FUNCTION instead.
   (No attempt to preserve original per-function byte lengths is required
    after relocation-based import reindexing is complete.)
```

### Rust Reference

- `crates/wasm_module/src/lib.rs` lines 267–393: `eliminate_dead_code()` — the complete implementation
- `crates/wasm_module/src/lib.rs` lines 395–503: `trace_live_functions()` — call graph tracing
- `crates/wasm_module/src/lib.rs` lines 276: `fn_index_min` calculation using `dead_import_dummy_count`
- `crates/wasm_module/src/lib.rs` lines 293–328: dead import elimination with reindexing
- `crates/wasm_module/src/lib.rs` lines 330–391: dead code body replacement

### Tests

```
test "eliminateDeadCode — exported function and its callees are preserved"
test "eliminateDeadCode — unreachable function body replaced with unreachable stub"
test "eliminateDeadCode — function indices unchanged after elimination"
test "eliminateDeadCode — dead import removed, dead_import_dummy_count incremented"
test "eliminateDeadCode — non-function imports are preserved"
test "eliminateDeadCode — indirect call targets (element section) preserved"
test "eliminateDeadCode — transitive callees preserved (A calls B calls C → all live)"
test "eliminateDeadCode — init functions preserved"
test "eliminateDeadCode — call_indirect conservatively keeps matching-signature functions"
```

---

## Phase 11: Serialization Updates ✅

### What

Extend the existing `encode()` method to handle the new fields from surgical linking:
raw code bytes, dummy functions, and stripping of linking metadata.

### Why

After surgical linking, the module contains:
- `code_bytes` with patched relocation values (not the old `func_bodies` array)
- `dead_import_dummy_count` dummy functions that must be prepended
- Linking and relocation sections that should be stripped from the final output
  (they were only needed during the linking process)

The serializer must produce a valid, standard WASM module (no custom linking sections).

### Changes to `encode()`

The code section serialization becomes:

```zig
fn encodeCodeSection(self: *const WasmModule, output: *std.ArrayList(u8)) !void {
    // Section header
    try output.append(10); // section ID: Code
    const size_offset = output.items.len;
    try appendPaddedU32(output, 0); // placeholder for section size

    // Function count = dummies + real functions
    const total = self.dead_import_dummy_count + @as(u32, @intCast(self.function_offsets.items.len));
    try leb128WriteU32(output, total);

    // Prepend dummy functions
    for (0..self.dead_import_dummy_count) |_| {
        try leb128WriteU32(output, DUMMY_FUNCTION.len); // body size
        try output.appendSlice(&DUMMY_FUNCTION);
    }

    // Append real function bodies from raw code_bytes
    // (starting from the first function's offset)
    if (self.function_offsets.items.len > 0) {
        const first = self.function_offsets.items[0];
        try output.appendSlice(self.code_bytes.items[first..]);
    }

    // Patch section size
    const section_size = output.items.len - size_offset - 5;
    overwritePaddedU32(output.items, @intCast(size_offset), @intCast(section_size));
}
```

**Important**: The linking section, reloc.CODE, and reloc.DATA sections are NOT serialized
into the final output. They are only used during the surgical linking process. The final
`.wasm` is a standard module that any WASM runtime can execute.

### Rust Reference

- `crates/wasm_module/src/sections.rs` lines 1451–1468: code section serialization with
  `dead_import_dummy_count` prepending
- `crates/wasm_module/src/lib.rs` lines 80–99: `serialize()` — section ordering and
  selective serialization (skips empty sections)

### Tests

```
test "encode — dummy functions prepended before real functions in code section"
test "encode — code section function count includes dummies"
test "encode — linking section NOT present in output"
test "encode — reloc.CODE section NOT present in output"
test "encode — output is valid WASM (magic, version, section ordering)"
```

---

## Phase 12: CLI Integration — `roc build --target=wasm32` ✅

### What

Wire the surgical linking pipeline into the `roc build` command for the `wasm32` target.

### Status: Complete

The wasm32 target is now enabled in `src/cli/main.zig`. The full surgical linking
pipeline runs when `--target=wasm32` is specified, producing a standalone `.wasm` binary
without invoking `wasm-ld`.

### Implementation

#### Files Changed

- **`src/backend/wasm/WasmModule.zig`**
  - `findImportFuncIdx(module_name, field_name) → ?u32` — find an import by name
  - `BuiltinSymbols.populate()` — **fixed** to return actual function indices
    (`sym.index`) instead of symbol table indices
  - `transferAppFunctions()` — copies app function bodies from `func_bodies` into
    `code_bytes`/`function_offsets` so they're compatible with surgical linking,
    relocation resolution, and dead code elimination

- **`src/backend/wasm/WasmCodeGen.zig`**
  - `initWithHostModule(allocator, store, layout_store, host_module, builtin_syms)` —
    init variant that takes ownership of a preloaded host `WasmModule`
  - `registerRocOpsFromModule()` — finds existing `roc_alloc`/`roc_dealloc`/etc.
    imports in the host module (after builtins merge) and registers them in the
    funcref table for `call_indirect`
  - `generateEntrypointWrapper(proc, name, arg_layouts, ret_layout) → u32` —
    generates a RocCall ABI wrapper `(i32 roc_ops_ptr, i32 ret_ptr, i32 args_ptr) → void`
    that reads args from `args_ptr`, calls the compiled proc, stores the result to
    `ret_ptr`, and exports the function by name

- **`src/cli/main.zig`**
  - Removed the wasm32 error block — wasm32 is now supported alongside x86_64/aarch64
  - Added `target_usize` selection: uses `TargetUsize.u32` for wasm32 layouts
  - Added the complete wasm32 surgical linking pipeline as a branch after LIR lowering

#### Build Pipeline (as implemented)

```
 1. Get host.wasm path from platform's TargetsConfig link spec
    (first file_path entry before `app` in the wasm32 exe spec)

 2. Read and parse host module
    host_module = WasmModule.preload(allocator, host_bytes, true)

 3. Remove memory and table imports
    host_module.removeMemoryAndTableImports()

 4. Parse and merge builtins
    builtins_module = WasmModule.preload(allocator, BuiltinsObjects.forTarget(.wasm32), true)
    host_module.mergeModule(&builtins_module)

 5. Build BuiltinSymbols lookup
    builtin_syms = BuiltinSymbols.populate(&host_module)

 6. Create code generator with host module
    codegen = WasmCodeGen.initWithHostModule(allocator, &lir_store, &layout_store, host_module, builtin_syms)

 7. Register RocOps callbacks from existing module imports
    codegen.registerRocOpsFromModule()

 8. Compile all LIR proc specs
    codegen.compileAllProcSpecs(procs)

 9. Generate entrypoint wrappers + build host-to-app map
    for each entrypoint:
        wrapper_idx = codegen.generateEntrypointWrapper(proc, name, arg_layouts, ret_layout)
        host_to_app_map.append({ name, wrapper_idx })

10. Transfer app functions from func_bodies → code_bytes
    codegen.module.transferAppFunctions()

11. Surgical linking — redirect host imports to app functions
    codegen.module.linkHostToAppCalls(host_to_app_map)

12. Resolve code relocations (patches builtin call sites)
    codegen.module.resolveCodeRelocations()

13. Finalize memory and table (1MB stack)
    codegen.module.finalizeMemoryAndTable(1024 * 1024)

14. Verify no stale builtin imports
    codegen.module.verifyNoBuiltinImports()

15. Dead code elimination
    codegen.module.eliminateDeadCode(called_fns)

16. Materialize function bodies from code_bytes
    codegen.module.materializeFuncBodies()

17. Encode and write output
    final_bytes = codegen.module.encode(allocator)
    writeFile(final_output_path, final_bytes)
```

#### Key Design Decisions

- **Two-representation bridge**: The preloaded host module uses `code_bytes`/`function_offsets`
  while the codegen uses `func_bodies`/`setFunctionBody`. The `transferAppFunctions()` method
  bridges these by copying app bodies into `code_bytes` before surgical linking. After DCE,
  `materializeFuncBodies()` converts everything back to `func_bodies` for encoding.

- **BuiltinSymbols fix**: `populate()` was returning symbol table indices but the codegen
  used them as function indices in `call` instructions. Fixed to return `sym.index` (the
  actual function index) from the symbol table entry.

- **Canonical RocOps callbacks**: Eval-style modules reuse existing `roc_alloc`/`roc_dbg`/
  etc. imports, while host modules bind their locally-defined RocOps callback functions
  (the implementations assigned into `host_abi.RocOps`) into the funcref table without
  mutating the import section.

- **Entrypoint wrappers**: Each entrypoint is compiled as a regular LIR proc via
  `compileAllProcSpecs`. A thin RocCall wrapper reads args from `args_ptr`, calls the
  compiled proc, and stores the result to `ret_ptr`. The wrapper (not the proc) is
  exported and linked.

### Platform Configuration

Platforms declare their WASM host via the existing `TargetsConfig` system in
`src/compile/targets_config.zig`. The CLI target selection prefers `exe` over `static_lib`,
so the host should be declared under `exe`:

```roc
targets: {
    files: "targets/",
    exe: {
        wasm32: ["host.wasm", app],
    }
}
```

The `host.wasm` file must be a relocatable WASM object (compiled with
`clang --target=wasm32 -c -o host.wasm host.c` or Zig's `--target=wasm32-freestanding`).
It must contain `linking` and `reloc.*` custom sections.

### Remaining Work (not blocking Phase 12)

- **Broader verification**: The surgical linking pipeline is wired into the CLI and eval
  path now, but the full wasm verification story still needs to be hardened:
  - rerun the full eval/REPL test suite after the latest linker fixes and update the
    recorded pass/fail status
  - make the end-to-end wasm build/test harness self-contained in CI (`test-backend`,
    `test-wasm-static-lib`, bytebox/runner integration)
  - add at least one true end-to-end test that compiles a Roc program via
    `roc build --target=wasm32` and executes the emitted `.wasm`

- **Hosted effects coverage**: `generateHostedCall()` exists and the RocOps layout has
  slots for hosted functions, but the current wasm test platform still uses
  `hosted_fns.count = 0`. We need one wasm host/platform test that exposes a non-empty
  hosted-function table and proves `call_indirect` through `host_abi.RocOps.hosted_fns`
  works end-to-end.

### Rust Reference

- `crates/compiler/gen_wasm/src/lib.rs` lines 55–60: `parse_host()`
- `crates/compiler/gen_wasm/src/lib.rs` lines 80–188: `build_app_module()`
- `crates/compiler/gen_wasm/src/backend.rs` lines 79–114: `WasmBackend::new()`
- `crates/compiler/gen_wasm/src/backend.rs` lines 296–304: `finalize()`

---

## Phase 13: PIC Support & Eval Builtins — Done

### What

Support Position Independent Code (PIC) WASM modules in the surgical linker, and
wire the eval/REPL pipeline to merge real compiled builtins instead of using host
import reimplementations.

### Status

Implementation is in place:
- PIC imports/globals/tables are handled during preload/merge
- eval now merges real `roc_builtins.o` and resolves relocations
- `reloc.DATA` is normalized, merged, and resolved before encoding
- wasm host RocOps callbacks use canonical `host_abi` callback symbols instead of
  late import insertion

The remaining work is now primarily broader verification and end-to-end coverage,
captured below in Appendix C.

### Root Causes Fixed

**Relocation offset mismatch**: `reloc.CODE` offsets are relative to the code section
body (which includes a function-count LEB128 prefix), but `code_bytes` starts after
that prefix. Fix: record the function count's LEB128 byte size during
`parseCodeSection` and subtract it from all `reloc_code` entry offsets after parsing.

**type_index_leb resolution**: `R_WASM_TYPE_INDEX_LEB` relocations (for `call_indirect`)
need to remap placeholder type indices from the source type section to the merged type
section. The original code used `sym.index` (function index) which is wrong. Fix:
resolve `type_index_leb` during `mergeModule` when `type_remap` is available.

**Builtin ABI mismatch**: The merged `roc_builtins_*` functions use the wasm32 native
ABI (ptr/len/cap decomposition, split i128 args, sret result slots), but the codegen
was passing pointer-to-struct arguments. Fix: added shared helpers for the merged
builtins' ABI and migrated all call sites (string/list equality, string transforms,
numeric conversions, list operations).

### Implementation

#### PIC Module Support (WasmModule.zig, WasmLinking.zig)

- **GlobalImport / TableImport**: New types stored during `parseImportSection` so
  PIC globals (`__memory_base`, `__table_base`) and tables (`__indirect_function_table`)
  can be resolved by name during merge.
- **resolveName dispatch**: Fixed to look up function symbols in `imports`, global
  symbols in `global_imports`, and table symbols in `table_imports` (was incorrectly
  indexing into `imports` for all symbol kinds).
- **PIC globals in mergeModule**: `__memory_base` and `__table_base` are defined as
  immutable i32 globals initialized to 0. `__indirect_function_table` enables the
  module's table. These are encoded in the global section alongside `__stack_pointer`.
- **Element section parsing**: `parseElementSection_` now extracts function indices
  into `table_func_indices`. `mergeModule` remaps them through `func_remap`.
- **table_index_rel_sleb (type 12)**: Added as `IndexRelocType` (no addend), with
  correct signed LEB128 patching in both `applyRelocsU32` and `resolveCodeRelocations`.
- **Table index resolution**: `table_index_sleb`, `table_index_rel_sleb`, and
  `table_index_i32` resolve to the element section position (table index) rather
  than the function index.

#### Eval Pipeline (wasm_evaluator.zig, build.zig)

- **Build system**: `wasm32_builtins` module embeds `roc_builtins.o` for wasm32 via
  `addWriteFiles` + `@embedFile`, available to the eval module.
- **prepareModuleWithBuiltins**: Creates a fresh module, adds RocOps imports (so the
  import count is correct before merge), merges builtins, populates `BuiltinSymbols`,
  resolves relocations, and materializes `func_bodies`.
- **generateModule**: Reuses existing RocOps imports when the module was prepared with
  them up front, instead of adding imports after builtins have already been merged.

### Root Causes Fixed (continued)

**list_append must use safe version**: The LIR op `list_append_unsafe` is a misnomer —
the dev backend actually calls `roc_builtins_list_append_safe` (which handles capacity
reservation). The wasm codegen was calling the unsafe version (no allocation), causing
wrong values or crashes. Fix: use `list_append_safe` for non-ZST elements, passing
alignment, element_width, and elements_refcounted.

**List literal allocation needs refcount headers**: List literals were allocated via
`emitHeapAllocConst` (raw roc_alloc), but builtins expect data pointers with refcount
headers (for isUnique checks, reallocation, etc.). Fix: call
`roc_builtins_allocate_with_refcount` from `generateList` instead of raw heap alloc.

### Integration Gaps Found During Review (2026-04-01)

These are not theoretical cleanup items; they were found while diffing this branch
 against `origin/main` and checking the current wasm platform artifact.

**`reloc.DATA` was merged but never resolved**: `roc_builtins.o` contains a real
`reloc.DATA` section. The linker already preserved those entries through preload and
`mergeModule()`, but only `reloc.CODE` was ever patched before the final module was
encoded. Fix: record each parsed data segment's byte range within the original data
section body, normalize `reloc.DATA` entries to `(segment_index, in_segment_offset)`,
carry that remap through `mergeModule()`, and resolve data relocations before encode.

**Host RocOps registration was mutating function indices after preload**:
`registerRocOpsFromModule()` was allowed to add imports when the current module already
contained defined functions from a preloaded host object or merged builtins. That
violates the `WasmModule.addImport()` invariant that imports must be finalized before
defined functions exist, and it corrupts every downstream function index. The current
wasm test platform also does not expose RocOps callbacks as imports: it builds a
`RocOps` struct from local callback functions and only imports `roc_dbg`,
`roc_expect_failed`, and `roc_panic`. Fix: expose canonical host-ABI callback symbols in
the wasm host module, bind those existing callback functions into the table without
adding imports, and keep eval-style modules on a separate "reuse existing imports"
path instead of introducing a callback/import fallback.

**Builtin-import verification was stricter than the current platform contract**:
`verifyNoBuiltinImports()` only allowed the 6 RocOps callback names, but the current
platform legitimately imports `env.roc_panic` and uses it behind the local
`roc_crashed` wrapper. Verification runs before DCE, so rejecting `roc_panic` here is
a false failure against the current platform. Fix: treat `roc_panic` as an allowed
platform import at verification time.

---

## Phase 14: Rebase & Integration Fixes — Done

### What

Rebased the 24-commit surgical-wasm branch from `lir-interpreter` onto `origin/main`,
resolving conflicts and fixing post-rebase issues.

### Changes

**Rebase** (2026-03-31): Rebased all 24 commits from `01701f9ed5` to HEAD onto
`origin/main` (`902f567ccf`), dropping the lir-interpreter branch history. Resolved
merge conflicts in `build.zig`, `wasm_evaluator.zig`, `WasmCodeGen.zig` across 6
conflict rounds.

**Post-rebase fixups**:
- Removed dead `generateI128Shift` and `emitI128DivByConst` functions (were from the
  lir-interpreter branch, superseded by host-call based i128 ops)
- Added missing `dec_to_*_trunc` variants to `generateLowLevel` switch (main added
  these enum variants; our branch had a consolidated handler that only listed
  `dec_to_i64_trunc`)
- Exported `WasmCodeResult` from `eval/mod.zig` (it moved from inside `WasmEvaluator`
  to module level during our refactor)
- Wired `wasm32_builtins` module import for eval test step in `build.zig`
- Migrated regression tests from `eval_tests.zig` (which didn't exist on main) to
  `eval_test.zig`

**`mergeModule` func_remap offset bug** (critical fix):
`self_defined_base` was computed BEFORE the import remapping loop, but that loop can
add new imports via `addImport()` which increases `importCount()`. All defined function
indices in `func_remap` were off by the number of new imports added during merge. This
caused every `call` instruction in merged builtins to reference the wrong function,
producing hundreds of type mismatch errors in the encoded module.
Fix: compute `self_defined_base` AFTER the import loop completes.
Location: `WasmModule.zig` `mergeModule()`, ~line 899.

**Compiler-rt host functions**:
The wasm32 `roc_builtins.o` imports `__multi3` (128-bit multiply) and `__muloti4`
(128-bit multiply with overflow) from the Zig compiler-rt. These are function imports
in the `.o` file (not defined functions), so `mergeModule` correctly propagates them as
imports in the final module. Added host implementations in `wasm_runner.zig` using the
existing `compiler_rt_128.mul_i128` function.

**`preload` require_relocatable flag**: Changed from `true` to `false` for the eval
pipeline. The wasm32 builtins object is a valid relocatable module but may not pass
all strict validation checks (e.g. missing reloc sections for some targets). The merge
pipeline only needs the linking section and reloc entries, not strict validation.

**reloc.CODE offset test**: Fixed expected offset from 2 to 1 — `preload` adjusts
reloc offsets by subtracting the code section's function count LEB128 size (1 byte).

**Unused variable suppressions**: Removed 9 `_ = variable;` patterns (replaced with
`_` parameter names or deleted unused lookups) to pass the codebase lint check.

### Results

- Repl tests: **38/40 pass** (up from 11/40 before fixes)
- Build + lints + snapshots: all pass
- Remaining 2 repl failures: `Str.from_utf8` tests (TrapUnreachable, pre-existing)

---

## Phase 15: Remaining Test Failures — Done

### Status

**Repl**: 40/40 passing.
**Eval**: 1249/1249 passing (full suite).

### Fixes Applied

#### 1. `Str.from_utf8` FromUtf8Try ↔ tag union layout mismatch (FIXED)

The C builtin `roc_builtins_str_from_utf8` writes its result in `FromUtf8Try` layout
(byte_index@0, string@8, is_ok@20, problem_code@21 on wasm32), but the codegen
was writing this directly into the Roc tag union buffer without conversion. The Roc
tag union has a different layout (Ok: Str@0, disc@disc_offset; Err: byte_index@0,
problem@8, disc@disc_offset).

**Fix**: Added conversion code in the `str_from_utf8` codegen (WasmCodeGen.zig) that
calls the C builtin into a temporary 24-byte buffer, then reads `is_ok` and copies
the appropriate fields into the tag union result buffer:
- Ok: copy RocStr (12 bytes) from raw+8 to result+0, set disc=1
- Err: copy byte_index (8 bytes) from raw+0, problem_code from raw+21 to result+8, set disc=0

#### 2. WASM stack frame alignment (FIXED)

The WASM codegen's `emitStackPrologue` did not round the stack frame size to 8-byte
alignment. When a function's accumulated `stack_frame_size` was not a multiple of 8,
the frame pointer (obtained by subtracting from an 8-byte-aligned stack pointer)
would be misaligned. This caused `@alignCast` assertions in builtins that cast output
pointers to structs containing u64 fields (like `FromUtf8Try`).

**Fix**: Round `stack_frame_size` up to 8-byte alignment in `emitStackPrologue`:
`self.stack_frame_size = (self.stack_frame_size + 7) & ~@as(u32, 7);`

#### 3. Missing `transferAppFunctions()` before encode (FIXED)

App function bodies added via `setFunctionBody` (RocCall entrypoint, eval wrapper)
were not transferred into `code_bytes` before `encode()`. This meant
`function_offsets` had no entries for app functions, causing `materializeFuncBodies`
to go out of bounds during encoding — resulting in tests hanging.

**Fix**: Call `self.module.transferAppFunctions()` after all `setFunctionBody` calls
and before `encode()` in `WasmCodeGen.generateModule`.

#### 4. RocOps struct at address 0 causes null pointer trap (FIXED)

The eval wrapper placed the RocOps struct at WASM linear memory address 0. When
builtins cast `roc_ops` pointers to Zig optional pointers (`?*anyopaque`), address 0
is treated as `null`. In `strDecref` (called during `list.decref` for
`Str.join_with` and `Str.split_on`), the null context check hit an explicit
`unreachable`, causing `TrapUnreachable`.

**Fix**: Allocate the RocOps struct in the eval wrapper's stack frame (at a non-zero
address) instead of at memory offset 0.

#### 5. Memory leaks in eval pipeline (FIXED)

- `builtins_module` from `WasmModule.preload` was not freed after merge. Added
  `defer builtins_module.deinit()`.
- `MergeResult.symbol_remap` from `mergeModule` was discarded. Changed to call
  `merge_result.deinit()`.

### Known Limitation: Compiler-rt imports

The wasm32 `roc_builtins.o` imports `__multi3` and `__muloti4` from compiler-rt.
Currently handled by host function implementations in `helpers.zig`. For the
CLI `roc build` path, these will need to be resolved differently — either
by bundling compiler-rt into the builtins object for wasm32 only, or by providing
them through the host platform's object.

---

## Implementation Order and Dependencies

```
Phase 1: Padded LEB128 Helpers
    │  (no dependencies, pure utility functions)
    ▼
Phase 2: Linking Data Structures
    │  (depends on Phase 1 for overwritePaddedU32 in applyRelocsU32)
    ▼
Phase 3: WASM Module Parser
    │  (depends on Phase 2 for LinkingSection, RelocationSection types)
    ▼
Phase 4: Surgical Linking (linkHostToAppCalls)
    │  (depends on Phases 1-3)
    │
    │  ★ MILESTONE 1: Surgical linking works on test fixtures
    │  Can parse, link, and serialize clang-produced .wasm objects.
    │
    ├─────────────────────────┐
    ▼                         ▼
Phase 5: Memory/Table     Phase 6: Function Pointer
    Ownership                 Representation & RocOps
    │                         │
    │  (design decisions      │  (design decisions that
    │   that inform codegen)  │   inform codegen + hosted calls)
    │                         │
    ├─────────────────────────┘
    ▼
Phase 7a: Entrypoint ABI Migration
    │  (depends on Phase 6 for RocOps layout)
    ▼
Phase 7b: CodeBuilder & WasmCodeGen Refactor
    │  (depends on Phase 7a for RocCall signature)
    │
    │  ★ MILESTONE 2: App code appends into host module
    │  CodeBuilder inserts functions with correct relocation offsets.
    │
    ├─────────────────────────┐
    ▼                         │
Phase 8: Builtins Migration   │
    │  8a: Merge .o           │
    │  8b: Symbol mapping     │
    │  8c: ABI rewrites       │
    │  8d: Remove old imports │
    │  8e: Verification       │
    │                         │
    │  ★ MILESTONE 3: Builtins work via surgical linking
    │  No env.roc_* builtin imports. All call sites use
    │  decomposed C ABI through roc_builtins_* symbols.
    │                         │
    ▼                         ▼
Phase 9: Hosted Call Lowering
    │  (depends on Phase 6 for function pointer representation)
    │
    │  ★ MILESTONE 4: Hosted functions work
    │  App can call platform-provided hosted functions
    │  via RocOps.hosted_fns table indices.
    │
    ▼
Phase 10: Dead Code Elimination
    │  (depends on all code generation phases being complete)
    ▼
Phase 11: Serialization Updates ✅
    │  (depends on Phase 10)
    ▼
Phase 12: CLI Integration ✅
    │  (depends on all previous phases)
    │
    │  ★ MILESTONE 5: End-to-end WASM builds
    │  `roc build --target=wasm32` produces working .wasm files.
    │  Eval tests pass using surgical linking pipeline.
    │  All host function reimplementations removed from helpers.zig.
    ▼
    DONE
```

**Parallelism opportunities**:
- Phases 5 and 6 can be developed in parallel (design-only, no code dependencies)
- Phase 8 (builtins) and Phase 9 (hosted calls) can be developed in parallel after
  Phase 7b is complete, since they are independent code generation concerns

---

## Appendix A: WASM Relocatable Object Format Reference

This section documents the binary format details needed for the parser (Phase 3).

### Section Layout

A relocatable `.wasm` file follows the standard WASM binary format with additional
custom sections:

```
┌──────────────────────────────────────┐
│ Magic: 0x00 0x61 0x73 0x6D ("\0asm")│
│ Version: 0x01 0x00 0x00 0x00  (1)   │
├──────────────────────────────────────┤
│ Type Section (ID=1)                  │
│ Import Section (ID=2)                │
│ Function Section (ID=3)              │
│ Table Section (ID=4)  [optional]     │
│ Memory Section (ID=5) [optional]     │
│ Global Section (ID=6) [must be empty]│
│ Export Section (ID=7)                │
│ Element Section (ID=9) [optional]    │
│ DataCount Section (ID=12) [optional] │
│ Code Section (ID=10)                 │
│ Data Section (ID=11)  [optional]     │
├──────────────────────────────────────┤
│ Custom: "linking" (symbol table)     │
│ Custom: "reloc.CODE"                 │
│ Custom: "reloc.DATA"  [optional]     │
│ Custom: "name"        [optional]     │
│ Custom: other/debug   [optional]     │
└──────────────────────────────────────┘
```

Notes:
- `DataCount` appears in real relocatable objects produced by Zig, including the shipped
  `src/cli/targets/wasm32/roc_builtins.o`, so the parser must consume it even though we do
  not use its payload.
- Additional custom sections such as debug metadata and `reloc..debug_*` may appear and
  should be skipped unless the parser explicitly needs them.

### Linking Custom Section Format

```
Section header: ID=0 (custom), size, name="linking"
Body:
  version: u32 (LEB128) = 2
  Subsections (repeated):
    subsection_id: u8
    subsection_size: u32 (LEB128)
    subsection_body: [subsection_size bytes]

Subsection 8 (WASM_SYMBOL_TABLE):
  count: u32 (LEB128)
  For each symbol:
    kind: u8 (0=func, 1=data, 2=global, 3=section, 4=event, 5=table)
    flags: u32 (LEB128)
    If kind == func or kind == global or kind == event or kind == table:
      index: u32 (LEB128)  // function/global/event/table index
      If flags & WASM_SYM_EXPLICIT_NAME or NOT flags & WASM_SYM_UNDEFINED:
        name_len: u32 (LEB128)
        name: [name_len bytes]
    If kind == data:
      name_len: u32 (LEB128)
      name: [name_len bytes]
      If NOT flags & WASM_SYM_UNDEFINED:
        segment_index: u32 (LEB128)
        data_offset: u32 (LEB128)
        data_size: u32 (LEB128)

Subsection 5 (WASM_SEGMENT_INFO):
  count: u32 (LEB128)
  For each segment:
    name_len: u32 (LEB128)
    name: [name_len bytes]
    alignment: u32 (LEB128)  // log2 alignment
    flags: u32 (LEB128)

Subsection 6 (WASM_INIT_FUNCS):
  count: u32 (LEB128)
  For each init func:
    priority: u32 (LEB128)
    symbol_index: u32 (LEB128)
```

### Relocation Custom Section Format

```
Section header: ID=0 (custom), size, name="reloc.CODE" or "reloc.DATA"
Body:
  target_section_index: u32 (LEB128)  // which section these relocations apply to
  count: u32 (LEB128)
  For each relocation:
    type: u8
    offset: u32 (LEB128)         // byte offset within target section body
    symbol_index: u32 (LEB128)   // index into linking section's symbol table
    If type in {3, 4, 5, 8, 9, 14, 15, 16}:  // offset-type relocations
      addend: i32 (LEB128, signed)
```

### Padded LEB128 Encoding in Code Section

In a relocatable object, all `call`, `global.get`, `global.set`, and `call_indirect`
instructions use 5-byte padded LEB128 for their operands:

```
Standard:   call 3    →  10 03              (2 bytes)
Padded:     call 3    →  10 83 80 80 80 00  (6 bytes: opcode + 5-byte operand)
```

This padding is only present in relocatable objects. The final serialized module uses
standard variable-length LEB128 (the relocation values are already patched in, and
the raw bytes are emitted as-is — so the padding persists in the output, which is valid
WASM since padded LEB128 is a valid encoding of any value).

---

## Appendix B: Files Modified/Created

| File | Action | Description |
|------|--------|-------------|
| `src/backend/wasm/WasmModule.zig` | **Major rewrite** | Add parser (`preload`), extend struct, update serializer, memory/table ownership |
| `src/backend/wasm/WasmLinking.zig` | **New file** | Linking data structures, relocation types, `applyRelocsU32` |
| `src/backend/wasm/CodeBuilder.zig` | **New file** | Function body accumulator with deferred relocation offset computation |
| `src/backend/wasm/WasmCodeGen.zig` | **Major refactor** | RocCall ABI, CodeBuilder integration, builtin ABI rewrites, hosted call lowering |
| `src/backend/wasm/mod.zig` | **Minor update** | Export WasmLinking, CodeBuilder |
| `src/eval/wasm_evaluator.zig` | **Refactor** | Use surgical linking pipeline with eval host shim |
| `src/eval/test/helpers.zig` | **Simplify** | Remove ~45 host function reimplementations |
| `src/repl/wasm_runner.zig` | **Simplify** | Same host function removal as helpers.zig |
| `src/cli/main.zig` | **Enable** | Remove wasm32 block, wire up surgical linking build pipeline |
| `build.zig` | **Minor update** | Ensure `roc_builtins.o` for wasm32 is built and embedded |

---

## Appendix C: Status and Remaining Open Questions

1. **Builtins `.o` availability**: Resolved.
   `roc_builtins.o` for wasm32 is already built by `build.zig` and embedded in the CLI, so
   Phase 8 can treat it as an existing input rather than a prerequisite build task.

1b. **Phase 8 completed** (2026-03-29):
   All sub-phases implemented:
   - 8a: `mergeModule()` — type dedup, function/code/data/symbol/reloc merging
   - 8b: `BuiltinSymbols` — 45 builtin ops mapped to symbol indices via `populate()`
   - 8c: All call sites rewritten to use `builtin_syms` directly (no legacy imports)
   - 8d: All ~40 legacy import fields removed; `registerHostImports` → `registerRocOpsImports`
   - 8e: `verifyNoBuiltinImports()`, `resolveCodeRelocations()`, `materializeFuncBodies()`
   Missing builtins added to `dev_wrappers.zig`: list_eq, list_str_eq, list_list_eq,
   list_reverse, i32_mod_by, i64_mod_by.

1c. **Phase 9 completed** (2026-03-29):
   Implemented `generateHostedCall()` in `WasmCodeGen.zig` (~100 lines):
   - Generates arg expressions, stabilizes composites into locals
   - Marshals args into contiguous stack buffer with alignment
   - Loads table index from `RocOps.hosted_fns_ptr + (index * 4)` in linear memory
   - Emits `call_indirect` with `roc_call_type_idx` (3-arg RocCall ABI)
   - Loads return value (primitive, composite pointer, or ZST)
   Not yet testable end-to-end: eval wrapper still hardcodes `hosted_fns_count=0`,
   test platform provides no hosted functions, and build system doesn't thread
   hosted function count to WASM backend. Will be exercisable after CLI integration
   (Phase 12) connects the full pipeline.

1d. **Phase 10 completed** (2026-03-29):
   Implemented `eliminateDeadCode()` and `traceLiveFunctions()` in `WasmModule.zig`:
   - Iterative call graph tracing from exports, init funcs, element section entries, and called_fns
   - Dead JS imports removed (not just stubbed), `dead_import_dummy_count` incremented
   - Remaining import call sites reindexed via relocation patching
   - Dead defined-function bodies replaced with 3-byte `unreachable; end` stubs
   - Function indices preserved (stubs maintain index stability)
   - Conservative indirect call handling: `type_index_leb` relocs mark all
     element-section functions with matching type signature as live
   Also fixed `init_funcs` parsing in `WasmLinking.zig` (was previously skipped).
   All 9 planned tests passing.

1e. **Phase 14 completed** (2026-03-31):
   Rebased onto origin/main, fixed critical `mergeModule` func_remap offset bug (defined
   function indices were off by the count of imports added during merge), added compiler-rt
   host functions for `__multi3`/`__muloti4`, and various post-rebase cleanups. Repl tests
   went from 11/40 to 38/40.

1f. **Current follow-up completed** (2026-04-01):
   Three post-integration issues found during review were fixed:
   - `reloc.DATA` entries are now normalized, remapped during merge, and resolved into
     final data bytes before encoding
   - host-side RocOps registration no longer adds late imports; host modules expose
     canonical `host_abi` callback functions and bind those existing callbacks into the table
   - `verifyNoBuiltinImports()` now allows the platform's legitimate `roc_panic` import

1g. **Remaining work to call surgical WASM complete**:
   - **Full eval/regression rerun**: rerun the full eval/REPL suites after the latest fixes,
     update the recorded pass rate, and fix any remaining builtin/runtime failures if they
     still exist
   - **End-to-end wasm harness**: make the wasm integration tests self-contained and green.
     In the current checkout, `zig build test-backend` is blocked by a missing `bytebox`
     test-module import and `zig build test-wasm-static-lib` expects `test/wasm/app.wasm`
     to already exist
   - **Hosted effects coverage**: add a wasm platform/test with a non-empty hosted function
     table so `generateHostedCall()` is exercised end-to-end instead of only by local/unit
     coverage
   - **Status cleanup**: keep this document's recorded status synchronized with actual
     verification results instead of leaving stale “in progress” or old pass-count notes

2. **Host module authoring guidance**: What exact compilation flags do we want to support and
   document for platform authors producing relocatable `host.wasm` artifacts?
   We should publish one blessed example, ideally based on the existing WASM test platform.

3. **COMDAT groups**: The linking section can contain COMDAT metadata. The old Rust compiler
   parsed but did not semantically use it. We should preserve that behavior: parse enough to
   keep symbol-table indices correct, but defer any COMDAT-aware deduplication logic.

4. **Compiler-rt dependency**: The wasm32 `roc_builtins.o` currently imports `__multi3` and
   `__muloti4` from compiler-rt. The builtins codebase has `is_wasm` guards in
   `compiler_rt_128.zig` that decompose most i128 ops into 64-bit ops for wasm32, but some
   code paths still trigger native i128 multiply (likely in `num.zig` overflow detection or
   `dec.zig`). Ideally these should be found and fixed so the builtins have zero compiler-rt
   imports on wasm32, matching the approach used for all other targets.
