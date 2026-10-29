//! Shared-memory image for dev backend execution through the interpreter shim.
//!
//! The parent process emits machine code plus explicit relocation and readonly
//! data records. The child shim maps the image, patches those records in place,
//! marks the generated code pages executable, and calls the requested Roc ABI
//! entrypoint wrapper directly from the shared mapping.

const std = @import("std");
const lir = @import("lir");
const Relocation = @import("Relocation.zig").IndexedRelocation;
const DataRelocationKind = @import("Relocation.zig").DataRelocationKind;
const StaticDataExport = @import("StaticDataExport.zig").StaticDataExport;
const StaticDataRelocation = @import("StaticDataExport.zig").StaticDataRelocation;
pub const StaticDataPool = @import("StaticDataPool.zig");

const base = @import("base");

const Allocator = std.mem.Allocator;

/// Boxy runtime sidecar embedded in a dev run image: descriptor tables, the
/// committed layout store, and the name store the descriptors index. Its
/// offsets are relative to the run image's boxy blob region.
pub const BoxySidecar = lir.LirImage.BoxySidecar;

/// Magic number identifying a dev backend run image ("RDEV" in little-endian bytes).
pub const MAGIC: u32 = 0x56454452;

/// Version of the shared-memory dev run image format.
/// v6: function relocations carry their producer-selected call encoding.
/// v7: data symbols may live in the hot-reload static pool, and images carry
/// their static pool chunk and whether their static data names code.
pub const FORMAT_VERSION: u32 = 7;

/// Maximum bytes reserved per host jump stub on the supported dev-shim hosts.
/// The machine-code shim owns the per-arch emitted sizes and asserts at compile
/// time that each fits within this reservation (see `machine_code_shim/main.zig`).
pub const max_jump_stub_size = 20;

/// Errors raised when a mapped dev run image is malformed or unsupported.
pub const ImageError = error{
    InvalidDevRunImage,
    UnsupportedDevRunImageVersion,
};

/// Errors raised while serializing machine code and static data into shared memory.
pub const WriteError = Allocator.Error || ImageError || StaticDataPool.Error || error{
    UnsupportedDevRunRelocation,
};

/// Offset and length pair for image sections stored relative to the image base.
pub const ArrayRef = extern struct {
    offset: u64,
    len: u64,

    pub fn empty() ArrayRef {
        return .{ .offset = 0, .len = 0 };
    }
};

/// Offset and length pair for names stored in the image symbol-name byte table.
pub const StringRef = extern struct {
    offset: u64,
    len: u64,
};

/// Header at the start of every shared-memory dev run image.
pub const Header = extern struct {
    magic: u32,
    format_version: u32,
    image_size: u64,
    page_size: u64,
    executable: ArrayRef,
    code: ArrayRef,
    function_stubs: ArrayRef,
    entrypoints: ArrayRef,
    code_symbols: ArrayRef,
    relocations: ArrayRef,
    data_relocations: ArrayRef,
    symbol_names: ArrayRef,
    data: ArrayRef,
    data_symbols: ArrayRef,
    /// Raw bytes of the boxy sidecar blob (empty when the program has no boxy
    /// tables). `boxy_sidecar` offsets are relative to this region's base.
    boxy_blob: ArrayRef,
    boxy_sidecar: BoxySidecar,
    /// This image's static pool chunk: its `StaticDataPool.Chunk` header
    /// through the end of the image, which holds the data section. Empty when
    /// the image has no entries for later images to reuse.
    static_pool: ArrayRef,
    /// Nonzero when the image's static data holds pointers to its own code.
    static_code_references: u32,
    _padding: u32 = 0,
};

/// Entry wrapper exported by the generated dev code image.
pub const Entrypoint = extern struct {
    ordinal: u32,
    _padding: u32 = 0,
    code_offset: u64,
};

/// Relocation encodings stored in shared memory for the shim to apply.
pub const RelocationKind = enum(u8) {
    linked_function_x86_call_rel32 = 1,
    linked_data_abs64 = 2,
    linked_data_rel32 = 3,
    linked_data_page21 = 4,
    linked_data_pageoff12 = 5,
    linked_function_aarch64_call26 = 6,
};

/// Serialized relocation record that names the target symbol explicitly.
pub const RelocationRecord = extern struct {
    code_offset: u64,
    symbol: StringRef,
    kind: u8,
    _padding: [7]u8 = @as([7]u8, @splat(0)),

    pub fn relocationKind(self: RelocationRecord) ImageError!RelocationKind {
        return std.enums.fromInt(RelocationKind, self.kind) orelse error.InvalidDevRunImage;
    }
};

/// Where a data symbol's bytes live.
pub const DataLocation = enum(u32) {
    /// In this image's data section; `data_offset` is relative to it.
    image = 0,
    /// In the hot-reload static pool; `data_offset` is relative to the mapping base.
    pool = 1,
};

/// Description of one exported readonly data symbol in the run image.
pub const DataSymbol = extern struct {
    name: StringRef,
    data_offset: u64,
    len: u64,
    symbol_offset: u64,
    alignment: u32,
    location: u32 = @backingInt(DataLocation.image),

    pub fn dataLocation(self: DataSymbol) ImageError!DataLocation {
        return std.enums.fromInt(DataLocation, self.location) orelse error.InvalidDevRunImage;
    }
};

/// Description of one generated Roc procedure in the run image.
pub const CodeSymbol = extern struct {
    name: StringRef,
    code_offset: u64,
};

/// Runtime meaning of a pointer relocation stored in readonly data.
pub const StaticDataTargetKind = enum(u8) {
    address = 1,
    function_pointer = 2,
};

/// Pointer relocation from one readonly data symbol to another image symbol.
pub const DataRelocationRecord = extern struct {
    data_offset: u64,
    symbol: StringRef,
    addend: i64,
    target_kind: u8,
    _padding: [7]u8 = @as([7]u8, @splat(0)),

    pub fn targetKind(self: DataRelocationRecord) ImageError!StaticDataTargetKind {
        return std.enums.fromInt(StaticDataTargetKind, self.target_kind) orelse error.InvalidDevRunImage;
    }
};

/// Entrypoint metadata provided by codegen before the image is serialized.
pub const EntrypointInput = struct {
    ordinal: u32,
    code_offset: usize,
};

/// Generated procedure metadata provided by codegen before image serialization.
pub const CodeSymbolInput = struct {
    name: []const u8,
    code_offset: usize,
};

/// Borrowed view of a validated dev run image mapped in the shim process.
pub const ProgramView = struct {
    executable: []u8,
    code: []u8,
    function_stubs: []u8,
    entrypoints: []const Entrypoint,
    code_symbols: []const CodeSymbol,
    relocations: []const RelocationRecord,
    data_relocations: []const DataRelocationRecord,
    symbol_names: []const u8,
    data: []u8,
    data_symbols: []const DataSymbol,
    page_size: usize,
    boxy_blob: []u8,
    boxy_sidecar: BoxySidecar,
    /// The whole shared mapping, which pooled data symbols point into.
    mapping: []u8,
    static_code_references: bool,

    pub fn symbolName(self: *const ProgramView, ref: StringRef) ImageError![]const u8 {
        const start = try asBoundedOffset(ref.offset, self.symbol_names.len);
        const len = try asBoundedLen(ref.len);
        if (len > self.symbol_names.len or start > self.symbol_names.len - len) return error.InvalidDevRunImage;
        return self.symbol_names[start..][0..len];
    }

    pub fn dataSymbolName(self: *const ProgramView, symbol: DataSymbol) ImageError![]const u8 {
        return self.symbolName(symbol.name);
    }

    pub fn codeSymbolName(self: *const ProgramView, symbol: CodeSymbol) ImageError![]const u8 {
        return self.symbolName(symbol.name);
    }

    /// The bytes of a data symbol, wherever they live.
    pub fn dataSymbolBytes(self: *const ProgramView, symbol: DataSymbol) ImageError![]u8 {
        const container = switch (try symbol.dataLocation()) {
            .image => self.data,
            .pool => self.mapping,
        };
        const data_offset = try asBoundedOffset(symbol.data_offset, container.len);
        const len = try asBoundedLen(symbol.len);
        if (len > container.len - data_offset) return error.InvalidDevRunImage;
        if (symbol.symbol_offset > len) return error.InvalidDevRunImage;
        return container[data_offset..][0..len];
    }
};

/// Serialize dev backend machine code, entrypoints, relocations, and data into
/// shared memory, placing each static data export as `plan` decided.
pub fn writeToSharedMemory(
    scratch: Allocator,
    image_allocator: Allocator,
    base_ptr: [*]align(1) u8,
    page_size: usize,
    code: []const u8,
    entrypoint_inputs: []const EntrypointInput,
    code_symbol_inputs: []const CodeSymbolInput,
    relocation_symbol_names: []const []const u8,
    relocations: []const Relocation,
    data_exports: []const StaticDataExport,
    plan: *const StaticDataPool.Plan,
    boxy_blob: []const u8,
    boxy_sidecar: BoxySidecar,
) WriteError!*Header {
    if (!std.math.isPowerOfTwo(page_size)) return error.InvalidDevRunImage;
    if (plan.placements.len != data_exports.len) base.invariant("run image invariant violated: static data plan covers {d} of {d} exports", .{ plan.placements.len, data_exports.len });

    var symbol_names: SymbolNames = .{};
    defer symbol_names.deinit(scratch);

    const symbol_refs = try scratch.alloc(StringRef, relocation_symbol_names.len);
    defer scratch.free(symbol_refs);
    for (relocation_symbol_names, symbol_refs) |name, *ref| ref.* = try symbol_names.intern(scratch, name, .reference);

    var relocation_records = std.ArrayList(RelocationRecord).empty;
    defer relocation_records.deinit(scratch);
    try relocation_records.ensureTotalCapacity(scratch, relocations.len);
    var data_relocation_records = std.ArrayList(DataRelocationRecord).empty;
    defer data_relocation_records.deinit(scratch);

    var code_symbols = std.ArrayList(CodeSymbol).empty;
    defer code_symbols.deinit(scratch);
    try code_symbols.ensureTotalCapacity(scratch, code_symbol_inputs.len);

    for (code_symbol_inputs) |input| {
        if (input.code_offset >= code.len) return error.InvalidDevRunImage;
        const name_ref = try symbol_names.intern(scratch, input.name, .code);
        code_symbols.appendAssumeCapacity(.{
            .name = name_ref,
            .code_offset = @intCast(input.code_offset),
        });
    }

    for (relocations) |relocation| {
        switch (relocation) {
            .linked_function => |function| {
                relocation_records.appendAssumeCapacity(.{
                    .code_offset = function.offset,
                    .symbol = symbol_refs[@backingInt(function.symbol)],
                    .kind = @backingInt(switch (function.kind) {
                        .x86_call_rel32 => RelocationKind.linked_function_x86_call_rel32,
                        .aarch64_call26 => RelocationKind.linked_function_aarch64_call26,
                    }),
                });
            },
            .linked_data => |data| {
                relocation_records.appendAssumeCapacity(.{
                    .code_offset = data.offset,
                    .symbol = symbol_refs[@backingInt(data.symbol)],
                    .kind = @backingInt(relocationKindForData(data.kind)),
                });
            },
            .local_data => return error.UnsupportedDevRunRelocation,
            .retired, .runtime_number => {},
        }
    }

    var data_symbols = std.ArrayList(DataSymbol).empty;
    defer data_symbols.deinit(scratch);
    try data_symbols.ensureTotalCapacity(scratch, data_exports.len);
    try data_relocation_records.ensureTotalCapacity(scratch, plan.local_relocation_count);

    const data_refs = try scratch.alloc(StringRef, data_exports.len);
    defer scratch.free(data_refs);
    for (data_exports, data_refs) |data_export, *ref| {
        ref.* = try symbol_names.intern(scratch, data_export.symbol_name, .data);
    }

    for (data_exports, data_refs, plan.placements) |data_export, name_ref, placement| {
        const alignment = StaticDataPool.exportAlignment(data_export);
        if (!std.math.isPowerOfTwo(alignment)) return error.InvalidStaticDataAlignment;
        const data_offset, const location: DataLocation = switch (placement) {
            .local => |local| .{ local.data_offset, .image },
            .pooled => |pooled| .{ pooled, .pool },
        };
        data_symbols.appendAssumeCapacity(.{
            .name = name_ref,
            .data_offset = @intCast(data_offset),
            .len = @intCast(data_export.bytes.len),
            .symbol_offset = data_export.symbol_offset,
            .alignment = alignment,
            .location = @backingInt(location),
        });
        // A pooled entry's relocations were applied when its own image loaded.
        if (location == .pool) continue;

        for (data_export.relocations) |relocation| {
            if (relocation.offset > std.math.maxInt(usize)) return error.InvalidDevRunImage;
            const relocation_offset: usize = @intCast(relocation.offset);
            if (relocation_offset > data_export.bytes.len or @sizeOf(usize) > data_export.bytes.len - relocation_offset) {
                return error.InvalidDevRunImage;
            }
            const target_ref = switch (relocation.kind) {
                .address => switch (relocation.target) {
                    .data_symbol => |id| data_refs[@backingInt(id)],
                    .named => symbol_names.find(relocation.target_symbol_name, .data) orelse return error.UnsupportedStaticDataRelocation,
                },
                .function_pointer => symbol_names.find(relocation.target_symbol_name, .code) orelse return error.UnsupportedStaticDataRelocation,
            };
            data_relocation_records.appendAssumeCapacity(.{
                .data_offset = @intCast(data_offset + relocation_offset),
                .symbol = target_ref,
                .addend = relocation.addend,
                .target_kind = @backingInt(switch (relocation.kind) {
                    .address => StaticDataTargetKind.address,
                    .function_pointer => StaticDataTargetKind.function_pointer,
                }),
            });
        }
    }

    const function_stub_count = try countReservedFunctionStubs(scratch, relocation_symbol_names, relocations, &symbol_names);
    const function_stub_len = try mulNoOverflow(function_stub_count, max_jump_stub_size);

    const header = try image_allocator.create(Header);
    header.* = undefined;

    const entrypoints = try image_allocator.alloc(Entrypoint, entrypoint_inputs.len);
    for (entrypoint_inputs, entrypoints) |input, *entrypoint| {
        entrypoint.* = .{
            .ordinal = input.ordinal,
            .code_offset = @intCast(input.code_offset),
        };
    }

    const code_symbols_copy = try image_allocator.alloc(CodeSymbol, code_symbols.items.len);
    @memcpy(code_symbols_copy, code_symbols.items);

    const relocation_copy = try image_allocator.alloc(RelocationRecord, relocation_records.items.len);
    @memcpy(relocation_copy, relocation_records.items);

    const data_relocation_copy = try image_allocator.alloc(DataRelocationRecord, data_relocation_records.items.len);
    @memcpy(data_relocation_copy, data_relocation_records.items);

    const symbol_names_copy = try image_allocator.alloc(u8, symbol_names.byte_len);
    var name_entries = symbol_names.refs.iterator();
    while (name_entries.next()) |entry| {
        const ref = entry.value_ptr.ref;
        const start = try asBoundedOffset(ref.offset, symbol_names_copy.len);
        const len = try asBoundedLen(ref.len);
        if (len > symbol_names_copy.len - start) return error.InvalidDevRunImage;
        @memcpy(symbol_names_copy[start..][0..len], entry.key_ptr.*);
    }

    const data_symbols_copy = try image_allocator.alloc(DataSymbol, data_symbols.items.len);
    @memcpy(data_symbols_copy, data_symbols.items);

    const code_copy = try allocRuntimeAlignedBytes(
        image_allocator,
        std.mem.Alignment.fromByteUnits(page_size),
        code.len,
    );
    @memcpy(code_copy, code);

    const function_stubs = try image_allocator.alignedAlloc(u8, .@"16", function_stub_len);
    @memset(function_stubs, 0);

    // Page-aligned so it never shares a page with executable code.
    const boxy_blob_copy = try allocRuntimeAlignedBytes(image_allocator, std.mem.Alignment.fromByteUnits(page_size), boxy_blob.len);
    @memcpy(boxy_blob_copy, boxy_blob);

    // The chunk starts on its own page so that reclaiming the rest of the
    // image never hands out a page the pool still uses.
    const has_chunk = plan.listed_count > 0;
    const chunk: ?*Chunk = if (has_chunk) blk: {
        const bytes = try allocRuntimeAlignedBytes(image_allocator, std.mem.Alignment.fromByteUnits(page_size), @sizeOf(Chunk));
        break :blk @ptrCast(@alignCast(bytes.ptr));
    } else null;
    const chunk_entries = try image_allocator.alloc(StaticDataPool.Entry, plan.listed_count);
    const chunk_relocations = try image_allocator.alloc(StaticDataPool.Relocation, plan.listed_relocation_count);

    const data_copy = try allocRuntimeAlignedBytes(
        image_allocator,
        std.mem.Alignment.fromByteUnits(@max(page_size, plan.max_local_alignment)),
        dataSectionLen(plan),
    );
    @memset(data_copy, 0);
    for (data_exports, plan.placements) |data_export, placement| {
        switch (placement) {
            .local => |local| @memcpy(data_copy[local.data_offset..][0..data_export.bytes.len], data_export.bytes),
            .pooled => {},
        }
    }

    if (chunk) |chunk_header| {
        const data_base = @intFromPtr(data_copy.ptr) - @intFromPtr(base_ptr);
        var entry_index: usize = 0;
        var relocation_index: usize = 0;
        for (data_exports, plan.placements, plan.hashes, 0..) |data_export, placement, hash, i| {
            const local = switch (placement) {
                .local => |local| local,
                .pooled => continue,
            };
            if (!local.listed) continue;
            chunk_entries[entry_index] = .{
                .hash = hash,
                .data_offset = @intCast(data_base + local.data_offset),
                .len = @intCast(data_export.bytes.len),
                .symbol_offset = data_export.symbol_offset,
                .alignment = StaticDataPool.exportAlignment(data_export),
                .relocation_count = @intCast(data_export.relocations.len),
                .first_relocation = @intCast(relocation_index),
            };
            entry_index += 1;
            for (plan.sortedRelocations(i)) |r| {
                const relocation = data_export.relocations[r];
                const target_offset = switch (plan.placements[plan.relocationTarget(i, r)]) {
                    .local => |target| data_base + target.data_offset,
                    .pooled => |target| target,
                };
                chunk_relocations[relocation_index] = .{
                    .offset = relocation.offset,
                    .addend = relocation.addend,
                    .target_data_offset = @intCast(target_offset),
                };
                relocation_index += 1;
            }
        }
        if (entry_index != chunk_entries.len or relocation_index != chunk_relocations.len) base.invariant("run image invariant violated: static pool chunk tables disagree with the plan", .{});
        chunk_header.* = .{
            .entries_offset = (try arrayRef(base_ptr, bytesOfSlice(chunk_entries))).offset,
            .entry_count = @intCast(chunk_entries.len),
            .relocations_offset = (try arrayRef(base_ptr, bytesOfSlice(chunk_relocations))).offset,
            .relocation_count = @intCast(chunk_relocations.len),
        };
    }

    const executable_end = if (function_stubs.len == 0)
        @intFromPtr(code_copy.ptr) + code_copy.len
    else
        @intFromPtr(function_stubs.ptr) + function_stubs.len;
    const executable_len = std.mem.alignForward(usize, executable_end - @intFromPtr(code_copy.ptr), page_size);
    const executable = code_copy.ptr[0..executable_len];

    const image_size = std.mem.alignForward(usize, maxEnd(base_ptr, &.{
        bytesOf(header),
        bytesOfSlice(entrypoints),
        bytesOfSlice(code_symbols_copy),
        bytesOfSlice(relocation_copy),
        bytesOfSlice(data_relocation_copy),
        symbol_names_copy,
        bytesOfSlice(data_symbols_copy),
        executable,
        boxy_blob_copy,
        if (chunk) |chunk_header| bytesOf(chunk_header) else &.{},
        bytesOfSlice(chunk_entries),
        bytesOfSlice(chunk_relocations),
        data_copy,
    }), page_size);

    const static_pool: ArrayRef = if (chunk) |chunk_header| blk: {
        const chunk_offset = @intFromPtr(chunk_header) - @intFromPtr(base_ptr);
        break :blk .{ .offset = @intCast(chunk_offset), .len = @intCast(image_size - chunk_offset) };
    } else ArrayRef.empty();

    header.* = .{
        .magic = MAGIC,
        .format_version = FORMAT_VERSION,
        .image_size = @intCast(image_size),
        .page_size = @intCast(page_size),
        .executable = try arrayRef(base_ptr, executable),
        .code = try arrayRef(base_ptr, code_copy),
        .function_stubs = try arrayRef(base_ptr, function_stubs),
        .entrypoints = try arrayRef(base_ptr, bytesOfSlice(entrypoints)),
        .code_symbols = try arrayRef(base_ptr, bytesOfSlice(code_symbols_copy)),
        .relocations = try arrayRef(base_ptr, bytesOfSlice(relocation_copy)),
        .data_relocations = try arrayRef(base_ptr, bytesOfSlice(data_relocation_copy)),
        .symbol_names = try arrayRef(base_ptr, symbol_names_copy),
        .data = try arrayRef(base_ptr, data_copy),
        .data_symbols = try arrayRef(base_ptr, bytesOfSlice(data_symbols_copy)),
        .boxy_blob = try arrayRef(base_ptr, boxy_blob_copy),
        .boxy_sidecar = boxy_sidecar,
        .static_pool = static_pool,
        .static_code_references = @intFromBool(plan.static_code_references),
    };

    return header;
}

const Chunk = StaticDataPool.Chunk;

/// A chunk's entries need real addresses, so a chunk's data section is never
/// empty even when every listed entry is.
fn dataSectionLen(plan: *const StaticDataPool.Plan) usize {
    return if (plan.listed_count > 0) @max(plan.local_data_len, 1) else plan.local_data_len;
}

/// Return the exact allocator capacity needed to serialize this run image.
pub fn requiredCapacity(
    scratch: Allocator,
    page_size: usize,
    code: []const u8,
    entrypoint_inputs: []const EntrypointInput,
    code_symbol_inputs: []const CodeSymbolInput,
    relocation_symbol_names: []const []const u8,
    relocations: []const Relocation,
    data_exports: []const StaticDataExport,
    plan: *const StaticDataPool.Plan,
    boxy_blob: []const u8,
) WriteError!usize {
    return requiredCapacityFromOffset(scratch, page_size, 0, code, entrypoint_inputs, code_symbol_inputs, relocation_symbol_names, relocations, data_exports, plan, boxy_blob);
}

/// Return the exact allocator offset after serializing this run image starting
/// from an existing allocation cursor.
pub fn requiredCapacityFromOffset(
    scratch: Allocator,
    page_size: usize,
    initial_offset: usize,
    code: []const u8,
    entrypoint_inputs: []const EntrypointInput,
    code_symbol_inputs: []const CodeSymbolInput,
    relocation_symbol_names: []const []const u8,
    relocations: []const Relocation,
    data_exports: []const StaticDataExport,
    plan: *const StaticDataPool.Plan,
    boxy_blob: []const u8,
) WriteError!usize {
    if (!std.math.isPowerOfTwo(page_size)) return error.InvalidDevRunImage;
    if (plan.placements.len != data_exports.len) base.invariant("run image invariant violated: static data plan covers {d} of {d} exports", .{ plan.placements.len, data_exports.len });

    var names: SymbolNames = .{};
    defer names.deinit(scratch);
    for (relocation_symbol_names) |name| _ = try names.intern(scratch, name, .reference);
    for (data_exports) |data_export| _ = try names.intern(scratch, data_export.symbol_name, .data);
    for (code_symbol_inputs) |symbol| _ = try names.intern(scratch, symbol.name, .code);

    var relocation_count: usize = 0;
    for (code_symbol_inputs) |input| {
        if (input.code_offset >= code.len) return error.InvalidDevRunImage;
    }
    for (relocations) |relocation| {
        switch (relocation) {
            .linked_function, .linked_data => {
                relocation_count = try addNoOverflow(relocation_count, 1);
            },
            .local_data => return error.UnsupportedDevRunRelocation,
            .retired, .runtime_number => {},
        }
    }

    for (data_exports, plan.placements) |data_export, placement| {
        if (!std.math.isPowerOfTwo(StaticDataPool.exportAlignment(data_export))) return error.InvalidStaticDataAlignment;
        if (placement == .pooled) continue;
        for (data_export.relocations) |relocation| {
            if (relocation.offset > std.math.maxInt(usize)) return error.InvalidDevRunImage;
            const relocation_offset: usize = @intCast(relocation.offset);
            if (relocation_offset > data_export.bytes.len or @sizeOf(usize) > data_export.bytes.len - relocation_offset) {
                return error.InvalidDevRunImage;
            }
            const target_exists = switch (relocation.kind) {
                .address => switch (relocation.target) {
                    .data_symbol => |id| @backingInt(id) < data_exports.len,
                    .named => names.find(relocation.target_symbol_name, .data) != null,
                },
                .function_pointer => names.find(relocation.target_symbol_name, .code) != null,
            };
            if (!target_exists) return error.UnsupportedStaticDataRelocation;
        }
    }

    const function_stub_count = try countReservedFunctionStubs(scratch, relocation_symbol_names, relocations, &names);
    const function_stub_len = try mulNoOverflow(function_stub_count, max_jump_stub_size);

    var capacity: usize = initial_offset;
    capacity = try addAllocationCapacity(capacity, @alignOf(Header), @sizeOf(Header));
    capacity = try addAllocationCapacity(capacity, @alignOf(Entrypoint), try mulNoOverflow(entrypoint_inputs.len, @sizeOf(Entrypoint)));
    capacity = try addAllocationCapacity(capacity, @alignOf(CodeSymbol), try mulNoOverflow(code_symbol_inputs.len, @sizeOf(CodeSymbol)));
    capacity = try addAllocationCapacity(capacity, @alignOf(RelocationRecord), try mulNoOverflow(relocation_count, @sizeOf(RelocationRecord)));
    capacity = try addAllocationCapacity(capacity, @alignOf(DataRelocationRecord), try mulNoOverflow(plan.local_relocation_count, @sizeOf(DataRelocationRecord)));
    capacity = try addAllocationCapacity(capacity, @alignOf(u8), names.byte_len);
    capacity = try addAllocationCapacity(capacity, @alignOf(DataSymbol), try mulNoOverflow(data_exports.len, @sizeOf(DataSymbol)));
    capacity = try addAllocationCapacity(capacity, page_size, code.len);
    capacity = try addAllocationCapacity(capacity, 16, function_stub_len);
    if (boxy_blob.len > 0) capacity = try addAllocationCapacity(capacity, page_size, boxy_blob.len);
    if (plan.listed_count > 0) capacity = try addAllocationCapacity(capacity, page_size, @sizeOf(Chunk));
    capacity = try addAllocationCapacity(capacity, @alignOf(StaticDataPool.Entry), try mulNoOverflow(plan.listed_count, @sizeOf(StaticDataPool.Entry)));
    capacity = try addAllocationCapacity(capacity, @alignOf(StaticDataPool.Relocation), try mulNoOverflow(plan.listed_relocation_count, @sizeOf(StaticDataPool.Relocation)));
    capacity = std.mem.alignForward(usize, capacity, page_size);
    capacity = try addAllocationCapacity(capacity, @max(page_size, plan.max_local_alignment), dataSectionLen(plan));
    return std.mem.alignForward(usize, capacity, page_size);
}

/// Validate and view an already-mapped dev run image. The image occupies the
/// first `image_bound` bytes after `base_ptr`, inside a mapping of
/// `mapping_size` bytes.
pub fn viewMappedImage(header: *const Header, base_ptr: [*]align(1) u8, image_bound: usize, mapping_size: usize) ImageError!ProgramView {
    if (header.magic != MAGIC) return error.InvalidDevRunImage;
    if (header.format_version != FORMAT_VERSION) return error.UnsupportedDevRunImageVersion;
    if (image_bound > mapping_size) return error.InvalidDevRunImage;
    if (header.image_size == 0 or header.image_size > image_bound) return error.InvalidDevRunImage;

    const image_size: usize = @intCast(header.image_size);
    const page_size = try asBoundedLen(header.page_size);
    if (page_size == 0 or !std.math.isPowerOfTwo(page_size)) return error.InvalidDevRunImage;
    _ = try bytesFromRef(base_ptr, image_size, header.static_pool);

    return .{
        .executable = try bytesFromRef(base_ptr, image_size, header.executable),
        .code = try bytesFromRef(base_ptr, image_size, header.code),
        .function_stubs = try bytesFromRef(base_ptr, image_size, header.function_stubs),
        .entrypoints = try sliceFromRef(Entrypoint, base_ptr, image_size, header.entrypoints),
        .code_symbols = try sliceFromRef(CodeSymbol, base_ptr, image_size, header.code_symbols),
        .relocations = try sliceFromRef(RelocationRecord, base_ptr, image_size, header.relocations),
        .data_relocations = try sliceFromRef(DataRelocationRecord, base_ptr, image_size, header.data_relocations),
        .symbol_names = try bytesFromRef(base_ptr, image_size, header.symbol_names),
        .data = try bytesFromRef(base_ptr, image_size, header.data),
        .data_symbols = try sliceFromRef(DataSymbol, base_ptr, image_size, header.data_symbols),
        .page_size = page_size,
        .boxy_blob = try bytesFromRef(base_ptr, image_size, header.boxy_blob),
        .boxy_sidecar = header.boxy_sidecar,
        .mapping = base_ptr[0..mapping_size],
        .static_code_references = header.static_code_references != 0,
    };
}

fn relocationKindForData(kind: DataRelocationKind) RelocationKind {
    return switch (kind) {
        .abs64 => .linked_data_abs64,
        .rel32 => .linked_data_rel32,
        .page21 => .linked_data_page21,
        .pageoff12 => .linked_data_pageoff12,
    };
}

/// Borrow each distinct name once and assign its final image byte range.
const SymbolNames = struct {
    const Kind = enum(u2) { reference = 0, code = 1, data = 2 };
    const Entry = struct { ref: StringRef, kinds: u2 };

    refs: std.StringHashMapUnmanaged(Entry) = .empty,
    byte_len: usize = 0,

    fn deinit(self: *SymbolNames, allocator: Allocator) void {
        self.refs.deinit(allocator);
    }

    fn intern(self: *SymbolNames, allocator: Allocator, name: []const u8, kind: Kind) WriteError!StringRef {
        const entry = try self.refs.getOrPut(allocator, name);
        if (!entry.found_existing) {
            errdefer _ = self.refs.remove(name);
            const end = try addNoOverflow(self.byte_len, name.len);
            if (end > std.math.maxInt(u32)) return error.InvalidDevRunImage;
            entry.value_ptr.* = .{ .ref = .{ .offset = @intCast(self.byte_len), .len = @intCast(name.len) }, .kinds = 0 };
            self.byte_len = end;
        }
        entry.value_ptr.kinds |= @backingInt(kind);
        return entry.value_ptr.ref;
    }

    fn find(self: *const SymbolNames, name: []const u8, kind: Kind) ?StringRef {
        const entry = self.refs.get(name) orelse return null;
        return if (entry.kinds & @backingInt(kind) != 0) entry.ref else null;
    }
};

fn countReservedFunctionStubs(
    scratch: Allocator,
    names: []const []const u8,
    relocations: []const Relocation,
    declarations: *const SymbolNames,
) WriteError!usize {
    const uses = try scratch.alloc(u2, names.len);
    defer scratch.free(uses);
    @memset(uses, 0);
    for (relocations) |relocation| switch (relocation) {
        .linked_function => |function| {
            uses[@backingInt(function.symbol)] |= 1;
        },
        .linked_data => |data| {
            uses[@backingInt(data.symbol)] |= 2;
        },
        .local_data => return error.UnsupportedDevRunRelocation,
        .retired, .runtime_number => {},
    };
    var count: usize = 0;
    for (names, uses) |name, use| {
        if (use & 1 != 0 or (use & 2 != 0 and declarations.find(name, .data) == null)) count += 1;
    }
    return count;
}

fn addNoOverflow(a: usize, b: usize) ImageError!usize {
    if (b > std.math.maxInt(usize) - a) return error.InvalidDevRunImage;
    return a + b;
}

fn mulNoOverflow(a: usize, b: usize) ImageError!usize {
    if (a != 0 and b > std.math.maxInt(usize) / a) return error.InvalidDevRunImage;
    return a * b;
}

fn addAllocationCapacity(capacity: usize, alignment: usize, len: usize) ImageError!usize {
    const aligned = std.mem.alignForward(usize, capacity, alignment);
    return addNoOverflow(aligned, len);
}

fn allocRuntimeAlignedBytes(allocator: Allocator, alignment: std.mem.Alignment, len: usize) Allocator.Error![]u8 {
    if (len == 0) return allocator.alloc(u8, 0);
    const ptr = allocator.rawAlloc(len, alignment, @returnAddress()) orelse return error.OutOfMemory;
    return ptr[0..len];
}

fn arrayRef(base_ptr: [*]align(1) const u8, bytes: []const u8) ImageError!ArrayRef {
    if (bytes.len == 0) return ArrayRef.empty();

    const base_addr = @intFromPtr(base_ptr);
    const ptr_addr = @intFromPtr(bytes.ptr);
    if (ptr_addr < base_addr) return error.InvalidDevRunImage;
    return .{
        .offset = @intCast(ptr_addr - base_addr),
        .len = @intCast(bytes.len),
    };
}

fn bytesFromRef(base_ptr: [*]align(1) u8, image_size: usize, ref: ArrayRef) ImageError![]u8 {
    const start = try asBoundedOffset(ref.offset, image_size);
    const len = try asBoundedLen(ref.len);
    if (len > image_size or start > image_size - len) return error.InvalidDevRunImage;
    return base_ptr[start..][0..len];
}

fn sliceFromRef(comptime T: type, base_ptr: [*]align(1) u8, image_size: usize, ref: ArrayRef) ImageError![]const T {
    const bytes = try bytesFromRef(base_ptr, image_size, ref);
    if (bytes.len % @sizeOf(T) != 0) return error.InvalidDevRunImage;
    if (bytes.len == 0) return &.{};
    const ptr: [*]const T = @ptrCast(@alignCast(bytes.ptr));
    return ptr[0 .. bytes.len / @sizeOf(T)];
}

fn asBoundedOffset(offset: u64, bound: usize) ImageError!usize {
    if (offset > std.math.maxInt(usize)) return error.InvalidDevRunImage;
    const result: usize = @intCast(offset);
    if (result > bound) return error.InvalidDevRunImage;
    return result;
}

fn asBoundedLen(len: u64) ImageError!usize {
    if (len > std.math.maxInt(usize)) return error.InvalidDevRunImage;
    return @intCast(len);
}

test "run image offsets reject values outside the host buffer" {
    try std.testing.expectEqual(@as(usize, 3), try asBoundedOffset(3, 3));
    try std.testing.expectError(error.InvalidDevRunImage, asBoundedOffset(4, 3));
    try std.testing.expectError(error.InvalidDevRunImage, asBoundedOffset(@as(u64, 1) << 32, 3));
    if (@sizeOf(usize) == 4) {
        try std.testing.expectError(error.InvalidDevRunImage, asBoundedLen(@as(u64, 1) << 32));
    }
}

fn bytesOf(ptr: anytype) []u8 {
    const raw: [*]u8 = @ptrCast(ptr);
    return raw[0..@sizeOf(@TypeOf(ptr.*))];
}

fn bytesOfSlice(slice: anytype) []u8 {
    const Slice = @TypeOf(slice);
    const info = @typeInfo(Slice);
    const child = info.pointer.child;
    const raw: [*]u8 = @ptrCast(slice.ptr);
    return raw[0 .. slice.len * @sizeOf(child)];
}

fn maxEnd(base_ptr: [*]align(1) const u8, slices: []const []const u8) usize {
    const base_addr = @intFromPtr(base_ptr);
    var max_end: usize = 0;
    for (slices) |slice| {
        if (slice.len == 0) continue;
        const start = @intFromPtr(slice.ptr) - base_addr;
        max_end = @max(max_end, start + slice.len);
    }
    return max_end;
}

test "writeToSharedMemory serializes only executable image sections" {
    const page_size = @max(std.heap.page_size_min, 16 * 1024);
    const scratch = std.testing.allocator;

    const code = [_]u8{ 0x48, 0x31, 0xc0, 0xc3 };
    const entrypoint_inputs = [_]EntrypointInput{
        .{ .ordinal = 0, .code_offset = 0 },
        .{ .ordinal = 1, .code_offset = 3 },
    };
    const code_symbol_inputs = [_]CodeSymbolInput{
        .{ .name = "roc__p2a", .code_offset = 3 },
    };
    var symbols: @import("SymbolTable.zig").Table = .{};
    defer symbols.deinit(scratch);
    const alloc_symbol = try symbols.intern(scratch, @import("builtins").shim_symbols.roc_alloc, .shared);
    const answer_symbol = try symbols.intern(scratch, "roc__answer", .shared);
    const relocations = [_]Relocation{
        .{ .linked_function = .{ .offset = 1, .symbol = alloc_symbol, .kind = .x86_call_rel32 } },
        .{ .linked_data = .{ .offset = 2, .symbol = answer_symbol, .kind = .rel32 } },
    };
    const data_bytes = [_]u8{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
    const target_data_bytes = [_]u8{42};
    const data_relocations = [_]StaticDataRelocation{
        .{
            .offset = 0,
            .target_symbol_name = "roc__target",
            .addend = 4,
        },
        .{
            .offset = @sizeOf(usize),
            .target_symbol_name = "roc__p2a",
            .kind = .function_pointer,
        },
    };
    const data_exports = [_]StaticDataExport{
        .{
            .symbol_name = "roc__d0",
            .bytes = &data_bytes,
            .symbol_offset = 1,
            .alignment = 8,
            .relocations = &data_relocations,
        },
        .{
            .symbol_name = "roc__target",
            .bytes = &target_data_bytes,
            .alignment = 1,
        },
    };
    var plan = try StaticDataPool.Plan.init(scratch, &data_exports, false);
    defer plan.deinit();
    const capacity = try requiredCapacity(scratch, page_size, &code, &entrypoint_inputs, &code_symbol_inputs, symbols.names.items, &relocations, &data_exports, &plan, &.{});
    const image_bytes = try scratch.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(page_size), capacity);
    defer scratch.free(image_bytes);

    var image_buffer = std.heap.FixedBufferAllocator.init(image_bytes);
    const image_allocator = image_buffer.allocator();

    const header = try writeToSharedMemory(
        scratch,
        image_allocator,
        image_bytes.ptr,
        page_size,
        &code,
        &entrypoint_inputs,
        &code_symbol_inputs,
        symbols.names.items,
        &relocations,
        &data_exports,
        &plan,
        &.{},
        std.mem.zeroes(BoxySidecar),
    );

    try std.testing.expectEqual(MAGIC, header.magic);
    try std.testing.expectEqual(FORMAT_VERSION, header.format_version);
    try std.testing.expectEqual(@as(u64, page_size), header.page_size);
    try std.testing.expect(header.image_size <= image_bytes.len);
    try std.testing.expect(header.image_size <= capacity);

    const view = try viewMappedImage(header, image_bytes.ptr, @intCast(header.image_size), image_bytes.len);

    try std.testing.expectEqual(page_size, view.page_size);
    try std.testing.expectEqual(@as(usize, 2 * max_jump_stub_size), view.function_stubs.len);
    try std.testing.expectEqual(@as(usize, 0), @intFromPtr(view.executable.ptr) % page_size);
    try std.testing.expectEqualSlices(u8, &code, view.code);
    try std.testing.expectEqual(@as(usize, entrypoint_inputs.len), view.entrypoints.len);
    try std.testing.expectEqual(@as(u32, 0), view.entrypoints[0].ordinal);
    try std.testing.expectEqual(@as(u64, 0), view.entrypoints[0].code_offset);
    try std.testing.expectEqual(@as(u32, 1), view.entrypoints[1].ordinal);
    try std.testing.expectEqual(@as(u64, 3), view.entrypoints[1].code_offset);

    try std.testing.expectEqual(@as(usize, code_symbol_inputs.len), view.code_symbols.len);
    try std.testing.expectEqualStrings("roc__p2a", try view.codeSymbolName(view.code_symbols[0]));
    try std.testing.expectEqual(@as(u64, 3), view.code_symbols[0].code_offset);

    try std.testing.expectEqual(@as(usize, relocations.len), view.relocations.len);
    try std.testing.expectEqual(RelocationKind.linked_function_x86_call_rel32, try view.relocations[0].relocationKind());
    try std.testing.expectEqual(@as(u64, 1), view.relocations[0].code_offset);
    try std.testing.expectEqualStrings(@import("builtins").shim_symbols.roc_alloc, try view.symbolName(view.relocations[0].symbol));
    try std.testing.expectEqual(RelocationKind.linked_data_rel32, try view.relocations[1].relocationKind());
    try std.testing.expectEqual(@as(u64, 2), view.relocations[1].code_offset);
    try std.testing.expectEqualStrings("roc__answer", try view.symbolName(view.relocations[1].symbol));

    try std.testing.expectEqualSlices(u8, &data_bytes, view.data[0..data_bytes.len]);
    try std.testing.expectEqualSlices(u8, &target_data_bytes, view.data[data_bytes.len..][0..target_data_bytes.len]);
    try std.testing.expectEqual(@as(usize, data_exports.len), view.data_symbols.len);
    try std.testing.expectEqualStrings("roc__d0", try view.dataSymbolName(view.data_symbols[0]));
    try std.testing.expectEqual(@as(u64, 0), view.data_symbols[0].data_offset);
    try std.testing.expectEqual(@as(u64, data_bytes.len), view.data_symbols[0].len);
    try std.testing.expectEqual(@as(u64, 1), view.data_symbols[0].symbol_offset);
    try std.testing.expectEqual(@as(u32, 8), view.data_symbols[0].alignment);
    try std.testing.expectEqualStrings("roc__target", try view.dataSymbolName(view.data_symbols[1]));
    try std.testing.expectEqual(@as(u64, data_bytes.len), view.data_symbols[1].data_offset);
    try std.testing.expectEqual(@as(u64, target_data_bytes.len), view.data_symbols[1].len);

    try std.testing.expectEqual(@as(usize, data_relocations.len), view.data_relocations.len);
    try std.testing.expectEqual(StaticDataTargetKind.address, try view.data_relocations[0].targetKind());
    try std.testing.expectEqual(@as(u64, 0), view.data_relocations[0].data_offset);
    try std.testing.expectEqual(@as(i64, 4), view.data_relocations[0].addend);
    try std.testing.expectEqualStrings("roc__target", try view.symbolName(view.data_relocations[0].symbol));
    try std.testing.expectEqual(StaticDataTargetKind.function_pointer, try view.data_relocations[1].targetKind());
    try std.testing.expectEqual(@as(u64, @sizeOf(usize)), view.data_relocations[1].data_offset);
    try std.testing.expectEqual(@as(i64, 0), view.data_relocations[1].addend);
    try std.testing.expectEqualStrings("roc__p2a", try view.symbolName(view.data_relocations[1].symbol));
    // Relocations reuse declaration byte ranges, including forward data references.
    try std.testing.expectEqual(view.data_symbols[1].name, view.data_relocations[0].symbol);
    try std.testing.expectEqual(view.code_symbols[0].name, view.data_relocations[1].symbol);
    try std.testing.expectEqual(@as(usize, "roc_alloc".len + "roc__answer".len + "roc__p2a".len + "roc__d0".len + "roc__target".len), view.symbol_names.len);
}

const PoolTestImage = struct {
    header: *Header,
    view: ProgramView,
};

fn writePoolTestImage(
    mapping: []align(std.heap.page_size_min) u8,
    start: usize,
    page_size: usize,
    data_exports: []const StaticDataExport,
    pool: StaticDataPool.Pool,
) (WriteError || error{TestUnexpectedResult})!PoolTestImage {
    const scratch = std.testing.allocator;
    const code = [_]u8{0xc3};
    const code_symbol_inputs = [_]CodeSymbolInput{.{ .name = "roc__p1", .code_offset = 0 }};

    var plan = try StaticDataPool.Plan.init(scratch, data_exports, true);
    defer plan.deinit();
    const unpooled_end = try requiredCapacityFromOffset(scratch, page_size, start, &code, &.{}, &code_symbol_inputs, &.{}, &.{}, data_exports, &plan, &.{});
    try plan.reusePool(data_exports, pool, start, unpooled_end);
    const end = try requiredCapacityFromOffset(scratch, page_size, start, &code, &.{}, &code_symbol_inputs, &.{}, &.{}, data_exports, &plan, &.{});
    try std.testing.expect(end <= unpooled_end);

    var image_buffer = std.heap.FixedBufferAllocator.init(mapping[start..end]);
    const header = try writeToSharedMemory(
        scratch,
        image_buffer.allocator(),
        mapping.ptr,
        page_size,
        &code,
        &.{},
        &code_symbol_inputs,
        &.{},
        &.{},
        data_exports,
        &plan,
        &.{},
        std.mem.zeroes(BoxySidecar),
    );
    return .{
        .header = header,
        .view = try viewMappedImage(header, mapping.ptr, @intCast(header.image_size), mapping.len),
    };
}

fn poolTestSymbol(view: *const ProgramView, name: []const u8) (ImageError || error{TestUnexpectedResult})!DataSymbol {
    for (view.data_symbols) |symbol| {
        if (std.mem.eql(u8, try view.dataSymbolName(symbol), name)) return symbol;
    }
    return error.TestUnexpectedResult;
}

test "hot reload images reuse static pool entries with identical content" {
    const page_size = std.heap.page_size_min;
    const mapping = try std.testing.allocator.alignedAlloc(u8, .fromByteUnits(page_size), 64 * page_size);
    defer std.testing.allocator.free(mapping);
    @memset(mapping, 0);

    const string_bytes = [_]u8{ 0, 0, 0, 0, 0, 0, 0, 0 } ++ "a static string".*;
    var list_bytes = @as([2 * @sizeOf(usize)]u8, @splat(0));
    list_bytes[@sizeOf(usize)] = 1;
    const list_relocations = [_]StaticDataRelocation{.{
        .offset = 0,
        .target_symbol_name = "roc__string_a",
        .addend = 8,
    }};
    var callable_bytes = @as([@sizeOf(usize)]u8, @splat(0));
    const callable_relocations = [_]StaticDataRelocation{.{
        .offset = 0,
        .target_symbol_name = "roc__p1",
        .kind = .function_pointer,
    }};
    const first_exports = [_]StaticDataExport{
        .{ .symbol_name = "roc__string_a", .bytes = &string_bytes, .symbol_offset = 8, .alignment = 8 },
        .{ .symbol_name = "roc__list_a", .bytes = &list_bytes, .alignment = 8, .relocations = &list_relocations },
        .{ .symbol_name = "roc__callable_a", .bytes = &callable_bytes, .alignment = 8, .relocations = &callable_relocations },
    };

    const first = try writePoolTestImage(mapping, page_size, page_size, &first_exports, .empty);
    try std.testing.expect(first.view.static_code_references);
    const chunk_offset: usize = @intCast(first.header.static_pool.offset);
    try std.testing.expect(chunk_offset != 0 and chunk_offset % page_size == 0);
    try std.testing.expectEqual(@as(u64, first.header.image_size), first.header.static_pool.offset + first.header.static_pool.len);
    const chunk: *const StaticDataPool.Chunk = @ptrCast(@alignCast(mapping.ptr + chunk_offset));
    // The callable names this image's code, so only the string and the list are reusable.
    try std.testing.expectEqual(@as(u64, 2), chunk.entry_count);
    try std.testing.expectEqual(@as(u64, 1), chunk.relocation_count);

    // Stand in for the shim's relocation: the pointer slot of a pooled entry
    // holds an address, which reuse must ignore.
    const first_list = try first.view.dataSymbolBytes(try poolTestSymbol(&first.view, "roc__list_a"));
    std.mem.writeInt(usize, first_list[0..@sizeOf(usize)], 0xdead_beef, .little);

    const later_string_bytes = [_]u8{ 0, 0, 0, 0, 0, 0, 0, 0 } ++ "a later string!".*;
    const second_list_relocations = [_]StaticDataRelocation{.{
        .offset = 0,
        .target_symbol_name = "roc__string_b",
        .addend = 8,
    }};
    const second_exports = [_]StaticDataExport{
        .{ .symbol_name = "roc__list_b", .bytes = &list_bytes, .alignment = 8, .relocations = &second_list_relocations },
        .{ .symbol_name = "roc__string_b", .bytes = &string_bytes, .symbol_offset = 8, .alignment = 8 },
        .{ .symbol_name = "roc__string_c", .bytes = &later_string_bytes, .symbol_offset = 8, .alignment = 8 },
    };
    const pool = StaticDataPool.Pool{ .mapping = mapping, .head_offset = chunk_offset };
    const second = try writePoolTestImage(mapping, @intCast(first.header.image_size), page_size, &second_exports, pool);
    try std.testing.expect(!second.view.static_code_references);

    const first_string = try poolTestSymbol(&first.view, "roc__string_a");
    const reused_string = try poolTestSymbol(&second.view, "roc__string_b");
    const reused_list = try poolTestSymbol(&second.view, "roc__list_b");
    try std.testing.expectEqual(DataLocation.pool, try reused_string.dataLocation());
    try std.testing.expectEqual(DataLocation.pool, try reused_list.dataLocation());
    try std.testing.expectEqual(
        @intFromPtr((try first.view.dataSymbolBytes(first_string)).ptr),
        @intFromPtr((try second.view.dataSymbolBytes(reused_string)).ptr),
    );
    try std.testing.expectEqual(@intFromPtr(first_list.ptr), @intFromPtr((try second.view.dataSymbolBytes(reused_list)).ptr));
    try std.testing.expectEqual(DataLocation.image, try (try poolTestSymbol(&second.view, "roc__string_c")).dataLocation());
    // Reused entries were relocated when the first image loaded.
    try std.testing.expectEqual(@as(usize, 0), second.view.data_relocations.len);
    const second_chunk: *const StaticDataPool.Chunk = @ptrCast(@alignCast(mapping.ptr + @as(usize, @intCast(second.header.static_pool.offset))));
    try std.testing.expectEqual(@as(u64, 1), second_chunk.entry_count);
}

test "static pool reuse requires identical bytes outside pointer slots" {
    const page_size = std.heap.page_size_min;
    const mapping = try std.testing.allocator.alignedAlloc(u8, .fromByteUnits(page_size), 64 * page_size);
    defer std.testing.allocator.free(mapping);
    @memset(mapping, 0);

    const first_bytes = [_]u8{ 0, 0, 0, 0, 0, 0, 0, 0 } ++ "generation-1 LABEL, longer than 23 bytes".*;
    const second_bytes = [_]u8{ 0, 0, 0, 0, 0, 0, 0, 0 } ++ "generation-2 LABEL, longer than 23 bytes".*;
    const first_exports = [_]StaticDataExport{.{ .symbol_name = "roc__s", .bytes = &first_bytes, .symbol_offset = 8, .alignment = 8 }};
    const second_exports = [_]StaticDataExport{.{ .symbol_name = "roc__s", .bytes = &second_bytes, .symbol_offset = 8, .alignment = 8 }};

    const first = try writePoolTestImage(mapping, page_size, page_size, &first_exports, .empty);
    const pool = StaticDataPool.Pool{ .mapping = mapping, .head_offset = @intCast(first.header.static_pool.offset) };
    const second = try writePoolTestImage(mapping, @intCast(first.header.image_size), page_size, &second_exports, pool);

    const symbol = try poolTestSymbol(&second.view, "roc__s");
    try std.testing.expectEqual(DataLocation.image, try symbol.dataLocation());
    try std.testing.expectEqualSlices(u8, &second_bytes, try second.view.dataSymbolBytes(symbol));
    try std.testing.expectEqualSlices(u8, &first_bytes, try first.view.dataSymbolBytes(try poolTestSymbol(&first.view, "roc__s")));
}

test "static pool reuse stays within code-to-data displacement range" {
    const exports = [_]StaticDataExport{.{ .symbol_name = "roc__s", .bytes = "reachable", .alignment = 1 }};
    const page_size = std.heap.page_size_min;
    const mapping = try std.testing.allocator.alignedAlloc(u8, .fromByteUnits(page_size), 64 * page_size);
    defer std.testing.allocator.free(mapping);
    @memset(mapping, 0);

    const first = try writePoolTestImage(mapping, page_size, page_size, &exports, .empty);
    const pool = StaticDataPool.Pool{ .mapping = mapping, .head_offset = @intCast(first.header.static_pool.offset) };

    var near = try StaticDataPool.Plan.init(std.testing.allocator, &exports, true);
    defer near.deinit();
    try near.reusePool(&exports, pool, @intCast(first.header.image_size), 32 * page_size);
    try std.testing.expect(near.placements[0] == .pooled);

    var far = try StaticDataPool.Plan.init(std.testing.allocator, &exports, true);
    defer far.deinit();
    const far_start: usize = @as(usize, std.math.maxInt(i32)) + mapping.len;
    try far.reusePool(&exports, pool, far_start, far_start + page_size);
    try std.testing.expect(far.placements[0] == .local);
}
