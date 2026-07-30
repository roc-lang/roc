//! Shared-memory image for dev backend execution through the interpreter shim.
//!
//! The parent process emits machine code plus explicit relocation and readonly
//! data records. The child shim maps the image, patches those records in place,
//! marks the generated code pages executable, and calls the requested Roc ABI
//! entrypoint wrapper directly from the shared mapping.

const std = @import("std");
const Relocation = @import("Relocation.zig").Relocation;
const DataRelocationKind = @import("Relocation.zig").DataRelocationKind;
const StaticDataExport = @import("StaticDataExport.zig").StaticDataExport;
const StaticDataRelocation = @import("StaticDataExport.zig").StaticDataRelocation;

const Allocator = std.mem.Allocator;

/// Magic number identifying a dev backend run image ("RDEV" in little-endian bytes).
pub const MAGIC: u32 = 0x56454452;

/// Version of the shared-memory dev run image format.
pub const FORMAT_VERSION: u32 = 4;

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
pub const WriteError = Allocator.Error || ImageError || error{
    InvalidStaticDataAlignment,
    UnsupportedDevRunRelocation,
    UnsupportedStaticDataRelocation,
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
};

/// Entry wrapper exported by the generated dev code image.
pub const Entrypoint = extern struct {
    ordinal: u32,
    _padding: u32 = 0,
    code_offset: u64,
};

/// Relocation encodings stored in shared memory for the shim to apply.
pub const RelocationKind = enum(u8) {
    linked_function = 1,
    linked_data_abs64 = 2,
    linked_data_rel32 = 3,
    linked_data_page21 = 4,
    linked_data_pageoff12 = 5,
};

/// Serialized relocation record that names the target symbol explicitly.
pub const RelocationRecord = extern struct {
    code_offset: u64,
    symbol: StringRef,
    kind: u8,
    _padding: [7]u8 = [_]u8{0} ** 7,

    pub fn relocationKind(self: RelocationRecord) ImageError!RelocationKind {
        return switch (self.kind) {
            @intFromEnum(RelocationKind.linked_function) => .linked_function,
            @intFromEnum(RelocationKind.linked_data_abs64) => .linked_data_abs64,
            @intFromEnum(RelocationKind.linked_data_rel32) => .linked_data_rel32,
            @intFromEnum(RelocationKind.linked_data_page21) => .linked_data_page21,
            @intFromEnum(RelocationKind.linked_data_pageoff12) => .linked_data_pageoff12,
            else => error.InvalidDevRunImage,
        };
    }
};

/// Description of one exported readonly data symbol in the run image.
pub const DataSymbol = extern struct {
    name: StringRef,
    data_offset: u64,
    len: u64,
    symbol_offset: u64,
    alignment: u32,
    _padding: u32 = 0,
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
    _padding: [7]u8 = [_]u8{0} ** 7,

    pub fn targetKind(self: DataRelocationRecord) ImageError!StaticDataTargetKind {
        return switch (self.target_kind) {
            @intFromEnum(StaticDataTargetKind.address) => .address,
            @intFromEnum(StaticDataTargetKind.function_pointer) => .function_pointer,
            else => error.InvalidDevRunImage,
        };
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
};

/// Serialize dev backend machine code, entrypoints, relocations, and data into shared memory.
pub fn writeToSharedMemory(
    scratch: Allocator,
    image_allocator: Allocator,
    base_ptr: [*]align(1) u8,
    page_size: usize,
    code: []const u8,
    entrypoint_inputs: []const EntrypointInput,
    code_symbol_inputs: []const CodeSymbolInput,
    relocations: []const Relocation,
    data_exports: []const StaticDataExport,
) WriteError!*Header {
    if (!std.math.isPowerOfTwo(page_size)) return error.InvalidDevRunImage;

    var symbol_names = std.ArrayList(u8).empty;
    defer symbol_names.deinit(scratch);

    var relocation_records = std.ArrayList(RelocationRecord).empty;
    defer relocation_records.deinit(scratch);
    var data_relocation_records = std.ArrayList(DataRelocationRecord).empty;
    defer data_relocation_records.deinit(scratch);

    var code_symbols = std.ArrayList(CodeSymbol).empty;
    defer code_symbols.deinit(scratch);
    var code_symbol_names = std.StringHashMapUnmanaged(void){};
    defer code_symbol_names.deinit(scratch);

    for (code_symbol_inputs) |input| {
        if (input.code_offset >= code.len) return error.InvalidDevRunImage;
        try code_symbol_names.put(scratch, input.name, {});
        try code_symbols.append(scratch, .{
            .name = try appendStringRef(scratch, &symbol_names, input.name),
            .code_offset = @intCast(input.code_offset),
        });
    }

    for (relocations) |relocation| {
        switch (relocation) {
            .linked_function => |function| {
                try relocation_records.append(scratch, .{
                    .code_offset = function.offset,
                    .symbol = try appendStringRef(scratch, &symbol_names, function.name),
                    .kind = @intFromEnum(RelocationKind.linked_function),
                });
            },
            .linked_data => |data| {
                try relocation_records.append(scratch, .{
                    .code_offset = data.offset,
                    .symbol = try appendStringRef(scratch, &symbol_names, data.name),
                    .kind = @intFromEnum(relocationKindForData(data.kind)),
                });
            },
            .local_data, .jmp_to_return => return error.UnsupportedDevRunRelocation,
        }
    }

    var data_bytes = std.ArrayList(u8).empty;
    defer data_bytes.deinit(scratch);

    var data_symbols = std.ArrayList(DataSymbol).empty;
    defer data_symbols.deinit(scratch);

    var data_symbol_names = std.StringHashMapUnmanaged(void){};
    defer data_symbol_names.deinit(scratch);

    var max_data_alignment: usize = 1;
    for (data_exports) |data_export| {
        const alignment = if (data_export.alignment == 0) 1 else data_export.alignment;
        if (!std.math.isPowerOfTwo(alignment)) return error.InvalidStaticDataAlignment;
        max_data_alignment = @max(max_data_alignment, alignment);

        const aligned_offset = std.mem.alignForward(usize, data_bytes.items.len, alignment);
        try data_bytes.appendNTimes(scratch, 0, aligned_offset - data_bytes.items.len);
        const data_offset = data_bytes.items.len;
        try data_bytes.appendSlice(scratch, data_export.bytes);

        const name_ref = try appendStringRef(scratch, &symbol_names, data_export.symbol_name);
        try data_symbol_names.put(scratch, data_export.symbol_name, {});

        try data_symbols.append(scratch, .{
            .name = name_ref,
            .data_offset = @intCast(data_offset),
            .len = @intCast(data_export.bytes.len),
            .symbol_offset = data_export.symbol_offset,
            .alignment = alignment,
        });

        for (data_export.relocations) |relocation| {
            if (relocation.offset > std.math.maxInt(usize)) return error.InvalidDevRunImage;
            const relocation_offset: usize = @intCast(relocation.offset);
            if (relocation_offset > data_export.bytes.len or @sizeOf(usize) > data_export.bytes.len - relocation_offset) {
                return error.InvalidDevRunImage;
            }
            try data_relocation_records.append(scratch, .{
                .data_offset = @intCast(data_offset + relocation_offset),
                .symbol = try appendStringRef(scratch, &symbol_names, relocation.target_symbol_name),
                .addend = relocation.addend,
                .target_kind = @intFromEnum(switch (relocation.kind) {
                    .address => StaticDataTargetKind.address,
                    .function_pointer => StaticDataTargetKind.function_pointer,
                }),
            });
        }
    }

    for (data_exports) |data_export| {
        for (data_export.relocations) |relocation| {
            const target_exists = switch (relocation.kind) {
                .address => data_symbol_names.contains(relocation.target_symbol_name),
                .function_pointer => code_symbol_names.contains(relocation.target_symbol_name),
            };
            if (!target_exists) return error.UnsupportedStaticDataRelocation;
        }
    }

    const function_stub_count = try countReservedFunctionStubs(scratch, relocations, &data_symbol_names);
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

    const symbol_names_copy = try image_allocator.alloc(u8, symbol_names.items.len);
    @memcpy(symbol_names_copy, symbol_names.items);

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

    const data_copy = try allocRuntimeAlignedBytes(
        image_allocator,
        std.mem.Alignment.fromByteUnits(@max(page_size, max_data_alignment)),
        data_bytes.items.len,
    );
    @memcpy(data_copy, data_bytes.items);

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
        data_copy,
    }), page_size);

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
    };

    return header;
}

/// Return the exact allocator capacity needed to serialize this run image.
pub fn requiredCapacity(
    page_size: usize,
    code: []const u8,
    entrypoint_inputs: []const EntrypointInput,
    code_symbol_inputs: []const CodeSymbolInput,
    relocations: []const Relocation,
    data_exports: []const StaticDataExport,
) WriteError!usize {
    return requiredCapacityFromOffset(page_size, 0, code, entrypoint_inputs, code_symbol_inputs, relocations, data_exports);
}

/// Return the exact allocator offset after serializing this run image starting
/// from an existing allocation cursor.
pub fn requiredCapacityFromOffset(
    page_size: usize,
    initial_offset: usize,
    code: []const u8,
    entrypoint_inputs: []const EntrypointInput,
    code_symbol_inputs: []const CodeSymbolInput,
    relocations: []const Relocation,
    data_exports: []const StaticDataExport,
) WriteError!usize {
    if (!std.math.isPowerOfTwo(page_size)) return error.InvalidDevRunImage;

    var symbol_names_len: usize = 0;
    var relocation_count: usize = 0;
    for (code_symbol_inputs) |input| {
        if (input.code_offset >= code.len) return error.InvalidDevRunImage;
        symbol_names_len = try addNoOverflow(symbol_names_len, input.name.len);
    }
    for (relocations) |relocation| {
        switch (relocation) {
            .linked_function => |function| {
                symbol_names_len = try addNoOverflow(symbol_names_len, function.name.len);
                relocation_count = try addNoOverflow(relocation_count, 1);
            },
            .linked_data => |data| {
                symbol_names_len = try addNoOverflow(symbol_names_len, data.name.len);
                relocation_count = try addNoOverflow(relocation_count, 1);
            },
            .local_data, .jmp_to_return => return error.UnsupportedDevRunRelocation,
        }
    }

    var data_len: usize = 0;
    var max_data_alignment: usize = 1;
    var data_relocation_count: usize = 0;
    for (data_exports) |data_export| {
        const alignment = if (data_export.alignment == 0) 1 else data_export.alignment;
        if (!std.math.isPowerOfTwo(alignment)) return error.InvalidStaticDataAlignment;
        max_data_alignment = @max(max_data_alignment, alignment);
        data_len = std.mem.alignForward(usize, data_len, alignment);
        data_len = try addNoOverflow(data_len, data_export.bytes.len);
        symbol_names_len = try addNoOverflow(symbol_names_len, data_export.symbol_name.len);
        data_relocation_count = try addNoOverflow(data_relocation_count, data_export.relocations.len);
        for (data_export.relocations) |relocation| {
            if (relocation.offset > std.math.maxInt(usize)) return error.InvalidDevRunImage;
            const relocation_offset: usize = @intCast(relocation.offset);
            if (relocation_offset > data_export.bytes.len or @sizeOf(usize) > data_export.bytes.len - relocation_offset) {
                return error.InvalidDevRunImage;
            }
            symbol_names_len = try addNoOverflow(symbol_names_len, relocation.target_symbol_name.len);
            const target_exists = switch (relocation.kind) {
                .address => dataExportNamesContain(data_exports, relocation.target_symbol_name),
                .function_pointer => codeSymbolNamesContain(code_symbol_inputs, relocation.target_symbol_name),
            };
            if (!target_exists) return error.UnsupportedStaticDataRelocation;
        }
    }

    const function_stub_count = try countReservedFunctionStubsNoAlloc(relocations, data_exports);
    const function_stub_len = try mulNoOverflow(function_stub_count, max_jump_stub_size);

    var capacity: usize = initial_offset;
    capacity = try addAllocationCapacity(capacity, @alignOf(Header), @sizeOf(Header));
    capacity = try addAllocationCapacity(capacity, @alignOf(Entrypoint), try mulNoOverflow(entrypoint_inputs.len, @sizeOf(Entrypoint)));
    capacity = try addAllocationCapacity(capacity, @alignOf(CodeSymbol), try mulNoOverflow(code_symbol_inputs.len, @sizeOf(CodeSymbol)));
    capacity = try addAllocationCapacity(capacity, @alignOf(RelocationRecord), try mulNoOverflow(relocation_count, @sizeOf(RelocationRecord)));
    capacity = try addAllocationCapacity(capacity, @alignOf(DataRelocationRecord), try mulNoOverflow(data_relocation_count, @sizeOf(DataRelocationRecord)));
    capacity = try addAllocationCapacity(capacity, @alignOf(u8), symbol_names_len);
    capacity = try addAllocationCapacity(capacity, @alignOf(DataSymbol), try mulNoOverflow(data_exports.len, @sizeOf(DataSymbol)));
    capacity = try addAllocationCapacity(capacity, page_size, code.len);
    capacity = try addAllocationCapacity(capacity, 16, function_stub_len);
    capacity = std.mem.alignForward(usize, capacity, page_size);
    capacity = try addAllocationCapacity(capacity, @max(page_size, max_data_alignment), data_len);
    return std.mem.alignForward(usize, capacity, page_size);
}

/// Validate and view an already-mapped dev run image.
pub fn viewMappedImage(header: *const Header, base_ptr: [*]align(1) u8, mapped_size: usize) ImageError!ProgramView {
    if (header.magic != MAGIC) return error.InvalidDevRunImage;
    if (header.format_version != FORMAT_VERSION) return error.UnsupportedDevRunImageVersion;
    if (header.image_size == 0 or header.image_size > mapped_size) return error.InvalidDevRunImage;

    const image_size: usize = @intCast(header.image_size);
    const page_size = try asBoundedLen(header.page_size);
    if (page_size == 0 or !std.math.isPowerOfTwo(page_size)) return error.InvalidDevRunImage;

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

fn appendStringRef(scratch: Allocator, symbol_names: *std.ArrayList(u8), name: []const u8) Allocator.Error!StringRef {
    const offset = symbol_names.items.len;
    try symbol_names.appendSlice(scratch, name);
    return .{
        .offset = @intCast(offset),
        .len = @intCast(name.len),
    };
}

fn countReservedFunctionStubs(
    scratch: Allocator,
    relocations: []const Relocation,
    data_symbol_names: *const std.StringHashMapUnmanaged(void),
) WriteError!usize {
    var stub_names = std.StringHashMapUnmanaged(void){};
    defer stub_names.deinit(scratch);

    for (relocations) |relocation| {
        const name = switch (relocation) {
            .linked_function => |function| function.name,
            .linked_data => |data| if (data_symbol_names.contains(data.name)) continue else data.name,
            .local_data, .jmp_to_return => return error.UnsupportedDevRunRelocation,
        };
        try stub_names.put(scratch, name, {});
    }

    return stub_names.count();
}

fn countReservedFunctionStubsNoAlloc(
    relocations: []const Relocation,
    data_exports: []const StaticDataExport,
) WriteError!usize {
    var count: usize = 0;
    for (relocations, 0..) |relocation, i| {
        const name = switch (relocation) {
            .linked_function => |function| function.name,
            .linked_data => |data| if (dataExportNamesContain(data_exports, data.name)) continue else data.name,
            .local_data, .jmp_to_return => return error.UnsupportedDevRunRelocation,
        };

        for (relocations[0..i]) |previous| {
            const previous_name = switch (previous) {
                .linked_function => |function| function.name,
                .linked_data => |data| if (dataExportNamesContain(data_exports, data.name)) continue else data.name,
                .local_data, .jmp_to_return => return error.UnsupportedDevRunRelocation,
            };
            if (std.mem.eql(u8, previous_name, name)) break;
        } else {
            count = try addNoOverflow(count, 1);
        }
    }
    return count;
}

fn dataExportNamesContain(data_exports: []const StaticDataExport, name: []const u8) bool {
    for (data_exports) |data_export| {
        if (std.mem.eql(u8, data_export.symbol_name, name)) return true;
    }
    return false;
}

fn codeSymbolNamesContain(code_symbols: []const CodeSymbolInput, name: []const u8) bool {
    for (code_symbols) |symbol| {
        if (std.mem.eql(u8, symbol.name, name)) return true;
    }
    return false;
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
        .{ .name = "roc__proc_2a", .code_offset = 3 },
    };
    const relocations = [_]Relocation{
        .{ .linked_function = .{ .offset = 1, .name = "roc_alloc" } },
        .{ .linked_data = .{ .offset = 2, .name = "roc__answer", .kind = .rel32 } },
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
            .target_symbol_name = "roc__proc_2a",
            .kind = .function_pointer,
        },
    };
    const data_exports = [_]StaticDataExport{
        .{
            .symbol_name = "roc__static",
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
    const capacity = try requiredCapacity(page_size, &code, &entrypoint_inputs, &code_symbol_inputs, &relocations, &data_exports);
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
        &relocations,
        &data_exports,
    );

    try std.testing.expectEqual(MAGIC, header.magic);
    try std.testing.expectEqual(FORMAT_VERSION, header.format_version);
    try std.testing.expectEqual(@as(u64, page_size), header.page_size);
    try std.testing.expect(header.image_size <= image_bytes.len);
    try std.testing.expect(header.image_size <= capacity);

    const view = try viewMappedImage(header, image_bytes.ptr, @intCast(header.image_size));

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
    try std.testing.expectEqualStrings("roc__proc_2a", try view.codeSymbolName(view.code_symbols[0]));
    try std.testing.expectEqual(@as(u64, 3), view.code_symbols[0].code_offset);

    try std.testing.expectEqual(@as(usize, relocations.len), view.relocations.len);
    try std.testing.expectEqual(RelocationKind.linked_function, try view.relocations[0].relocationKind());
    try std.testing.expectEqual(@as(u64, 1), view.relocations[0].code_offset);
    try std.testing.expectEqualStrings("roc_alloc", try view.symbolName(view.relocations[0].symbol));
    try std.testing.expectEqual(RelocationKind.linked_data_rel32, try view.relocations[1].relocationKind());
    try std.testing.expectEqual(@as(u64, 2), view.relocations[1].code_offset);
    try std.testing.expectEqualStrings("roc__answer", try view.symbolName(view.relocations[1].symbol));

    try std.testing.expectEqualSlices(u8, &data_bytes, view.data[0..data_bytes.len]);
    try std.testing.expectEqualSlices(u8, &target_data_bytes, view.data[data_bytes.len..][0..target_data_bytes.len]);
    try std.testing.expectEqual(@as(usize, data_exports.len), view.data_symbols.len);
    try std.testing.expectEqualStrings("roc__static", try view.dataSymbolName(view.data_symbols[0]));
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
    try std.testing.expectEqualStrings("roc__proc_2a", try view.symbolName(view.data_relocations[1].symbol));
}
