//! Shared-memory image for dev backend execution through the interpreter shim.
//!
//! The parent process emits machine code plus explicit relocation and readonly
//! data records. The child shim maps the image, patches those records into an
//! executable allocation, and calls the requested Roc ABI entrypoint wrapper.

const std = @import("std");
const Relocation = @import("Relocation.zig").Relocation;
const DataRelocationKind = @import("Relocation.zig").DataRelocationKind;
const StaticDataExport = @import("StaticDataExport.zig").StaticDataExport;

const Allocator = std.mem.Allocator;

/// Magic number identifying a dev backend run image ("RDEV" in little-endian bytes).
pub const MAGIC: u32 = 0x56454452;

/// Version of the shared-memory dev run image format.
pub const FORMAT_VERSION: u32 = 1;

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
    code: ArrayRef,
    entrypoints: ArrayRef,
    relocations: ArrayRef,
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

/// Entrypoint metadata provided by codegen before the image is serialized.
pub const EntrypointInput = struct {
    ordinal: u32,
    code_offset: usize,
};

/// Borrowed view of a validated dev run image mapped in the shim process.
pub const ProgramView = struct {
    code: []const u8,
    entrypoints: []const Entrypoint,
    relocations: []const RelocationRecord,
    symbol_names: []const u8,
    data: []const u8,
    data_symbols: []const DataSymbol,

    pub fn symbolName(self: *const ProgramView, ref: StringRef) ImageError![]const u8 {
        const start = try asBoundedOffset(ref.offset, self.symbol_names.len);
        const len = try asBoundedLen(ref.len);
        if (len > self.symbol_names.len or start > self.symbol_names.len - len) return error.InvalidDevRunImage;
        return self.symbol_names[start..][0..len];
    }

    pub fn dataSymbolName(self: *const ProgramView, symbol: DataSymbol) ImageError![]const u8 {
        return self.symbolName(symbol.name);
    }
};

/// Serialize dev backend machine code, entrypoints, relocations, and data into shared memory.
pub fn writeToSharedMemory(
    scratch: Allocator,
    image_allocator: Allocator,
    base_ptr: [*]align(1) u8,
    code: []const u8,
    entrypoint_inputs: []const EntrypointInput,
    relocations: []const Relocation,
    data_exports: []const StaticDataExport,
) WriteError!*Header {
    const header = try image_allocator.create(Header);
    header.* = undefined;

    var symbol_names = std.ArrayList(u8).empty;
    defer symbol_names.deinit(scratch);

    var relocation_records = std.ArrayList(RelocationRecord).empty;
    defer relocation_records.deinit(scratch);

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

    var max_data_alignment: usize = 1;
    for (data_exports) |data_export| {
        if (data_export.relocations.len != 0) return error.UnsupportedStaticDataRelocation;
        const alignment = if (data_export.alignment == 0) 1 else data_export.alignment;
        if (!std.math.isPowerOfTwo(alignment)) return error.InvalidStaticDataAlignment;
        max_data_alignment = @max(max_data_alignment, alignment);

        const aligned_offset = std.mem.alignForward(usize, data_bytes.items.len, alignment);
        try data_bytes.appendNTimes(scratch, 0, aligned_offset - data_bytes.items.len);
        const data_offset = data_bytes.items.len;
        try data_bytes.appendSlice(scratch, data_export.bytes);

        try data_symbols.append(scratch, .{
            .name = try appendStringRef(scratch, &symbol_names, data_export.symbol_name),
            .data_offset = @intCast(data_offset),
            .len = @intCast(data_export.bytes.len),
            .symbol_offset = data_export.symbol_offset,
            .alignment = alignment,
        });
    }

    const entrypoints = try image_allocator.alloc(Entrypoint, entrypoint_inputs.len);
    for (entrypoint_inputs, entrypoints) |input, *entrypoint| {
        entrypoint.* = .{
            .ordinal = input.ordinal,
            .code_offset = @intCast(input.code_offset),
        };
    }

    const code_copy = try image_allocator.alignedAlloc(u8, .@"16", code.len);
    @memcpy(code_copy, code);

    const relocation_copy = try image_allocator.alloc(RelocationRecord, relocation_records.items.len);
    @memcpy(relocation_copy, relocation_records.items);

    const symbol_names_copy = try image_allocator.alloc(u8, symbol_names.items.len);
    @memcpy(symbol_names_copy, symbol_names.items);

    const data_copy = try allocRuntimeAlignedBytes(
        image_allocator,
        std.mem.Alignment.fromByteUnits(max_data_alignment),
        data_bytes.items.len,
    );
    @memcpy(data_copy, data_bytes.items);

    const data_symbols_copy = try image_allocator.alloc(DataSymbol, data_symbols.items.len);
    @memcpy(data_symbols_copy, data_symbols.items);

    const image_size = maxEnd(base_ptr, &.{
        bytesOf(header),
        code_copy,
        bytesOfSlice(entrypoints),
        bytesOfSlice(relocation_copy),
        symbol_names_copy,
        data_copy,
        bytesOfSlice(data_symbols_copy),
    });

    header.* = .{
        .magic = MAGIC,
        .format_version = FORMAT_VERSION,
        .image_size = @intCast(image_size),
        .code = try arrayRef(base_ptr, code_copy),
        .entrypoints = try arrayRef(base_ptr, bytesOfSlice(entrypoints)),
        .relocations = try arrayRef(base_ptr, bytesOfSlice(relocation_copy)),
        .symbol_names = try arrayRef(base_ptr, symbol_names_copy),
        .data = try arrayRef(base_ptr, data_copy),
        .data_symbols = try arrayRef(base_ptr, bytesOfSlice(data_symbols_copy)),
    };

    return header;
}

/// Validate and view an already-mapped dev run image.
pub fn viewMappedImage(header: *const Header, base_ptr: [*]align(1) u8, mapped_size: usize) ImageError!ProgramView {
    if (header.magic != MAGIC) return error.InvalidDevRunImage;
    if (header.format_version != FORMAT_VERSION) return error.UnsupportedDevRunImageVersion;
    if (header.image_size == 0 or header.image_size > mapped_size) return error.InvalidDevRunImage;

    const image_size: usize = @intCast(header.image_size);
    return .{
        .code = try bytesFromRef(base_ptr, image_size, header.code),
        .entrypoints = try sliceFromRef(Entrypoint, base_ptr, image_size, header.entrypoints),
        .relocations = try sliceFromRef(RelocationRecord, base_ptr, image_size, header.relocations),
        .symbol_names = try bytesFromRef(base_ptr, image_size, header.symbol_names),
        .data = try bytesFromRef(base_ptr, image_size, header.data),
        .data_symbols = try sliceFromRef(DataSymbol, base_ptr, image_size, header.data_symbols),
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

fn bytesFromRef(base_ptr: [*]align(1) u8, image_size: usize, ref: ArrayRef) ImageError![]const u8 {
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
