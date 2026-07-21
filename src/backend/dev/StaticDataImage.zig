//! In-process readonly-data image for native dev-backend execution.
//!
//! The image owns target-aligned copies of explicit `StaticDataExport` records.
//! Data-to-data relocations are resolved when the image is built; function
//! relocations are resolved after generated code has its final executable
//! address. Backends consume only the resulting symbol addresses.

const std = @import("std");

const StaticDataExport = @import("StaticDataExport.zig").StaticDataExport;
const StaticDataRelocation = @import("StaticDataExport.zig").StaticDataRelocation;

const Allocator = std.mem.Allocator;

/// Failures that indicate an invalid or unresolved static-data graph.
pub const Error = Allocator.Error || error{
    DuplicateStaticDataSymbol,
    InvalidStaticDataAlignment,
    InvalidStaticDataRelocation,
    MissingStaticDataSymbol,
    UnresolvedStaticFunction,
};

/// Resolves one explicit function-symbol relocation to an in-process address.
pub const FunctionResolver = struct {
    context: ?*anyopaque = null,
    resolve: *const fn (?*anyopaque, StaticDataRelocation) ?usize,
};

/// One owned, target-aligned immutable data image for in-process execution.
pub const StaticDataImage = struct {
    allocator: Allocator,
    allocation: []u8,
    alignment: std.mem.Alignment,
    symbols: []Symbol,
    symbol_indices: std.StringHashMapUnmanaged(usize),

    const Symbol = struct {
        data_export: *const StaticDataExport,
        allocation_offset: usize,

        fn address(self: Symbol, allocation: []const u8) usize {
            return @intFromPtr(allocation.ptr) + self.allocation_offset + self.data_export.symbol_offset;
        }
    };

    pub fn init(allocator: Allocator, exports: []const StaticDataExport) Error!StaticDataImage {
        const symbols = try allocator.alloc(Symbol, exports.len);
        var symbols_owned = true;
        errdefer if (symbols_owned) allocator.free(symbols);
        var symbol_indices: std.StringHashMapUnmanaged(usize) = .{};
        var symbol_indices_owned = true;
        errdefer if (symbol_indices_owned) symbol_indices.deinit(allocator);

        var allocation_len: usize = 0;
        var allocation_alignment: usize = 1;
        for (exports, 0..) |*static_export, index| {
            if (static_export.alignment == 0 or !std.math.isPowerOfTwo(static_export.alignment)) {
                return error.InvalidStaticDataAlignment;
            }
            if (static_export.symbol_offset > static_export.bytes.len) {
                return error.InvalidStaticDataRelocation;
            }
            const gop = try symbol_indices.getOrPut(allocator, static_export.symbol_name);
            if (gop.found_existing) return error.DuplicateStaticDataSymbol;
            gop.value_ptr.* = index;

            const alignment: usize = static_export.alignment;
            allocation_alignment = @max(allocation_alignment, alignment);
            allocation_len = alignForwardChecked(allocation_len, alignment) orelse
                return error.InvalidStaticDataAlignment;

            symbols[index] = .{
                .data_export = static_export,
                .allocation_offset = allocation_len,
            };
            allocation_len = std.math.add(usize, allocation_len, @max(static_export.bytes.len, 1)) catch
                return error.InvalidStaticDataAlignment;
        }

        const alignment = std.mem.Alignment.fromByteUnits(allocation_alignment);
        const allocation_ptr = allocator.rawAlloc(@max(allocation_len, 1), alignment, @returnAddress()) orelse
            return error.OutOfMemory;
        const allocation = allocation_ptr[0..@max(allocation_len, 1)];
        var allocation_owned = true;
        errdefer if (allocation_owned) allocator.rawFree(allocation, alignment, @returnAddress());
        @memset(allocation, 0);
        for (symbols) |symbol| {
            @memcpy(
                allocation[symbol.allocation_offset..][0..symbol.data_export.bytes.len],
                symbol.data_export.bytes,
            );
        }

        var image = StaticDataImage{
            .allocator = allocator,
            .allocation = allocation,
            .alignment = alignment,
            .symbols = symbols,
            .symbol_indices = symbol_indices,
        };
        symbols_owned = false;
        symbol_indices_owned = false;
        allocation_owned = false;
        errdefer image.deinit();
        try image.resolveDataRelocations();
        return image;
    }

    pub fn deinit(self: *StaticDataImage) void {
        self.symbol_indices.deinit(self.allocator);
        self.allocator.rawFree(self.allocation, self.alignment, @returnAddress());
        self.allocator.free(self.symbols);
        self.* = undefined;
    }

    pub fn symbolAddress(self: *const StaticDataImage, name: []const u8) ?usize {
        const index = self.symbol_indices.get(name) orelse return null;
        return self.symbols[index].address(self.allocation);
    }

    /// Return the resolved address of every compact LIR static-data value, in
    /// `StaticDataId` order. The LIR table is dense by construction, so native
    /// consumers can index this slice directly without a runtime symbol lookup.
    pub fn lirValueAddresses(self: *const StaticDataImage, allocator: Allocator, count: usize) Error![]usize {
        const addresses = try allocator.alloc(usize, count);
        errdefer allocator.free(addresses);
        for (addresses, 0..) |*address, index| {
            var name_buf: [64]u8 = undefined;
            const name = std.fmt.bufPrint(&name_buf, "roc__static_const_value_{d}", .{index}) catch
                return error.InvalidStaticDataRelocation;
            address.* = self.symbolAddress(name) orelse return error.MissingStaticDataSymbol;
        }
        return addresses;
    }

    pub fn resolveFunctionRelocations(self: *StaticDataImage, resolver: FunctionResolver) Error!void {
        for (self.symbols) |symbol| {
            for (symbol.data_export.relocations) |relocation| {
                if (relocation.kind != .function_pointer) continue;
                const target = resolver.resolve(resolver.context, relocation) orelse
                    return error.UnresolvedStaticFunction;
                try self.writeRelocation(symbol, relocation, target);
            }
        }
    }

    fn resolveDataRelocations(self: *StaticDataImage) Error!void {
        for (self.symbols) |symbol| {
            for (symbol.data_export.relocations) |relocation| {
                if (relocation.kind != .address) continue;
                const target = self.symbolAddress(relocation.target_symbol_name) orelse
                    return error.MissingStaticDataSymbol;
                try self.writeRelocation(symbol, relocation, target);
            }
        }
    }

    fn writeRelocation(
        self: *StaticDataImage,
        symbol: Symbol,
        relocation: StaticDataRelocation,
        target: usize,
    ) Error!void {
        const pointer_size = @sizeOf(usize);
        const offset: usize = @intCast(relocation.offset);
        if (offset > symbol.data_export.bytes.len or pointer_size > symbol.data_export.bytes.len - offset) {
            return error.InvalidStaticDataRelocation;
        }

        const adjusted = if (relocation.addend >= 0)
            std.math.add(usize, target, @intCast(relocation.addend)) catch return error.InvalidStaticDataRelocation
        else
            std.math.sub(usize, target, @intCast(-relocation.addend)) catch return error.InvalidStaticDataRelocation;
        const allocation_offset = symbol.allocation_offset + offset;
        std.mem.writeInt(usize, self.allocation[allocation_offset..][0..pointer_size], adjusted, .little);
    }
};

fn alignForwardChecked(value: usize, alignment: usize) ?usize {
    const mask = alignment - 1;
    const with_padding = std.math.add(usize, value, mask) catch return null;
    return with_padding & ~mask;
}

test "static data image resolves data relocations and compact LIR addresses" {
    const allocator = std.testing.allocator;
    var target_bytes = [_]u8{ 10, 20, 30, 40 };
    var root_bytes = [_]u8{0} ** @sizeOf(usize);
    const root_relocations = [_]StaticDataRelocation{.{
        .offset = 0,
        .target_symbol_name = "payload",
        .addend = 2,
    }};
    const exports = [_]StaticDataExport{
        .{
            .symbol_name = "payload",
            .bytes = &target_bytes,
            .alignment = 4,
        },
        .{
            .symbol_name = "roc__static_const_value_0",
            .bytes = &root_bytes,
            .alignment = @alignOf(usize),
            .relocations = &root_relocations,
        },
    };

    var image = try StaticDataImage.init(allocator, &exports);
    defer image.deinit();
    const addresses = try image.lirValueAddresses(allocator, 1);
    defer allocator.free(addresses);

    const target = image.symbolAddress("payload") orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(addresses[0], image.symbolAddress("roc__static_const_value_0").?);
    try std.testing.expectEqual(target + 2, @as(*align(1) const usize, @ptrFromInt(addresses[0])).*);
}

test "static data image resolves function relocations explicitly" {
    const allocator = std.testing.allocator;
    var root_bytes = [_]u8{0} ** @sizeOf(usize);
    const root_relocations = [_]StaticDataRelocation{.{
        .offset = 0,
        .target_symbol_name = "roc__proc_1",
        .kind = .function_pointer,
        .callable_capture_offset = 16,
        .procedure = @enumFromInt(1),
    }};
    const exports = [_]StaticDataExport{.{
        .symbol_name = "callable",
        .bytes = &root_bytes,
        .alignment = @alignOf(usize),
        .relocations = &root_relocations,
    }};

    var image = try StaticDataImage.init(allocator, &exports);
    defer image.deinit();
    const Resolver = struct {
        fn resolve(_: ?*anyopaque, relocation: StaticDataRelocation) ?usize {
            if (!std.mem.eql(u8, relocation.target_symbol_name, "roc__proc_1")) return null;
            if (relocation.callable_capture_offset != 16) return null;
            if (@intFromEnum(relocation.procedure orelse return null) != 1) return null;
            return 0x1234;
        }
    };
    try image.resolveFunctionRelocations(.{ .resolve = Resolver.resolve });

    const address = image.symbolAddress("callable") orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(usize, 0x1234), @as(*align(1) const usize, @ptrFromInt(address)).*);
}

test "static data image rejects and releases an unresolved data graph" {
    var root_bytes = [_]u8{0} ** @sizeOf(usize);
    const relocations = [_]StaticDataRelocation{.{
        .offset = 0,
        .target_symbol_name = "missing",
    }};
    const exports = [_]StaticDataExport{.{
        .symbol_name = "root",
        .bytes = &root_bytes,
        .alignment = @alignOf(usize),
        .relocations = &relocations,
    }};

    try std.testing.expectError(
        error.MissingStaticDataSymbol,
        StaticDataImage.init(std.testing.allocator, &exports),
    );
}
