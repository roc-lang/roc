//! In-process readonly-data image for native dev-backend execution.
//!
//! The image holds target-aligned copies of explicit `StaticDataExport`
//! records, in memory it allocates or in memory its caller placed it in
//! (`Layout.place`). Data-to-data relocations are resolved when the image is
//! built; function relocations are resolved after generated code has its
//! final executable address. Backends consume only the resulting symbol
//! addresses.

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
    /// Whether the image allocated `allocation`, rather than being placed in
    /// memory its caller owns.
    owns_allocation: bool = true,
    symbols: []Symbol,
    symbol_indices: std.StringHashMapUnmanaged(usize),

    const Symbol = struct {
        data_export: *const StaticDataExport,
        allocation_offset: usize,

        fn address(self: Symbol, allocation: []const u8) usize {
            return @intFromPtr(allocation.ptr) + self.allocation_offset + self.data_export.symbol_offset;
        }
    };

    /// Layout choices an in-process consumer makes for its own copy of the graph.
    pub const Options = struct {
        /// Bytes reserved immediately before every export that holds an erased
        /// callable's code relocation, for the consumer's per-callable header.
        callable_header_size: usize = 0,
    };

    pub fn init(allocator: Allocator, exports: []const StaticDataExport) Error!StaticDataImage {
        return initWithOptions(allocator, exports, .{});
    }

    pub fn initWithOptions(allocator: Allocator, exports: []const StaticDataExport, options: Options) Error!StaticDataImage {
        var planned = try Layout.plan(allocator, exports, options);
        const alignment = planned.alignment;
        const allocation_ptr = allocator.rawAlloc(planned.size, alignment, @returnAddress()) orelse {
            planned.deinit(allocator);
            return error.OutOfMemory;
        };
        const allocation = allocation_ptr[0..planned.size];
        // Placing takes the layout's tables, and releases them if it fails.
        var image = planned.place(allocator, allocation) catch |err| {
            allocator.rawFree(allocation, alignment, @returnAddress());
            return err;
        };
        image.owns_allocation = true;
        return image;
    }

    /// Where every export of a graph goes in one image, before any memory
    /// for it exists.
    pub const Layout = struct {
        symbols: []Symbol,
        symbol_indices: std.StringHashMapUnmanaged(usize),
        /// Bytes the image occupies, at least one.
        size: usize,
        alignment: std.mem.Alignment,

        pub fn plan(allocator: Allocator, exports: []const StaticDataExport, options: Options) Error!Layout {
            const symbols = try allocator.alloc(Symbol, exports.len);
            errdefer allocator.free(symbols);
            var symbol_indices: std.StringHashMapUnmanaged(usize) = .{};
            errdefer symbol_indices.deinit(allocator);

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
                if (options.callable_header_size != 0 and exportHoldsCallable(static_export)) {
                    allocation_len = std.math.add(usize, allocation_len, options.callable_header_size) catch
                        return error.InvalidStaticDataAlignment;
                }
                allocation_len = alignForwardChecked(allocation_len, alignment) orelse
                    return error.InvalidStaticDataAlignment;

                symbols[index] = .{
                    .data_export = static_export,
                    .allocation_offset = allocation_len,
                };
                allocation_len = std.math.add(usize, allocation_len, @max(static_export.bytes.len, 1)) catch
                    return error.InvalidStaticDataAlignment;
            }
            return .{
                .symbols = symbols,
                .symbol_indices = symbol_indices,
                .size = @max(allocation_len, 1),
                .alignment = std.mem.Alignment.fromByteUnits(allocation_alignment),
            };
        }

        /// The offset in the image of the symbol `name`.
        pub fn symbolOffset(self: *const Layout, name: []const u8) ?usize {
            const index = self.symbol_indices.get(name) orelse return null;
            const symbol = self.symbols[index];
            return symbol.allocation_offset + symbol.data_export.symbol_offset;
        }

        /// Build the image in `memory`, which must be `size` bytes at
        /// `alignment` and outlive the image. The layout's tables move into
        /// the image.
        pub fn place(self: *Layout, allocator: Allocator, memory: []u8) Error!StaticDataImage {
            std.debug.assert(memory.len == self.size and self.alignment.check(@intFromPtr(memory.ptr)));
            @memset(memory, 0);
            for (self.symbols) |symbol| {
                @memcpy(
                    memory[symbol.allocation_offset..][0..symbol.data_export.bytes.len],
                    symbol.data_export.bytes,
                );
            }
            var image = StaticDataImage{
                .allocator = allocator,
                .allocation = memory,
                .alignment = self.alignment,
                .owns_allocation = false,
                .symbols = self.symbols,
                .symbol_indices = self.symbol_indices,
            };
            self.* = undefined;
            errdefer {
                image.symbol_indices.deinit(allocator);
                allocator.free(image.symbols);
            }
            try image.resolveDataRelocations();
            return image;
        }

        pub fn deinit(self: *Layout, allocator: Allocator) void {
            self.symbol_indices.deinit(allocator);
            allocator.free(self.symbols);
            self.* = undefined;
        }
    };

    pub fn deinit(self: *StaticDataImage) void {
        self.symbol_indices.deinit(self.allocator);
        if (self.owns_allocation) self.allocator.rawFree(self.allocation, self.alignment, @returnAddress());
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
        @memset(addresses, 0);
        for (self.symbols) |symbol| {
            if (symbol.data_export.value_id) |id| {
                const index = @backingInt(id);
                std.debug.assert(index < count and addresses[index] == 0);
                addresses[index] = symbol.address(self.allocation);
            }
        }
        for (addresses) |address| std.debug.assert(address != 0);
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
                const target = switch (relocation.target) {
                    .data_symbol => |id| self.symbols[@backingInt(id)].address(self.allocation),
                    .named => self.symbolAddress(relocation.target_symbol_name) orelse return error.MissingStaticDataSymbol,
                };
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

fn exportHoldsCallable(static_export: *const StaticDataExport) bool {
    for (static_export.relocations) |relocation| {
        if (relocation.kind == .function_pointer and relocation.callable_capture_offset != null) return true;
    }
    return false;
}

fn alignForwardChecked(value: usize, alignment: usize) ?usize {
    const mask = alignment - 1;
    const with_padding = std.math.add(usize, value, mask) catch return null;
    return with_padding & ~mask;
}

test "static data image resolves data relocations and compact LIR addresses" {
    const allocator = std.testing.allocator;
    var target_bytes = [_]u8{ 10, 20, 30, 40 };
    var root_bytes = @as([@sizeOf(usize)]u8, @splat(0));
    const root_relocations = [_]StaticDataRelocation{.{
        .offset = 0,
        .target_symbol_name = "payload",
        .addend = 2,
    }};
    var exports = [_]StaticDataExport{
        .{
            .symbol_name = "payload",
            .bytes = &target_bytes,
            .alignment = 4,
        },
        .{
            .symbol_name = "root",
            .bytes = &root_bytes,
            .alignment = @alignOf(usize),
            .relocations = &root_relocations,
        },
    };

    // The fixture's dense root table names the export representing each LIR value.
    const roots = [_]usize{1};
    for (roots, 0..) |export_index, value_index| exports[export_index].value_id = @fromBackingInt(@intCast(value_index));
    var image = try StaticDataImage.init(allocator, &exports);
    defer image.deinit();
    const addresses = try image.lirValueAddresses(allocator, roots.len);
    defer allocator.free(addresses);

    const target = image.symbolAddress("payload") orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(addresses[0], image.symbolAddress("root").?);
    try std.testing.expectEqual(target + 2, @as(*align(1) const usize, @ptrFromInt(addresses[0])).*);
}

test "static data image resolves function relocations explicitly" {
    const allocator = std.testing.allocator;
    var root_bytes = @as([@sizeOf(usize)]u8, @splat(0));
    const root_relocations = [_]StaticDataRelocation{.{
        .offset = 0,
        .target_symbol_name = "roc__p1",
        .kind = .function_pointer,
        .callable_capture_offset = 16,
        .procedure = @fromBackingInt(@intCast(1)),
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
            if (!std.mem.eql(u8, relocation.target_symbol_name, "roc__p1")) return null;
            if (relocation.callable_capture_offset != 16) return null;
            if (@backingInt(relocation.procedure orelse return null) != 1) return null;
            return 0x1234;
        }
    };
    try image.resolveFunctionRelocations(.{ .resolve = Resolver.resolve });

    const address = image.symbolAddress("callable") orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(usize, 0x1234), @as(*align(1) const usize, @ptrFromInt(address)).*);
}

test "static data image rejects and releases an unresolved data graph" {
    var root_bytes = @as([@sizeOf(usize)]u8, @splat(0));
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

test "static data image resolves cyclic allocation identities" {
    const bytes = @as([@sizeOf(usize)]u8, @splat(0));
    const names = [_][]const u8{ "first", "second" };
    var relocations: [names.len]StaticDataRelocation = undefined;
    var exports: [names.len]StaticDataExport = undefined;
    for (names, &exports, &relocations, 0..) |name, *data_export, *relocation, i| {
        const next = (i + 1) % names.len;
        relocation.* = .{ .offset = 0, .target_symbol_name = names[next], .target = .{ .data_symbol = @fromBackingInt(@intCast(next)) } };
        data_export.* = .{ .symbol_name = name, .bytes = &bytes, .alignment = @alignOf(usize), .relocations = relocations[i..][0..1] };
    }
    var image = try StaticDataImage.init(std.testing.allocator, &exports);
    defer image.deinit();
    const first = image.symbolAddress("first").?;
    const second = image.symbolAddress("second").?;
    try std.testing.expectEqual(second, @as(*const usize, @ptrFromInt(first)).*);
    try std.testing.expectEqual(first, @as(*const usize, @ptrFromInt(second)).*);
}
