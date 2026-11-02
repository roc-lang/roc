//! Load the dev backend's explicit sections and indexed references directly.
//! There is no object encoding, object parsing, or DWARF generation here.
//! All addresses and mutable state belong to this process, never the cache.
const std = @import("std");
const Allocator = std.mem.Allocator;
const ObjectWriter = @import("ObjectWriter.zig");
const Relocation = @import("Relocation.zig");
const ExecutableMemory = @import("ExecutableMemory.zig").ExecutableMemory;
const HostSplice = @import("HostSplice.zig");
const coff = @import("object/coff.zig");

pub const Error = Allocator.Error || error{ MappingFailed, UnresolvedSymbol, InvalidRelocation };
pub const Binding = struct { address: usize, is_function: bool };
pub const Resolver = struct {
    context: *const anyopaque,
    resolve: *const fn (*const anyopaque, []const u8) ?Binding,
};
pub const Input = struct {
    code: []const u8,
    rodata: []const u8,
    zero_fill_size: u64,
    symbols: []const ObjectWriter.Symbol,
    relocations: []const Relocation.IndexedRelocation,
    data_relocations: []const ObjectWriter.IndexedDataRelocation,
    unwind: []const coff.FunctionInfo,
};

pub const Image = struct {
    allocator: Allocator,
    executable: ExecutableMemory,
    symbols: std.StringHashMapUnmanaged(usize) = .empty,
    functions: std.StringHashMapUnmanaged(void) = .empty,
    zero_filled: []u8,
    code_locations: []const u8 = &.{},

    pub fn code(self: *const Image) []const u8 {
        return self.executable.generatedCode();
    }

    pub fn resetMutableData(self: *const Image) void {
        @memset(self.zero_filled, 0);
    }

    pub fn deinit(self: *Image) void {
        var names = self.symbols.keyIterator();
        while (names.next()) |name| self.allocator.free(name.*);
        self.symbols.deinit(self.allocator);
        self.functions.deinit(self.allocator);
        self.allocator.free(self.code_locations);
        self.executable.deinit();
    }
};

pub fn load(allocator: Allocator, input: Input, resolver: Resolver) Error!Image {
    const addresses = try allocator.alloc(usize, input.symbols.len);
    defer allocator.free(addresses);
    const calls = try allocator.alloc(usize, input.symbols.len);
    defer allocator.free(calls);
    // A function referenced only through a data relocation is still code.
    const functions = try allocator.alloc(bool, input.symbols.len);
    defer allocator.free(functions);
    for (input.symbols, functions) |symbol, *function| function.* = symbol.is_function;
    for (input.relocations) |relocation| switch (relocation) {
        .linked_function => |function| functions[@backingInt(function.symbol)] = true,
        else => {},
    };
    var stubs: std.AutoArrayHashMapUnmanaged(usize, void) = .empty;
    defer stubs.deinit(allocator);
    for (input.symbols, functions, addresses) |symbol, *function, *address| {
        if (!symbol.is_external) continue;
        const binding = resolver.resolve(resolver.context, symbol.name) orelse {
            if (@import("base").dev_diagnostics.isSet(.trace_boxy_work)) std.debug.print("boxy-work: unbound CTFE image symbol {s}\n", .{symbol.name});
            return error.UnresolvedSymbol;
        };
        std.debug.assert(!function.* or binding.is_function);
        function.* = binding.is_function;
        address.* = binding.address;
        if (function.*) try stubs.put(allocator, address.*, {});
    }
    const stub_start = std.mem.alignForward(usize, input.code.len, 16);
    const rodata_start = std.mem.alignForward(usize, stub_start + stubs.count() * 16, std.heap.page_size_min);
    const writable_start = std.mem.alignForward(usize, rodata_start + input.rodata.len, std.heap.page_size_min);
    var executable = ExecutableMemory.initWritableWithUnwind(@max(writable_start + @as(usize, @intCast(input.zero_fill_size)), 1), input.code.len, input.unwind) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.MappingFailed,
    };
    errdefer executable.deinit();
    executable.code_size = input.code.len;
    const bytes = executable.memory;
    const base = @intFromPtr(bytes.ptr);
    @memcpy(bytes[0..input.code.len], input.code);
    @memcpy(bytes[rodata_start..][0..input.rodata.len], input.rodata);
    const zero_filled = bytes[writable_start..][0..@intCast(input.zero_fill_size)];
    @memset(zero_filled, 0);
    for (stubs.keys(), 0..) |target, index| HostSplice.writeStub(bytes[stub_start + index * 16 ..][0..16], target);
    for (input.symbols, functions, addresses, calls) |symbol, function, *address, *call| {
        if (symbol.is_external) {
            call.* = if (function) base + stub_start + stubs.getIndex(address.*).? * 16 else address.*;
            // PC-relative function addresses need a nearby code entry too.
            if (function) address.* = call.*;
        } else {
            const start: usize = switch (symbol.section) {
                .text => 0,
                .rodata => rodata_start,
                .zero_fill => writable_start,
                .undef => unreachable,
            };
            address.* = base + start + @as(usize, @intCast(symbol.offset));
            call.* = address.*;
        }
    }
    Relocation.applyIndexedRelocations(bytes[0..input.code.len], base, input.relocations, addresses, calls) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.InvalidRelocation,
    };
    for (input.data_relocations) |relocation| {
        const target = addresses[@backingInt(relocation.symbol)];
        const value: usize = @intCast(@as(i128, target) + relocation.addend);
        const field = rodata_start + @as(usize, @intCast(relocation.offset));
        std.mem.writeInt(usize, bytes[field..][0..@sizeOf(usize)], value, .little);
    }
    if (writable_start != 0) executable.finishWriteExecutablePrefix(writable_start) catch return error.MappingFailed;
    var functions_map: std.StringHashMapUnmanaged(void) = .empty;
    errdefer functions_map.deinit(allocator);
    var symbols: std.StringHashMapUnmanaged(usize) = .empty;
    errdefer {
        var names = symbols.keyIterator();
        while (names.next()) |name| allocator.free(name.*);
        symbols.deinit(allocator);
    }
    try symbols.ensureTotalCapacity(allocator, @intCast(input.symbols.len));
    try functions_map.ensureTotalCapacity(allocator, @intCast(input.symbols.len));
    for (input.symbols, addresses) |symbol, address| {
        if (symbol.is_external) continue;
        const name = try allocator.dupe(u8, symbol.name);
        errdefer allocator.free(name);
        symbols.putAssumeCapacity(name, address);
        if (symbol.section == .text) functions_map.putAssumeCapacity(name, {});
    }
    return .{ .allocator = allocator, .executable = executable, .symbols = symbols, .functions = functions_map, .zero_filled = zero_filled };
}

/// A process-local entry into code already loaded by the session. The
/// instructions are emitted from an explicit address, never persisted.
pub fn providedEntry(allocator: Allocator, source: @import("CodeExcerpt.zig").Excerpt, address: usize) Allocator.Error!@import("CodeExcerpt.zig").Excerpt {
    // Existing region-relative references address the original entry offset.
    const code = try allocator.alloc(u8, source.entry + 16);
    @memset(code, 0);
    HostSplice.writeStub(code[source.entry..][0..16], address);
    return .{
        .kind = source.kind,
        .code = code,
        .entry = source.entry,
        .frame = .{ .prologue_size = 0, .stack_alloc = 0, .frame_size = 0, .callee_saved_mask = 0, .epilogue_offset = 0, .uses_frame_pointer = false },
        .refs = &.{},
        .relocations = &.{},
        .data = &.{},
    };
}

test "native CTFE image binds pointers and resets only mutable data" {
    const isa = @import("isa.zig").host orelse return error.SkipZigTest;
    const code: []const u8 = switch (isa) {
        .x86_64 => &.{ 0xb8, 42, 0, 0, 0, 0xc3 },
        .aarch64 => &.{ 0x40, 0x05, 0x80, 0xd2, 0xc0, 0x03, 0x5f, 0xd6 },
    };
    const Never = struct {
        fn resolve(_: *const anyopaque, _: []const u8) ?Binding {
            return null;
        }
    };
    var image = try load(std.testing.allocator, .{
        .code = code,
        .rodata = &(@as([8]u8, @splat(0))),
        .zero_fill_size = 8,
        .symbols = &.{
            .{ .name = "answer", .offset = 0, .size = code.len, .is_global = true, .is_function = true, .is_external = false },
            .{ .name = "pointer", .offset = 0, .size = 8, .section = .rodata, .is_global = true, .is_function = false, .is_external = false },
            .{ .name = "state", .offset = 0, .size = 8, .section = .zero_fill, .is_global = true, .is_function = false, .is_external = false },
        },
        .relocations = &.{},
        .data_relocations = &.{.{ .offset = 0, .symbol = @fromBackingInt(0) }},
        .unwind = &.{},
    }, .{ .context = &code, .resolve = Never.resolve });
    defer image.deinit();
    const pointer: *const usize = @ptrFromInt(image.symbols.get("pointer").?);
    try std.testing.expectEqual(image.symbols.get("answer").?, pointer.*);
    const answer: *const fn () callconv(.c) u64 = @ptrFromInt(pointer.*);
    try std.testing.expectEqual(@as(u64, 42), answer());
    const state: *u64 = @ptrFromInt(image.symbols.get("state").?);
    state.* = 17;
    image.resetMutableData();
    try std.testing.expectEqual(@as(u64, 0), state.*);
    try std.testing.expectEqual(@as(u64, 42), answer());
}

test "native CTFE image binds external calls through nearby stubs" {
    const isa = @import("isa.zig").host orelse return error.SkipZigTest;
    const stack: u8 = if (@import("builtin").os.tag == .windows) 40 else 8;
    const code: []const u8 = switch (isa) {
        .x86_64 => &.{ 0x48, 0x83, 0xec, stack, 0xe8, 0, 0, 0, 0, 0x48, 0x83, 0xc4, stack, 0xc3 },
        .aarch64 => &.{ 0xfd, 0x7b, 0xbf, 0xa9, 0, 0, 0, 0x94, 0xfd, 0x7b, 0xc1, 0xa8, 0xc0, 0x03, 0x5f, 0xd6 },
    };
    const Host = struct {
        fn answer() callconv(.c) u64 {
            return 37;
        }
        fn resolve(_: *const anyopaque, name: []const u8) ?Binding {
            return if (std.mem.eql(u8, name, "host")) .{ .address = @intFromPtr(&answer), .is_function = true } else null;
        }
    };
    var image = try load(std.testing.allocator, .{
        .code = code,
        .rodata = &.{},
        .zero_fill_size = 0,
        .symbols = &.{.{ .name = "host", .offset = 0, .size = 0, .section = .undef, .is_global = true, .is_function = true, .is_external = true }},
        .relocations = &.{.{ .linked_function = .{ .offset = if (isa == .x86_64) 5 else 4, .symbol = @fromBackingInt(0), .kind = if (isa == .x86_64) .x86_call_rel32 else .aarch64_call26 } }},
        .data_relocations = &.{},
        .unwind = &.{},
    }, .{ .context = &code, .resolve = Host.resolve });
    defer image.deinit();
    const answer: *const fn () callconv(.c) u64 = @ptrCast(image.code().ptr);
    try std.testing.expectEqual(@as(u64, 37), answer());
}
