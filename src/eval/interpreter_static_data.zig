//! Relocated immutable data and explicit callable registry for an interpreter.
const std = @import("std");
const backend = @import("backend");
const Interpreter = @import("interpreter.zig").Interpreter;
const Allocator = std.mem.Allocator;

/// Owns relocated frozen values and callable metadata borrowed by an interpreter.
pub const InterpreterStaticData = struct {
    allocator: Allocator,
    image: backend.StaticDataImage,
    addresses: []usize,
    callables: []Interpreter.StaticErasedCallable,

    /// Borrows the export graph until deinit; owns its relocated bytes.
    pub fn init(allocator: Allocator, exports: []const backend.StaticDataExport, value_count: usize) Allocator.Error!InterpreterStaticData {
        var image = backend.StaticDataImage.init(allocator, exports) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => invariant("invalid interpreter static-data image"),
        };
        errdefer image.deinit();
        const addresses = image.lirValueAddresses(allocator, value_count) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => invariant("interpreter data graph omitted a requested value slot"),
        };
        errdefer allocator.free(addresses);
        image.resolveFunctionRelocations(.{ .resolve = resolveFunction }) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => invariant("interpreter data graph omitted a callable procedure"),
        };
        var callables: std.ArrayList(Interpreter.StaticErasedCallable) = .empty;
        errdefer callables.deinit(allocator);
        try appendCallableMetadata(allocator, exports, &image, &callables);
        return .{ .allocator = allocator, .image = image, .addresses = addresses, .callables = try callables.toOwnedSlice(allocator) };
    }

    pub fn install(self: *const InterpreterStaticData, interpreter: *Interpreter) void {
        interpreter.setStaticData(self.addresses, self.callables);
    }

    pub fn deinit(self: *InterpreterStaticData) void {
        self.allocator.free(self.callables);
        self.allocator.free(self.addresses);
        self.image.deinit();
        self.* = undefined;
    }
};

/// Resolve explicit callable and drop-helper relocations to interpreter trampolines.
pub fn resolveFunction(_: ?*anyopaque, relocation: backend.StaticDataRelocation) ?usize {
    if (relocation.rc_helper != null) return Interpreter.staticErasedCallableOnDropAddress();
    if (relocation.callable_capture_offset == null or relocation.procedure == null) return null;
    return Interpreter.staticErasedCallableTrampolineAddress();
}

/// Register each frozen callable using its declared capture offset and procedure.
pub fn appendCallableMetadata(allocator: Allocator, exports: []const backend.StaticDataExport, image: *const backend.StaticDataImage, callables: *std.ArrayList(Interpreter.StaticErasedCallable)) Allocator.Error!void {
    for (exports) |export_| {
        const symbol = image.symbolAddress(export_.symbol_name) orelse invariant("interpreter image omitted a committed export");
        const allocation = std.math.sub(usize, symbol, export_.symbol_offset) catch invariant("interpreter symbol offset underflow");
        for (export_.relocations) |relocation| {
            const capture_offset = relocation.callable_capture_offset orelse continue;
            if (relocation.kind != .function_pointer or relocation.rc_helper != null) invariant("interpreter callable metadata named a non-callable relocation");
            const payload = std.math.add(usize, allocation, @intCast(relocation.offset)) catch invariant("interpreter callable address overflow");
            const capture = std.math.add(usize, payload, capture_offset) catch invariant("interpreter capture address overflow");
            try callables.append(allocator, .{ .capture_ptr = @ptrFromInt(capture), .proc_id = relocation.procedure orelse invariant("interpreter callable omitted its procedure") });
        }
    }
}

fn invariant(message: []const u8) noreturn {
    std.debug.panic("interpreter static-data invariant violated: {s}", .{message});
}
