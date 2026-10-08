//! Relocated immutable data and explicit callable ownership for an interpreter.
const std = @import("std");
const base = @import("base");
const backend = @import("backend");
const builtins = @import("builtins");
const Interpreter = @import("interpreter.zig").Interpreter;
const Allocator = std.mem.Allocator;

/// Owns relocated frozen values and the owner their static callables run on.
pub const InterpreterStaticData = struct {
    allocator: Allocator,
    image: backend.StaticDataImage,
    addresses: []usize,
    /// Heap-pinned so callable headers stay valid when this struct moves.
    callable_owner: *Interpreter.StaticCallableOwner,

    /// Borrows the export graph until deinit; owns its relocated bytes. Static
    /// callables crash until the consumer names their owner.
    pub fn init(allocator: Allocator, exports: []const backend.StaticDataExport, value_count: usize) Allocator.Error!InterpreterStaticData {
        const callable_owner = try allocator.create(Interpreter.StaticCallableOwner);
        errdefer allocator.destroy(callable_owner);
        callable_owner.* = .unbound;
        var image = backend.StaticDataImage.initWithOptions(allocator, exports, image_options) catch |err| switch (err) {
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
        writeCallableHeaders(exports, &image, callable_owner);
        return .{ .allocator = allocator, .image = image, .addresses = addresses, .callable_owner = callable_owner };
    }

    pub fn install(self: *const InterpreterStaticData, interpreter: *Interpreter) void {
        interpreter.setStaticData(self.addresses);
    }

    /// Run this image's static callables on `interpreter`, which must outlive
    /// every holder of a value in the image.
    pub fn ownByInterpreter(self: *const InterpreterStaticData, interpreter: *Interpreter) void {
        self.callable_owner.* = .forInterpreter(interpreter);
    }

    /// Run this image's static callables on interpreters created per call
    /// from `program`, which must outlive every holder of a value in the image.
    pub fn ownByProgram(self: *const InterpreterStaticData, program: *const Interpreter.StaticCallableProgram) void {
        self.callable_owner.* = program.callable_owner;
    }

    pub fn deinit(self: *InterpreterStaticData) void {
        self.allocator.free(self.addresses);
        self.image.deinit();
        self.allocator.destroy(self.callable_owner);
        self.* = undefined;
    }
};

/// Interpreter static-data images reserve a `StaticCallableHeader` before
/// each erased callable's allocation.
pub const image_options: backend.StaticDataImage.Options = .{
    .callable_header_size = @sizeOf(Interpreter.StaticCallableHeader),
};

/// Resolve explicit callable and drop-helper relocations to interpreter trampolines.
pub fn resolveFunction(_: ?*anyopaque, relocation: backend.StaticDataRelocation) ?usize {
    if (relocation.rc_helper != null) return Interpreter.staticErasedCallableOnDropAddress();
    if (relocation.callable_capture_offset == null or relocation.procedure == null) return null;
    return Interpreter.staticErasedCallableTrampolineAddress();
}

/// Write each frozen callable's owner and declared procedure into the header
/// that `image_options` reserved before its allocation.
pub fn writeCallableHeaders(exports: []const backend.StaticDataExport, image: *const backend.StaticDataImage, owner: *const Interpreter.StaticCallableOwner) void {
    for (exports) |export_| {
        const symbol = image.symbolAddress(export_.symbol_name) orelse invariant("interpreter image omitted a committed export");
        const allocation = std.math.sub(usize, symbol, export_.symbol_offset) catch invariant("interpreter symbol offset underflow");
        for (export_.relocations) |relocation| {
            const capture_offset = relocation.callable_capture_offset orelse continue;
            if (relocation.kind != .function_pointer or relocation.rc_helper != null) invariant("interpreter callable metadata named a non-callable relocation");
            if (relocation.offset != Interpreter.static_callable_payload_offset or capture_offset != builtins.erased_callable.capture_offset) {
                invariant("static callable payload was not at its allocation's payload offset");
            }
            const header: *Interpreter.StaticCallableHeader = @ptrFromInt(allocation - @sizeOf(Interpreter.StaticCallableHeader));
            header.* = .{
                .owner = owner,
                .proc_id = @backingInt(relocation.procedure orelse invariant("interpreter callable omitted its procedure")),
            };
        }
    }
}

fn invariant(message: []const u8) noreturn {
    base.invariant("interpreter static-data invariant violated: {s}", .{message});
}
