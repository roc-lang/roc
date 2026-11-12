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

/// Static ownership is inline so copying an immutable step retains its identity.
pub const image_options: backend.StaticDataImage.Options = .{
    .callable_extra_bytes = callableExtraBytes,
};

const CallableTail = struct {
    header_offset: usize,
    capture_size: usize,
    source_state: usize,
    copied_state: usize,
    state_size: usize,
    extra_bytes: usize,
};

fn callableTail(export_: *const backend.StaticDataExport) CallableTail {
    const callable = builtins.erased_callable;
    const capture_start = Interpreter.static_callable_payload_offset + callable.capture_offset;
    if (export_.bytes.len < capture_start) invariant("static callable omitted its payload header");
    const size_offset = Interpreter.static_callable_payload_offset + @offsetOf(callable.Payload, "capture_size");
    const capture_size = std.mem.readInt(usize, export_.bytes[size_offset..][0..@sizeOf(usize)], .little);
    if (capture_size > export_.bytes.len - capture_start) invariant("static callable capture exceeded its allocation");
    const source_state = std.mem.alignForward(usize, capture_start + capture_size, callable.capture_alignment);
    const state_size = export_.bytes.len - @min(source_state, export_.bytes.len);
    if (state_size > builtins.boxed_sequence.state_capacity) invariant("static callable has an invalid private suffix");
    const header_offset = std.mem.alignForward(usize, export_.bytes.len, @alignOf(Interpreter.StaticCallableHeader));
    const header_end = header_offset + @sizeOf(Interpreter.StaticCallableHeader);
    const copied_state = std.mem.alignForward(usize, header_end, callable.capture_alignment);
    return .{
        .header_offset = header_offset,
        .capture_size = header_end - capture_start,
        .source_state = source_state,
        .copied_state = copied_state,
        .state_size = state_size,
        .extra_bytes = (if (state_size == 0) header_end else copied_state + state_size) - export_.bytes.len,
    };
}

fn callableExtraBytes(export_: *const backend.StaticDataExport) usize {
    return callableTail(export_).extra_bytes;
}

/// Resolve explicit callable and drop-helper relocations to interpreter trampolines.
pub fn resolveFunction(_: ?*anyopaque, relocation: backend.StaticDataRelocation) ?usize {
    if (relocation.rc_helper) |helper| return if (helper.op == .incref)
        Interpreter.staticErasedCallableOnCopyAddress()
    else
        Interpreter.staticErasedCallableOnDropAddress();
    if (relocation.callable_capture_offset == .none or relocation.procedure == .none) return null;
    return Interpreter.staticErasedCallableTrampolineAddress();
}

/// Put each frozen callable's owner inside its capture. A private sequence view
/// follows that capture; copy its relocated bytes after the new owner trailer.
pub fn writeCallableHeaders(exports: []const backend.StaticDataExport, image: *const backend.StaticDataImage, owner: *const Interpreter.StaticCallableOwner) void {
    for (exports) |export_| {
        const symbol = image.symbolAddress(export_.symbol_name) orelse invariant("interpreter image omitted a committed export");
        const allocation = std.math.sub(usize, symbol, export_.symbol_offset) catch invariant("interpreter symbol offset underflow");
        for (export_.relocations) |relocation| {
            const capture_offset = relocation.callable_capture_offset.get() orelse continue;
            if (relocation.kind != .function_pointer or relocation.rc_helper != null) invariant("interpreter callable metadata named a non-callable relocation");
            if (relocation.offset != Interpreter.static_callable_payload_offset or capture_offset != builtins.erased_callable.capture_offset) {
                invariant("static callable payload was not at its allocation's payload offset");
            }
            const tail = callableTail(&export_);
            const header: *Interpreter.StaticCallableHeader = @ptrFromInt(allocation + tail.header_offset);
            header.* = .{
                .owner = owner,
                .proc_id = @backingInt(relocation.procedure.get() orelse invariant("interpreter callable omitted its procedure")),
            };
            const data: [*]u8 = @ptrFromInt(allocation);
            builtins.erased_callable.payloadPtr(data + Interpreter.static_callable_payload_offset).capture_size = tail.capture_size;
            if (tail.state_size != 0) {
                @memcpy((data + tail.copied_state)[0..tail.state_size], (data + tail.source_state)[0..tail.state_size]);
            }
        }
    }
}

fn invariant(message: []const u8) noreturn {
    base.invariant("interpreter static-data invariant violated: {s}", .{message});
}
