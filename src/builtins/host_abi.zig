//! Since all of these can be passed across the host boundary, and since compiled Roc
//! programs must not depend on libc or on Zig's standard library, it's important that
//! none of these operations depend on any Zig types like slices or Allocator.
//!
//! Roc calls the host using the platform C ABI. Every host operation — the memory and
//! diagnostic callbacks in `RocOps`, and each platform-provided hosted function — takes a
//! leading `*RocOps` argument (the host's operation table, through which the host can reach
//! its own `env` context) and then its remaining arguments and return value in whatever
//! registers/memory the target's C ABI prescribes for their types.
//!
//! Hosted functions are exposed to the host with their natural C signatures (see
//! `src/layout/abi`), so small arguments and returns travel in registers exactly as a
//! hand-written C function would. A hosted function receives the leading `*RocOps` only when
//! its argument or return layouts require Roc memory management; otherwise it is a bare C
//! call.

const tracy = @import("tracy");

/// A type-erased pointer to a platform-provided hosted function.
///
/// Each hosted function will have its own natural C signature, determined by its Roc argument
/// and return types under the target C ABI, so there is no single concrete function-pointer
/// type for all of them. They are stored type-erased in `HostedFunctions` and cast back to
/// their concrete signature at the Roc call site (which knows the types).
///
/// (For now, hosted functions are still invoked through the uniform `(ops, ret_ptr, args_ptr)`
/// shape until the per-signature C-ABI call path lands; the erased pointer type below keeps
/// that working. The interpreter will bridge to the natural C ABI via a fixed assembly
/// register-image trampoline, and the compiled backends will emit direct C-ABI calls.)
///
/// Roc transfers ownership of refcounted arguments to hosted functions. A host function must
/// decref each owned refcounted argument when it is done with it, or transfer that ownership
/// into its return value or longer-lived storage. If the host keeps both the call argument
/// and a stored copy, it must incref the stored copy so each live reference has one
/// ownership.
/// How builtins and compiled Roc code reach the host's runtime operations.
pub const HostCallMode = enum {
    /// Through the RocOps vtable parameter (the interpreter, compiler-internal
    /// evaluation, and any host that constructs a RocOps).
    vtable,
    /// Through linker-resolved extern symbols (compiled output). The *RocOps
    /// parameters threaded through builtins are inert in this mode; the
    /// methods below ignore them and call the extern symbols directly.
    extern_symbols,
};

/// Selected by the root module (like std_options): declaring
/// `pub const roc_host_call_mode: host_abi.HostCallMode = .extern_symbols;`
/// at the root switches builtins to direct extern host calls.
pub const host_call_mode: HostCallMode = if (@hasDecl(@import("root"), "roc_host_call_mode"))
    @import("root").roc_host_call_mode
else
    .vtable;

/// The fixed runtime symbols every host defines under the symbol ABI.
const extern_host = struct {
    extern fn roc_alloc(length: usize, alignment: usize) ?*anyopaque;
    extern fn roc_dealloc(ptr: *anyopaque, alignment: usize) void;
    extern fn roc_realloc(ptr: *anyopaque, new_length: usize, alignment: usize) ?*anyopaque;
    extern fn roc_dbg(bytes: [*]const u8, len: usize) void;
    extern fn roc_expect_failed(bytes: [*]const u8, len: usize) void;
    extern fn roc_crashed(bytes: [*]const u8, len: usize) void;
};

/// Type-erased pointer to a hosted platform function stored in the
/// interpreter-internal vtable. The interpreter's trampoline rebuilds each
/// function's natural C ABI from its layouts before calling through this.
pub const HostedFn = *const fn (*anyopaque, *anyopaque, *anyopaque) callconv(.c) void;

/// Type-erase a concrete hosted function pointer to `HostedFn` for storage in the vtable.
pub fn hostedFn(func: anytype) HostedFn {
    return @ptrCast(func);
}

/// Array of hosted function pointers provided by the platform.
///
/// Dispatch is positional: the interpreter calls `fns[i]`. For a
/// typical platform — hosted functions in a single module — `i` is the
/// function's alphabetical index by `Module.fn_name` (trailing `!`
/// stripped). When hosted functions span multiple modules the ordering
/// rule is more involved; see `src/compile/README.md` ("Host functions").
/// Wrong order is silent: the wrong function gets called.
pub const HostedFunctions = extern struct {
    count: u32,
    fns: [*]HostedFn,
};

const empty_hosted_fns = struct {
    fn dummyHostedFn(_: *anyopaque, _: *anyopaque, _: *anyopaque) callconv(.c) void {}

    var fns: [1]HostedFn = .{hostedFn(&dummyHostedFn)};
};

/// Return a valid empty hosted function table for callers that don't expose any
/// platform functions but still need an initialized `RocOps.hosted_fns`.
pub fn emptyHostedFunctions() HostedFunctions {
    return .{ .count = 0, .fns = &empty_hosted_fns.fns };
}

/// Operations that the host provides to Roc code, including memory management,
/// panic handling, and platform-specific effects.
pub const RocOps = extern struct {
    /// The host's own context pointer. Roc never interprets it; the host's callbacks reach it
    /// through their leading `*RocOps` argument (`ops.env`) to find things like an arena
    /// allocator. May be null if the host has no context.
    env: *anyopaque,
    /// Allocate `length` bytes aligned to `alignment`, returning the allocation (or null on
    /// OOM — see below). Similar to `_aligned_malloc`.
    ///
    /// A host that cannot provide a non-null pointer (e.g. due to OOM) must not return a real
    /// pointer; a platform host aborts, while the compiler-internal host returns `null` so the
    /// surrounding interpreter can turn it into a Roc crash. (`null` and a real pointer share
    /// the same representation, so codegen and platform hosts that always succeed are
    /// unaffected.)
    roc_alloc: *const fn (*RocOps, usize, usize) callconv(.c) ?*anyopaque,
    /// Free the allocation at `ptr` that had alignment `alignment`. Similar to `_aligned_free`.
    /// (The length is not provided, because seamless slices make it unknown at runtime.)
    roc_dealloc: *const fn (*RocOps, *anyopaque, usize) callconv(.c) void,
    /// Reallocate `ptr` to `new_length` bytes aligned to `alignment`, returning the new
    /// allocation (or null on OOM, as for `roc_alloc`). Similar to `_aligned_realloc`.
    roc_realloc: *const fn (*RocOps, *anyopaque, usize, usize) callconv(.c) ?*anyopaque,
    /// Called when the Roc program runs `dbg`, with the UTF-8 message bytes and length.
    /// The bytes are non-null but not guaranteed to be null-terminated.
    roc_dbg: *const fn (*RocOps, [*]const u8, usize) callconv(.c) void,
    /// Called when an inline `expect` fails, with the UTF-8 message bytes and length.
    roc_expect_failed: *const fn (*RocOps, [*]const u8, usize) callconv(.c) void,
    /// Called when the Roc program crashes (e.g. integer overflow), with the UTF-8 message
    /// bytes and length. The host must stop execution of the Roc program and not return to it
    /// (a platform host aborts; the compiler-internal host longjmps out), so it never returns
    /// to Roc — but the type stays `void` since a longjmp-based host is not statically noreturn.
    roc_crashed: *const fn (*RocOps, [*]const u8, usize) callconv(.c) void,
    /// Hosted functions provided by the platform (sorted alphabetically by name).
    /// These are effectful operations like I/O that the platform provides to Type Modules.
    hosted_fns: HostedFunctions,

    /// Helper to crash the Roc program. The host does not return control to Roc.
    pub fn crash(self: *RocOps, msg: []const u8) void {
        const trace = tracy.trace(@src());
        defer trace.end();

        switch (comptime host_call_mode) {
            .vtable => self.roc_crashed(self, msg.ptr, msg.len),
            .extern_symbols => extern_host.roc_crashed(msg.ptr, msg.len),
        }
    }

    /// Helper to send debug output to the host.
    pub fn dbg(self: *RocOps, msg: []const u8) void {
        const trace = tracy.trace(@src());
        defer trace.end();

        switch (comptime host_call_mode) {
            .vtable => self.roc_dbg(self, msg.ptr, msg.len),
            .extern_symbols => extern_host.roc_dbg(msg.ptr, msg.len),
        }
    }

    /// Helper to report a failed `expect` to the host.
    pub fn expectFailed(self: *RocOps, msg: []const u8) void {
        const trace = tracy.trace(@src());
        defer trace.end();

        switch (comptime host_call_mode) {
            .vtable => self.roc_expect_failed(self, msg.ptr, msg.len),
            .extern_symbols => extern_host.roc_expect_failed(msg.ptr, msg.len),
        }
    }

    pub fn alloc(self: *RocOps, alignment: usize, length: usize) *anyopaque {
        const trace = tracy.trace(@src());
        defer trace.end();

        const answer = self.tryAlloc(length, alignment);

        if (tracy.enable_allocation) {
            tracy.alloc(@ptrCast(answer), length);
        }

        return answer.?;
    }

    /// Allocate, returning null on OOM exactly as the host reported it.
    pub fn tryAlloc(self: *RocOps, length: usize, alignment: usize) ?*anyopaque {
        return switch (comptime host_call_mode) {
            .vtable => self.roc_alloc(self, length, alignment),
            .extern_symbols => extern_host.roc_alloc(length, alignment),
        };
    }

    /// Reallocate, returning null on OOM exactly as the host reported it.
    pub fn tryRealloc(self: *RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) ?*anyopaque {
        return switch (comptime host_call_mode) {
            .vtable => self.roc_realloc(self, ptr, new_length, alignment),
            .extern_symbols => extern_host.roc_realloc(ptr, new_length, alignment),
        };
    }

    pub fn dealloc(self: *RocOps, ptr: *anyopaque, alignment: usize) void {
        const trace = tracy.trace(@src());
        defer trace.end();

        if (tracy.enable_allocation) {
            tracy.free(@ptrCast(ptr));
        }

        switch (comptime host_call_mode) {
            .vtable => self.roc_dealloc(self, ptr, alignment),
            .extern_symbols => extern_host.roc_dealloc(ptr, alignment),
        }
    }
};
