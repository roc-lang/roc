//! Since all of these can be passed across the host boundary, and since compiled Roc
//! programs must not depend on libc or on Zig's standard library, it's important that
//! none of these operations depend on any Zig types like slices or Allocator.
//!
//! All Roc functions that are exposed to the host return `void` and take a `*RocOps`
//! followed by the address of the return type (which the Roc function will write into)
//! and varargs (all pointers) to arguments to pass into the Roc function.
//!
//! This design makes Roc's ABI very simple; the calling convention is just "Ops pointer,
//! return pointer, args pointers".

/// todo: describe RocCall
pub const RocCall = fn (
    /// Function pointers that the Roc program uses, e.g. alloc, dealloc, etc.
    *RocOps,
    /// The Roc function will write its returned value into this address.
    ///
    /// (If the Roc function returns a zero-sized type like `{}`, it will
    /// not write anything into this address.)
    ///
    /// The host must ensure that this address points to enough memory for the
    /// Roc function to write its entire return value into the address.
    *anyopaque,
    /// Varargs, all of which are pointers (to arguments that will be passed
    /// into the Roc function). Arguments with small sizes can be combined into
    /// a struct, and a pointer to that struct can be passed as one of these.
    anytype,
) callconv(.C) void;

/// The operations (in the form of function pointers) that a running Roc program
/// needs the host to provide.
///
/// This is used in both calls from actual hosts as well as evaluation of constants
/// inside the Roc compiler itself.
pub const RocOps = struct {
    /// The host provides this pointer, and Roc passes it to each of the following
    /// function pointers as a second argument. This lets the host do things like use
    /// arena allocators for allocation and deallocation (by putting the arena in here).
    /// The pointer can be to absolutely anything the host likes, or null if unused.
    env: *anyopaque,
    /// Similar to  _aligned_malloc - https://learn.microsoft.com/en-us/cpp/c-runtime-library/reference/aligned-malloc
    roc_alloc: *const fn (*RocAlloc, *anyopaque) callconv(.C) void,
    /// Similar to  _aligned_free - https://learn.microsoft.com/en-us/cpp/c-runtime-library/reference/aligned-free
    roc_dealloc: *const fn (*RocDealloc, *anyopaque) callconv(.C) void,
    /// Similar to  _aligned_realloc - https://learn.microsoft.com/en-us/cpp/c-runtime-library/reference/aligned-realloc
    roc_realloc: *const fn (*RocRealloc, *anyopaque) callconv(.C) void,
    /// Called when the Roc program has called `dbg` on something.
    roc_dbg: *const fn (*const RocDbg, *anyopaque) callconv(.C) void,
    /// Called when the Roc program has run an `expect` which failed.
    roc_expect_failed: *const fn (*const RocExpectFailed, *anyopaque) callconv(.C) void,
    /// Called when the Roc program crashes, e.g. due to integer overflow.
    /// This function must not return, because the Roc program assumes it will
    /// not continue to be executed after this function is called.
    roc_crashed: *const fn (*const RocCrashed, *anyopaque) callconv(.C) noreturn,
    /// At the end of this struct, the host must include all the functions
    /// it wants to provide to the Roc program for the Roc program to call
    /// (e.g. I/O operations and such).
    host_fns: *anyopaque,

    /// Helper function to crash the Roc program, returns control to the host.
    pub fn crash(self: *RocOps, msg: []const u8) noreturn {
        const roc_crashed_args = RocCrashed{
            .utf8_bytes = @constCast(msg.ptr),
            .len = msg.len,
        };
        self.roc_crashed(&roc_crashed_args, self);
    }
};

/// When RocOps.roc_alloc gets called, it will be passed one of these.
/// That function should write the address to the allocated memory into `ret`.
/// If it cannot provide a non-null pointer (e.g. due to OOM), it
/// must not return, and must instead do something along the lines
/// of roc_crashed.
pub const RocAlloc = extern struct {
    alignment: usize,
    length: usize,
    answer: *anyopaque,
};

/// When RocOps.roc_dealloc gets called, it will be passed one of these.
/// (The length of the allocation cannot be provided, because it is
/// not always known at runtime due to the way seamless slices work.)
pub const RocDealloc = extern struct {
    alignment: usize,
    ptr: *anyopaque,
};

/// When RocOps.roc_realloc gets called, it will be passed one of these.
/// That function should write the allocated memory into `ret`.
/// If it cannot provide a non-null pointer (e.g. due to OOM), it
/// must not return, and must instead do something along the lines
/// of roc_crashed.
pub const RocRealloc = extern struct {
    alignment: usize,
    new_length: usize,
    answer: *anyopaque,
};

/// The UTF-8 string message the host receives when a Roc program crashes
/// (e.g. due to integer overflow).
///
/// The pointer to the UTF-8 bytes is guaranteed to be non-null,
/// but it is *not* guaranteed to be null-terminated.
pub const RocCrashed = extern struct {
    utf8_bytes: [*]u8,
    len: usize,
};

/// The information the host receives when a Roc program runs a `dbg`.
///
/// The pointer to the UTF-8 bytes is guaranteed to be non-null,
/// but it is *not* guaranteed to be null-terminated.
pub const RocDbg = extern struct {
    // TODO make this be structured instead of just a string, so that
    // the host can format things more nicely (e.g. syntax highlighting).
    utf8_bytes: [*]u8,
    len: usize,
};

/// The information the host receives when a Roc program runs an inline `expect`
/// which fails.
///
/// The pointer to the UTF-8 bytes is guaranteed to be non-null,
/// but it is *not* guaranteed to be null-terminated.
pub const RocExpectFailed = extern struct {
    // TODO make this be structured instead of just a string, so that
    // the host can format things more nicely (e.g. syntax highlighting).
    utf8_bytes: [*]u8,
    len: usize,
};
