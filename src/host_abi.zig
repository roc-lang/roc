/// All Roc functions that are exposed to the host take 1 argument and return void.
/// The 1 argument is a pointer to one of these structs, which includes an address
/// that the Roc call will write the return value into. This design makes Roc's ABI
/// very simple; the calling convention is just "pass a single pointer".
pub fn RocCall(
    comptime Arg: type,
    comptime Ret: type,
    comptime CallEnv: type,
) type {
    return struct {
        /// Function pointers that the Roc program uses, e.g. alloc, dealloc, etc.
        ops: *RocOps(CallEnv),
        /// The argument that the Roc entrypoint function will receive from the host.
        /// (If multiple arguments are needed, this should be a pointer to a struct.)
        arg: *Arg,
        /// What the Roc entrypoint function will return to the host.
        /// (The caller should have allocated enough space for the Roc function to write
        /// the entire return value into this.)
        ret: *Ret,
    };
}

/// The operations (in the form of function pointers) that a running Roc program
/// needs the host to provide.
///
/// This is used in both calls from actual hosts as well as evaluation of constants
/// inside the Roc compiler itself.
pub fn RocOps(comptime CallEnv: type) type {
    return struct {
        /// The host provides this pointer, and Roc passes it to each of the following
        /// function pointers as a second argument. This lets the host do things like use
        /// arena allocators for allocation and deallocation (by putting the arena in here).
        /// The pointer can be to absolutely anything the host likes, or null if unused.
        env: *CallEnv,
        /// Similar to  _aligned_malloc - https://learn.microsoft.com/en-us/cpp/c-runtime-library/reference/aligned-malloc
        roc_alloc: fn (*RocAlloc, *CallEnv) void,
        /// Similar to  _aligned_free - https://learn.microsoft.com/en-us/cpp/c-runtime-library/reference/aligned-free
        roc_dealloc: fn (*RocDealloc, *CallEnv) void,
        /// Similar to  _aligned_realloc - https://learn.microsoft.com/en-us/cpp/c-runtime-library/reference/aligned-realloc
        roc_realloc: fn (*RocRealloc, *CallEnv) void,
        /// Called when the Roc program has called `dbg` on something.
        roc_dbg: fn (*RocDbg, *CallEnv) void,
        /// Called when the Roc program has run an `expect` which failed.
        roc_expect_failed: fn (*RocExpectFailed, *CallEnv) void,
        /// Called when the Roc program crashes, e.g. due to integer overflow.
        /// This function must not return, because the Roc program assumes it will
        /// not continue to be executed after this function is called.
        roc_crashed: fn (*RocCrashed, *CallEnv) void,
    };
}

/// When RocOps.roc_alloc gets called, it will be passed one of these.
/// That function should write the address to the allocated memory into `ret`.
/// If it cannot provide a non-null pointer (e.g. due to OOM), it
/// must not return, and must instead do something along the lines
/// of roc_crashed.
pub const RocAlloc = struct {
    alignment: usize,
    length: usize,
    ret: *anyopaque,
};

/// When RocOps.roc_dealloc gets called, it will be passed one of these.
/// (The length of the allocation cannot be provided, because it is
/// not always known at runtime due to the way seamless slices work.)
pub const RocDealloc = struct {
    alignment: usize,
    ptr: *anyopaque,
};

/// When RocOps.roc_realloc gets called, it will be passed one of these.
/// That function should write the allocated memory into `ret`.
/// If it cannot provide a non-null pointer (e.g. due to OOM), it
/// must not return, and must instead do something along the lines
/// of roc_crashed.
pub const RocRealloc = struct {
    alignment: usize,
    new_length: usize,
    ret: *anyopaque,
};

/// The UTF-8 string message the host receives when a Roc program crashes
/// (e.g. due to integer overflow).
///
/// The pointer to the UTF-8 bytes is guaranteed to be non-null,
/// but it is *not* guaranteed to be null-terminated.
pub const RocCrashed = struct {
    utf8_bytes: *u8,
    len: usize,
};

/// The information the host receives when a Roc program runs a `dbg`.
///
/// The pointer to the UTF-8 bytes is guaranteed to be non-null,
/// but it is *not* guaranteed to be null-terminated.
pub const RocDbg = struct {
    // TODO make this be structured instead of just a string, so that
    // the host can format things more nicely (e.g. syntax highlighting).
    utf8_bytes: *u8,
    len: usize,
};

/// The information the host receives when a Roc program runs an inline `expect`
/// which fails.
///
/// The pointer to the UTF-8 bytes is guaranteed to be non-null,
/// but it is *not* guaranteed to be null-terminated.
pub const RocExpectFailed = struct {
    // TODO make this be structured instead of just a string, so that
    // the host can format things more nicely (e.g. syntax highlighting).
    utf8_bytes: *u8,
    len: usize,
};
