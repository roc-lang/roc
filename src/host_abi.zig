/// All Roc functions that are exposed to the host take 1 argument and return void.
/// The 1 argument is a pointer to one of these structs, which includes an address
/// that the Roc call will write the return value into. This design makes Roc's ABI
/// very simple; the calling convention is just "pass a single pointer".
pub const RocCall = struct {
    /// Function pointers that the Roc program uses, e.g. alloc, dealloc, etc.
    ops: *RocOps,
    /// The argument that the Roc entrypoint function will receive from the host.
    /// (If multiple arguments are needed, this should be a pointer to a struct.)
    arg: *anyopaque,
    /// What the Roc entrypoint function will return to the host.
    /// (The caller should have allocated enough space for the Roc function to write
    /// the entire return value into this.)
    ret: *anyopaque,
};

/// The operations (in the form of function pointers) that a running Roc program
/// needs the host to provide.
///
/// This is used in both calls from actual hosts as well as evaluation of constants
/// inside the Roc compiler itself.
pub const RocOps = struct {
    /// Like _aligned_malloc (size, alignment) - https://learn.microsoft.com/en-us/cpp/c-runtime-library/reference/aligned-malloc
    /// Roc will automatically call roc_crashed if this returns null.
    roc_alloc: fn (*RocAlloc) void,
    /// Like _aligned_free - https://learn.microsoft.com/en-us/cpp/c-runtime-library/reference/aligned-free
    roc_dealloc: fn (*RocDealloc) void,
    /// Like _aligned_realloc (ptr, size, alignment) - https://learn.microsoft.com/en-us/cpp/c-runtime-library/reference/aligned-realloc
    /// Roc will automatically call roc_crashed if this returns null.
    roc_realloc: fn (*RocRealloc) void,
    /// Called when the Roc program crashes, e.g. due to integer overflow.
    /// It receives a pointer to a UTF-8 string, along with its length in bytes.
    /// This function must not return, because the Roc program assumes it will
    /// not continue to be executed after this function is called.
    roc_crashed: fn (*RocCrashed) void,
    /// Called when the Roc program has called `dbg` on something.
    roc_dbg: fn (*RocDbg) void,
    /// Called when the Roc program has run an `expect` which failed.
    roc_expect_failed: fn (*RocExpectFailed) void,
};

/// When RocOps.roc_alloc gets called, it will be passed one of these.
/// That function should write the allocated memory into `ret`.
/// If it cannot proivde a non-null pointer (e.g. due to OOM), it
/// must not return, and must instead do something along the lines
/// of roc_crashed.
pub const RocAlloc = struct {
    alignment: usize,
    length: usize,
    ret: *anyopaque,
    ops: *RocOps,
};

/// When RocOps.roc_dealloc gets called, it will be passed one of these.
/// (The length of the allocation cannot be provided, because it is
/// not always known at runtime due to the way seamless slices work.)
pub const RocDealloc = struct {
    alignment: usize,
    ptr: *anyopaque,
    ops: *RocOps,
};

/// When RocOps.roc_realloc gets called, it will be passed one of these.
/// That function should write the allocated memory into `ret`.
/// If it cannot proivde a non-null pointer (e.g. due to OOM), it
/// must not return, and must instead do something along the lines
/// of roc_crashed.
pub const RocRealloc = struct {
    alignment: usize,
    new_length: usize,
    ret: *anyopaque,
    ops: *RocOps,
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
