const std = @import("std");

/// Thread-local buffer for constructing file paths.
/// This is a scratch buffer that can be used by any module that needs to
/// build up a path without allocating memory on the heap.
///
/// IMPORTANT: This buffer is not thread-safe. Each thread gets its own buffer,
/// but within a thread, only one function can safely use this buffer at a time.
/// If you need to use multiple paths simultaneously, make a local copy or allocate
/// memory as needed.
pub threadlocal var scratch_path: [std.fs.max_path_bytes:0]u8 = undefined;
