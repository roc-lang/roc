//! `memcpy` for an MSVC-ABI image without vcruntime.
//!
//! Like vcruntime's, each of these routines is its own archive member with a
//! strong definition: it takes precedence over compiler-rt's weak one, a link
//! that needs nothing else from this archive's compiler-rt member never loads
//! it, and an object that defines one of them itself does not collide with
//! the rest. A host that brings its own compiler-rt helpers, such as Rust's
//! `compiler_builtins`, would otherwise load both helper sets, whose weak
//! definitions `lld-link` rejects as duplicates.

export fn memcpy(noalias dest: ?[*]u8, noalias src: ?[*]const u8, len: usize) callconv(.c) ?[*]u8 {
    @disableIntrinsics();
    for (0..len) |i| dest.?[i] = src.?[i];
    return dest;
}
