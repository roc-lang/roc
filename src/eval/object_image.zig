//! Loads an object the LLVM backend compiled for in-process execution and
//! binds it to this compiler's own host: the runtime symbols of
//! `builtins.in_process_host`, the boxy runtime's native functions, the
//! compiler-rt helpers native codegen emits, and the C memory routines this
//! binary already carries. After loading, the object's calls into the
//! compiler are ordinary direct calls (or stubs where the address space puts
//! the compiler out of reach), exactly as a platform host's are after a link.

const std = @import("std");
const builtins = @import("builtins");
const backend = @import("backend");
const relocatable_loader = @import("vendor_relocatable_loader");
const boxy_abi = @import("boxy_abi.zig");

const Allocator = std.mem.Allocator;
const BoxyBuiltinFn = backend.LirCodeGenMod.BoxyBuiltinFn;

pub const Image = relocatable_loader.Image;
pub const LoadError = relocatable_loader.LoadError;

/// Load `object_bytes` against this compiler's host.
pub fn load(gpa: Allocator, object_bytes: []const u8) LoadError!Image {
    return Image.load(gpa, object_bytes, .{ .resolve = resolveHostSymbol });
}

fn resolveHostSymbol(_: ?*anyopaque, name: []const u8) ?usize {
    if (builtins.in_process_host.Symbol.fromName(name)) |symbol| return symbol.address();
    if (BoxyBuiltinFn.fromSymbolName(name)) |boxy_fn| return boxy_abi.nativeFnTable()[@intFromEnum(boxy_fn)];
    return builtins.native_runtime_libcalls.resolve(name);
}

test "the host resolver covers the runtime, boxy, and libcall symbols" {
    try std.testing.expect(resolveHostSymbol(null, "roc_alloc") != null);
    try std.testing.expect(resolveHostSymbol(null, "roc_expect_err_region") != null);
    try std.testing.expect(resolveHostSymbol(null, BoxyBuiltinFn.call_erased.symbolName()) != null);
    try std.testing.expect(resolveHostSymbol(null, "__divti3") != null);
    try std.testing.expect(resolveHostSymbol(null, "memcpy") != null);
    try std.testing.expect(resolveHostSymbol(null, "roc_boxy_init_embedded") == null);
}

test {
    std.testing.refAllDecls(relocatable_loader);
}
