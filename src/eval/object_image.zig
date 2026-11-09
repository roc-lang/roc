//! Loads an object the LLVM backend compiled for in-process execution and
//! binds it to this compiler's own host: the runtime symbols of
//! `builtins.in_process_host`, the
//! compiler-rt helpers native codegen emits, and the C memory and math
//! routines this binary already carries. After loading, the object's calls into the
//! compiler are ordinary direct calls (or stubs where the address space puts
//! the compiler out of reach), exactly as a platform host's are after a link.

const std = @import("std");
const builtins = @import("builtins");
const boxy_runtime = @import("boxy_runtime");
const relocatable_loader = @import("vendor_relocatable_loader");

const Allocator = std.mem.Allocator;

pub const Image = relocatable_loader.Image;
pub const LoadError = relocatable_loader.LoadError;

/// Load `object_bytes` against this compiler's host.
pub fn load(gpa: Allocator, object_bytes: []const u8) LoadError!Image {
    return Image.load(gpa, object_bytes, .{ .resolve = resolveHostSymbol });
}

fn resolveHostSymbol(_: ?*anyopaque, name: []const u8) ?usize {
    if (builtins.in_process_host.Symbol.fromName(name)) |symbol| return symbol.address();
    if (boxyRuntimeSymbol(name)) |address| return address;
    return builtins.native_runtime_libcalls.resolve(name);
}

/// The Boxy runtime entry point named `name` (`roc_desc_*`), which a
/// `.boxy` program's code calls.
fn boxyRuntimeSymbol(name: []const u8) ?usize {
    const prefix = "roc_desc_";
    if (!std.mem.startsWith(u8, name, prefix)) return null;
    const exports = boxy_runtime.exports;
    inline for (@typeInfo(exports).@"struct".decl_names) |decl| {
        if (comptime std.mem.startsWith(u8, decl, prefix)) {
            if (std.mem.eql(u8, name, decl)) return @intFromPtr(&@field(exports, decl));
        }
    }
    return null;
}

test "the host resolver covers the runtime and libcall symbols" {
    try std.testing.expect(resolveHostSymbol(null, "roc_alloc") != null);
    try std.testing.expect(resolveHostSymbol(null, "roc_expect_err_region") != null);
    try std.testing.expect(resolveHostSymbol(null, "__divti3") != null);
    try std.testing.expect(resolveHostSymbol(null, "memcpy") != null);
    try std.testing.expect(resolveHostSymbol(null, "fmod") != null);
    try std.testing.expect(resolveHostSymbol(null, "fmodf") != null);
    try std.testing.expect(resolveHostSymbol(null, "roc_desc_copy") != null);
}

test {
    std.testing.refAllDecls(relocatable_loader);
}
