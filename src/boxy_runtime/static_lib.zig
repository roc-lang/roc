//! Root of the native Boxy runtime object linked into `--specialize=no`
//! builds: it exports the `roc_desc_*` entry points, which reach the host
//! through the runtime symbols the host defines.

const std = @import("std");
const shim_io = @import("shim_io");
const builtins = @import("builtins");
const boxy_runtime = @import("boxy_runtime");

pub const panic = std.debug.no_panic;

pub const std_options_elf_debug_info_search_paths = shim_io.elfDebugInfoSearchPaths;
pub const std_options_debug_io = shim_io.io();
pub const std_options_debug_threaded_io = null;
pub const std_options = shim_io.std_options_static_archive;

/// The host that links this object defines the runtime symbols.
pub const roc_host_role: builtins.host_abi.HostRole = .platform;

comptime {
    const exports = boxy_runtime.exports;
    for (@typeInfo(exports).@"struct".decl_names) |name| {
        if (name.len > "roc_desc_".len and std.mem.eql(u8, name[0.."roc_desc_".len], "roc_desc_")) {
            @export(&@field(exports, name), .{ .name = name });
        }
    }
}
