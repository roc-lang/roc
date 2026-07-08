//! Root for the standalone boxy runtime object linked into `roc build --opt=dev`
//! executables.
//!
//! It exports the `roc_boxy_*` C-ABI wrappers over the process-global boxy
//! runtime (the same wrappers the machine-code shim resolves in-process) plus a
//! `roc_boxy_init_embedded` entry that installs that runtime from a boxy sidecar
//! embedded in the linked program. The dev backend emits a call to
//! `roc_boxy_init_embedded` at the top of each exported entrypoint, so the
//! runtime is ready before any Roc procedure runs. Host operations reach the
//! program through linker-resolved symbols (the extern symbol ABI), matching the
//! app and builtins objects this runtime links beside.

const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");
const eval = @import("eval");
const lir = @import("lir");
const shim_io = @import("shim_io");

const boxy_abi = eval.boxy_abi;
const RocOps = builtins.host_abi.RocOps;
const BoxySidecar = lir.LirImage.BoxySidecar;

/// Host operations resolve through linker-provided symbols.
pub const roc_host_call_mode: builtins.host_abi.HostCallMode = .extern_symbols;

pub const panic = std.debug.no_panic;
pub const std_options_elf_debug_info_search_paths = shim_io.elfDebugInfoSearchPaths;
/// Minimal debug output override; avoids pulling in the full threaded IO vtable.
pub const std_options_debug_io = shim_io.io();
/// Disables threaded debug IO to prevent the threaded vtable from being linked into user programs.
pub const std_options_debug_threaded_io = null;

/// Disables stack-trace capture; see `shim_io.std_options_no_stack_tracing`.
pub const std_options = shim_io.std_options_no_stack_tracing;

/// The self-contained boxy sidecar buffer emitted by the object compiler into
/// the linked program. `roc_boxy_sidecar_blob` is the table byte buffer,
/// `roc_boxy_sidecar_desc` its offset metadata, and `roc_boxy_sidecar_blob_len`
/// the buffer length. A zero length marks a program with no boxy tables.
extern var roc_boxy_sidecar_blob: u8;
extern const roc_boxy_sidecar_blob_len: u64;
extern const roc_boxy_sidecar_desc: BoxySidecar;

/// Backing `RocOps` for the global runtime. Under the extern symbol ABI its
/// methods dispatch to linker-resolved host symbols and never read this value,
/// so the runtime only needs a stable address to store. It backs the runtime's
/// refcounted application allocations (`allocRocDataWithRc`), which the host's
/// allocation tracker balances against the reference-counting drops the program
/// emits.
var startup_ops: RocOps = undefined;

/// A bump allocator for the runtime's own bookkeeping — the descriptor tables,
/// the decoded sidecar view, and its arenas. This memory lives for the whole
/// process and is reclaimed by the OS at exit, so it never frees. Keeping it off
/// the host allocation symbols (`roc_alloc` and friends) means the host's
/// Roc-allocation tracker sees only the program's reference-counted values, so
/// runtime infrastructure is not mistaken for an application memory leak. The
/// region is reserved with a raw `mmap` syscall (demand-paged, so the large
/// reservation costs no physical memory until touched) rather than
/// `std.heap.page_allocator`, whose posix `mmap`/`munmap` pull `__errno_location`
/// — a symbol absent from this freestanding standalone object.
const RuntimeArena = struct {
    base: [*]u8 = undefined,
    cap: usize = 0,
    used: usize = 0,
    ready: bool = false,

    fn ensure(self: *RuntimeArena) bool {
        if (self.ready) return true;
        const linux = std.os.linux;
        const cap: usize = 1 << 30;
        const raw = linux.mmap(
            null,
            cap,
            .{ .READ = true, .WRITE = true },
            .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
            -1,
            0,
        );
        if (linux.errno(raw) != .SUCCESS) return false;
        self.base = @ptrFromInt(raw);
        self.cap = cap;
        self.used = 0;
        self.ready = true;
        return true;
    }

    fn alloc(ctx: *anyopaque, len: usize, alignment: std.mem.Alignment, ret_addr: usize) ?[*]u8 {
        _ = ret_addr;
        const self: *RuntimeArena = @ptrCast(@alignCast(ctx));
        if (!self.ensure()) return null;
        const start = std.mem.alignForward(usize, self.used, alignment.toByteUnits());
        const end = start + len;
        if (end > self.cap) return null;
        self.used = end;
        return self.base + start;
    }

    fn resize(_: *anyopaque, memory: []u8, _: std.mem.Alignment, new_len: usize, _: usize) bool {
        return new_len <= memory.len;
    }

    fn remap(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize, _: usize) ?[*]u8 {
        return null;
    }

    fn free(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize) void {}

    const vtable = std.mem.Allocator.VTable{
        .alloc = alloc,
        .resize = resize,
        .remap = remap,
        .free = free,
    };

    fn allocator(self: *RuntimeArena) std.mem.Allocator {
        return .{ .ptr = self, .vtable = &vtable };
    }
};

var runtime_arena: RuntimeArena = .{};
const host_allocator = runtime_arena.allocator();

/// Install the process-global boxy runtime from the embedded sidecar. The
/// entrypoint wrappers call this before invoking any Roc procedure; the first
/// call installs the runtime and later calls return early. The decoded sidecar
/// view lives for the process, so it is intentionally leaked.
export fn roc_boxy_init_embedded() callconv(.c) void {
    const blob_len: usize = @intCast(roc_boxy_sidecar_blob_len);
    if (blob_len == 0) return;

    const gpa = host_allocator;
    const blob_ptr: [*]align(1) u8 = @ptrCast(&roc_boxy_sidecar_blob);

    const view = gpa.create(BoxySidecar.View) catch return;
    view.* = roc_boxy_sidecar_desc.view(
        blob_ptr,
        blob_len,
        base.target.TargetUsize.native,
        gpa,
    ) catch {
        gpa.destroy(view);
        return;
    };

    boxy_abi.initGlobalFromSidecarView(gpa, view, &startup_ops) catch |err| switch (err) {
        error.AlreadyInitialized, error.OutOfMemory => gpa.destroy(view),
    };
}

// Force-export the boxy C-ABI wrappers so the object compiler's relocations
// resolve against this runtime at the final link. Each name matches a `pub fn`
// in `src/eval/boxy_abi.zig` and the `BoxyBuiltinFn` symbol names the dev
// backend emits.
comptime {
    const names = [_][:0]const u8{
        "roc_boxy_static_desc",
        "roc_boxy_static_dict",
        "roc_boxy_nested_desc",
        "roc_boxy_inspect",
        "roc_boxy_box",
        "roc_boxy_unbox",
        "roc_boxy_tag",
        "roc_boxy_tag_payload",
        "roc_boxy_eq",
        "roc_boxy_drop",
        "roc_boxy_tag_match",
        "roc_boxy_desc_copy",
        "roc_boxy_dynamic_num_literal",
        "roc_boxy_dynamic_num_literal_ref",
        "roc_boxy_dynamic_frac_literal_ref",
        "roc_boxy_call_dict",
        "roc_boxy_materialize_call_result",
        "roc_boxy_register_proc",
        "roc_boxy_register_erased_proc",
        "roc_boxy_call_erased",
    };
    for (names) |name| {
        @export(&@field(boxy_abi, name), .{ .name = name });
    }
}
