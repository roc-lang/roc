//! Statement-LIR to LLVM code generator.
//!
//! This backend intentionally uses a small internal ABI:
//!
//!     void roc_proc_N(*RocOps, ret_ptr, args_ptr)
//!
//! Direct Roc calls pack argument bytes into `args_ptr` using Roc's canonical
//! alignment order, and callees write results into caller-owned storage. This
//! avoids target-specific aggregate return and argument rules while still
//! letting LLVM optimize ordinary local stack traffic.

const std = @import("std");

const Base = @import("base");
const builtin = @import("builtin");
const build_options = @import("build_options");
const SourceLoc = lir.SourceLoc;
const LowLevelBuiltins = Base.LowLevelBuiltins;
const numeric_conversion = Base.numeric_conversion;
const builtins = @import("builtins");
const shim_symbols = builtins.shim_symbols;
const BoxyBuiltinFn = @import("backend").LirCodeGenMod.BoxyBuiltinFn;
const layout = @import("layout");
const lir = @import("lir");
const GuardedList = lir.LirStore.GuardedList;
const CheckedArithmetic = lir.CheckedArithmetic;
const roc_target = @import("roc_target");

const CoreCtx = @import("ctx").CoreCtx;
const LlvmBuilder = @import("vendor_llvm_ir").Builder;

const Allocator = std.mem.Allocator;
const CFStmtId = lir.CFStmtId;
const LocalId = lir.LocalId;
const LocalSpan = lir.LocalSpan;
const LirProcSpec = lir.LirProcSpec;
const LirProcSpecId = lir.LirProcSpecId;
const RcAtomicity = lir.LIR.RcAtomicity;
const StrLiteral = lir.LIR.StrLiteral;

const BuiltinSymbol = builtins.builtin_registry.BuiltinFn;

const SimdLowLevel = enum(u16) {
    simd_splat = @intFromEnum(lir.LowLevel.simd_splat),
    simd_get_lane_unchecked = @intFromEnum(lir.LowLevel.simd_get_lane_unchecked),
    simd_with_lane_unchecked = @intFromEnum(lir.LowLevel.simd_with_lane_unchecked),
    simd_to_u128_bits = @intFromEnum(lir.LowLevel.simd_to_u128_bits),
    simd_from_u128_bits = @intFromEnum(lir.LowLevel.simd_from_u128_bits),
    simd_add_wrap = @intFromEnum(lir.LowLevel.simd_add_wrap),
    simd_sub_wrap = @intFromEnum(lir.LowLevel.simd_sub_wrap),
    simd_add_sat = @intFromEnum(lir.LowLevel.simd_add_sat),
    simd_sub_sat = @intFromEnum(lir.LowLevel.simd_sub_sat),
    simd_neg_wrap = @intFromEnum(lir.LowLevel.simd_neg_wrap),
    simd_abs_wrap = @intFromEnum(lir.LowLevel.simd_abs_wrap),
    simd_min = @intFromEnum(lir.LowLevel.simd_min),
    simd_max = @intFromEnum(lir.LowLevel.simd_max),
    simd_abs_diff = @intFromEnum(lir.LowLevel.simd_abs_diff),
    simd_avg_rounded = @intFromEnum(lir.LowLevel.simd_avg_rounded),
    simd_mul_wrap = @intFromEnum(lir.LowLevel.simd_mul_wrap),
    simd_mul_high = @intFromEnum(lir.LowLevel.simd_mul_high),
    simd_mul_q15_sat = @intFromEnum(lir.LowLevel.simd_mul_q15_sat),
    simd_mul_wide_lo = @intFromEnum(lir.LowLevel.simd_mul_wide_lo),
    simd_mul_wide_hi = @intFromEnum(lir.LowLevel.simd_mul_wide_hi),
    simd_dot_pairs = @intFromEnum(lir.LowLevel.simd_dot_pairs),
    simd_dot_pairs_sat = @intFromEnum(lir.LowLevel.simd_dot_pairs_sat),
    simd_sad = @intFromEnum(lir.LowLevel.simd_sad),
    simd_and = @intFromEnum(lir.LowLevel.simd_and),
    simd_or = @intFromEnum(lir.LowLevel.simd_or),
    simd_xor = @intFromEnum(lir.LowLevel.simd_xor),
    simd_not = @intFromEnum(lir.LowLevel.simd_not),
    simd_bit_select = @intFromEnum(lir.LowLevel.simd_bit_select),
    simd_eq_lanes = @intFromEnum(lir.LowLevel.simd_eq_lanes),
    simd_gt_lanes = @intFromEnum(lir.LowLevel.simd_gt_lanes),
    simd_gte_lanes = @intFromEnum(lir.LowLevel.simd_gte_lanes),
    simd_bitmask = @intFromEnum(lir.LowLevel.simd_bitmask),
    simd_shl_wrap = @intFromEnum(lir.LowLevel.simd_shl_wrap),
    simd_shr_wrap = @intFromEnum(lir.LowLevel.simd_shr_wrap),
    simd_shr_zf_wrap = @intFromEnum(lir.LowLevel.simd_shr_zf_wrap),
    simd_shr_rounded = @intFromEnum(lir.LowLevel.simd_shr_rounded),
    simd_interleave_lo = @intFromEnum(lir.LowLevel.simd_interleave_lo),
    simd_interleave_hi = @intFromEnum(lir.LowLevel.simd_interleave_hi),
    simd_even_lanes = @intFromEnum(lir.LowLevel.simd_even_lanes),
    simd_odd_lanes = @intFromEnum(lir.LowLevel.simd_odd_lanes),
    simd_reverse_lanes = @intFromEnum(lir.LowLevel.simd_reverse_lanes),
    simd_table_lookup = @intFromEnum(lir.LowLevel.simd_table_lookup),
    simd_concat_shift_bytes = @intFromEnum(lir.LowLevel.simd_concat_shift_bytes),
    simd_widen_lo = @intFromEnum(lir.LowLevel.simd_widen_lo),
    simd_widen_hi = @intFromEnum(lir.LowLevel.simd_widen_hi),
    simd_pairwise_add_widen = @intFromEnum(lir.LowLevel.simd_pairwise_add_widen),
    simd_narrow_wrap = @intFromEnum(lir.LowLevel.simd_narrow_wrap),
    simd_narrow_sat = @intFromEnum(lir.LowLevel.simd_narrow_sat),
    simd_sum_lanes = @intFromEnum(lir.LowLevel.simd_sum_lanes),
    simd_sum_lanes_wrap = @intFromEnum(lir.LowLevel.simd_sum_lanes_wrap),
    simd_clmul_lo = @intFromEnum(lir.LowLevel.simd_clmul_lo),
    simd_clmul_hi = @intFromEnum(lir.LowLevel.simd_clmul_hi),
};

/// Linker name of a registered builtin; the registry is the only place
/// builtin symbol names are spelled.
fn builtinSymbol(comptime f: BuiltinSymbol) [:0]const u8 {
    return f.symbolName();
}

/// Bridges the LIR refcount atomicity to the shared table's atomicity so that
/// refcount-helper symbols are selected through `LowLevelBuiltins.rcHelper`.
fn tableAtomicity(atomicity: RcAtomicity) LowLevelBuiltins.RcAtomicity {
    return switch (atomicity) {
        .atomic => .atomic,
        .single_thread => .single_thread,
    };
}

fn getLlvmTriple(target: std.Target) []const u8 {
    const arch = target.cpu.arch;
    if (arch == .x86_64) {
        return switch (target.os.tag) {
            .windows => if (target.abi == .msvc) "x86_64-pc-windows-msvc" else "x86_64-w64-windows-gnu",
            .macos => "x86_64-apple-macosx" ++ roc_target.macos_deployment.llvm_version,
            .linux => if (target.abi == .musl)
                "x86_64-unknown-linux-musl"
            else if (target.abi == .android)
                "x86_64-unknown-linux-android"
            else
                "x86_64-unknown-linux-gnu",
            .freebsd => "x86_64-unknown-freebsd",
            .openbsd => "x86_64-unknown-openbsd",
            .netbsd => "x86_64-unknown-netbsd",
            .freestanding => "x86_64-unknown-unknown",
            .other, .contiki, .fuchsia, .hermit, .managarm, .haiku, .hurd, .illumos, .plan9, .rtems, .serenity, .dragonfly, .driverkit, .ios, .maccatalyst, .tvos, .visionos, .watchos, .uefi, .@"3ds", .ps3, .ps4, .ps5, .psp, .vita, .emscripten, .wasi, .amdhsa, .amdpal, .cuda, .mesa3d, .nvcl, .opencl, .opengl, .vulkan => "x86_64-unknown-unknown",
        };
    }
    if (arch == .aarch64) {
        return switch (target.os.tag) {
            .windows => if (target.abi == .msvc) "aarch64-pc-windows-msvc" else "aarch64-w64-windows-gnu",
            .macos => "aarch64-apple-macosx" ++ roc_target.macos_deployment.llvm_version,
            .ios => "aarch64-apple-ios",
            .linux => if (target.abi == .musl)
                "aarch64-unknown-linux-musl"
            else if (target.abi == .android)
                "aarch64-unknown-linux-android"
            else
                "aarch64-unknown-linux-gnu",
            .freebsd => "aarch64-unknown-freebsd",
            .openbsd => "aarch64-unknown-openbsd",
            .netbsd => "aarch64-unknown-netbsd",
            .freestanding => "aarch64-unknown-unknown",
            .other, .contiki, .fuchsia, .hermit, .managarm, .haiku, .hurd, .illumos, .plan9, .rtems, .serenity, .dragonfly, .driverkit, .maccatalyst, .tvos, .visionos, .watchos, .uefi, .@"3ds", .ps3, .ps4, .ps5, .psp, .vita, .emscripten, .wasi, .amdhsa, .amdpal, .cuda, .mesa3d, .nvcl, .opencl, .opengl, .vulkan => "aarch64-unknown-unknown",
        };
    }
    if (arch == .x86) {
        return switch (target.os.tag) {
            .windows => if (target.abi == .msvc) "i686-pc-windows-msvc" else "i686-w64-windows-gnu",
            .linux => if (target.abi == .musl)
                "i686-unknown-linux-musl"
            else if (target.abi == .android)
                "i686-unknown-linux-android"
            else
                "i686-unknown-linux-gnu",
            .freestanding => "i686-unknown-unknown",
            .other, .contiki, .fuchsia, .hermit, .managarm, .haiku, .hurd, .illumos, .plan9, .rtems, .serenity, .dragonfly, .freebsd, .netbsd, .openbsd, .driverkit, .ios, .maccatalyst, .macos, .tvos, .visionos, .watchos, .uefi, .@"3ds", .ps3, .ps4, .ps5, .psp, .vita, .emscripten, .wasi, .amdhsa, .amdpal, .cuda, .mesa3d, .nvcl, .opencl, .opengl, .vulkan => "i686-unknown-unknown",
        };
    }
    if (arch == .arm or arch == .armeb or arch == .thumb or arch == .thumbeb) {
        return switch (target.os.tag) {
            .linux => if (target.abi == .musleabihf)
                "arm-unknown-linux-musleabihf"
            else if (target.abi == .gnueabihf)
                "arm-unknown-linux-gnueabihf"
            else if (target.abi == .musleabi)
                "arm-unknown-linux-musleabi"
            else if (target.abi == .gnueabi)
                "arm-unknown-linux-gnueabi"
            else
                "arm-unknown-linux-gnueabihf",
            .freestanding => "arm-unknown-unknown",
            .other, .contiki, .fuchsia, .hermit, .managarm, .haiku, .hurd, .illumos, .plan9, .rtems, .serenity, .dragonfly, .freebsd, .netbsd, .openbsd, .driverkit, .ios, .maccatalyst, .macos, .tvos, .visionos, .watchos, .windows, .uefi, .@"3ds", .ps3, .ps4, .ps5, .psp, .vita, .emscripten, .wasi, .amdhsa, .amdpal, .cuda, .mesa3d, .nvcl, .opencl, .opengl, .vulkan => "arm-unknown-unknown",
        };
    }
    if (arch == .wasm32) {
        return switch (target.os.tag) {
            .wasi => "wasm32-wasi",
            .freestanding, .other, .contiki, .fuchsia, .hermit, .managarm, .haiku, .hurd, .illumos, .linux, .plan9, .rtems, .serenity, .dragonfly, .freebsd, .netbsd, .openbsd, .driverkit, .ios, .maccatalyst, .macos, .tvos, .visionos, .watchos, .windows, .uefi, .@"3ds", .ps3, .ps4, .ps5, .psp, .vita, .emscripten, .amdhsa, .amdpal, .cuda, .mesa3d, .nvcl, .opencl, .opengl, .vulkan => "wasm32-unknown-unknown",
        };
    }
    if (arch == .wasm64) {
        return switch (target.os.tag) {
            .wasi => "wasm64-wasi",
            .freestanding, .other, .contiki, .fuchsia, .hermit, .managarm, .haiku, .hurd, .illumos, .linux, .plan9, .rtems, .serenity, .dragonfly, .freebsd, .netbsd, .openbsd, .driverkit, .ios, .maccatalyst, .macos, .tvos, .visionos, .watchos, .windows, .uefi, .@"3ds", .ps3, .ps4, .ps5, .psp, .vita, .emscripten, .amdhsa, .amdpal, .cuda, .mesa3d, .nvcl, .opencl, .opengl, .vulkan => "wasm64-unknown-unknown",
        };
    }
    if (arch == .riscv32) return "riscv32-unknown-unknown";
    if (arch == .riscv64) return "riscv64-unknown-unknown";
    return "unknown-unknown-unknown";
}

fn getLlvmDataLayout(target: std.Target) []const u8 {
    if (target.cpu.arch == .wasm32) return "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-n32:64-S128-ni:1:10:20";
    if (target.cpu.arch == .x86_64) {
        return switch (target.os.tag) {
            .windows => "e-m:w-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128",
            .macos => "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128",
            .linux, .freebsd, .openbsd, .netbsd, .freestanding => "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128",
            .other, .contiki, .fuchsia, .hermit, .managarm, .haiku, .hurd, .illumos, .plan9, .rtems, .serenity, .dragonfly, .driverkit, .ios, .maccatalyst, .tvos, .visionos, .watchos, .uefi, .@"3ds", .ps3, .ps4, .ps5, .psp, .vita, .emscripten, .wasi, .amdhsa, .amdpal, .cuda, .mesa3d, .nvcl, .opencl, .opengl, .vulkan => unsupportedLlvmDataLayout(target),
        };
    }
    if (target.cpu.arch == .aarch64) {
        return switch (target.os.tag) {
            .windows => "e-m:w-p:64:64-i32:32-i64:64-i128:128-n32:64-S128-Fn32",
            .macos, .ios => "e-m:o-i64:64-i128:128-n32:64-S128-Fn32",
            .linux, .freebsd, .openbsd, .netbsd, .freestanding => "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32",
            .other, .contiki, .fuchsia, .hermit, .managarm, .haiku, .hurd, .illumos, .plan9, .rtems, .serenity, .dragonfly, .driverkit, .maccatalyst, .tvos, .visionos, .watchos, .uefi, .@"3ds", .ps3, .ps4, .ps5, .psp, .vita, .emscripten, .wasi, .amdhsa, .amdpal, .cuda, .mesa3d, .nvcl, .opencl, .opengl, .vulkan => unsupportedLlvmDataLayout(target),
        };
    }
    return unsupportedLlvmDataLayout(target);
}

fn unsupportedLlvmDataLayout(target: std.Target) noreturn {
    if (builtin.mode == .Debug) {
        std.debug.panic("LLVM codegen invariant violated: unsupported target for data layout: {s}-{s}", .{
            @tagName(target.cpu.arch),
            @tagName(target.os.tag),
        });
    }
    unreachable;
}

fn llvmInvariantFmt(comptime fmt: []const u8, args: anytype) noreturn {
    std.debug.panic("LLVM codegen invariant violated: " ++ fmt, args);
}

/// Lowers statement-only LIR procedures to LLVM bitcode.
pub const MonoLlvmCodeGen = struct {
    pub const EntrypointAbi = enum {
        test_runner,
        plugin,
    };

    pub const Entrypoint = struct {
        symbol_name: []const u8,
        proc: LirProcSpecId,
        arg_layouts: []const layout.Idx,
        ret_layout: layout.Idx,
        abi: EntrypointAbi = .test_runner,
    };

    allocator: Allocator,
    target: std.Target,
    triple: []const u8,
    data_layout: []const u8,
    builtin_symbol_mode: BuiltinSymbolMode = .bitcode,
    proc_symbol_mode: ProcSymbolMode = .local_index,
    /// How generated code reaches the host: through the RocOps vtable (the
    /// eval JIT, which passes a live RocOps at runtime) or through
    /// linker-resolved extern symbols (all linked output).
    host_call_mode: builtins.host_abi.HostCallMode = .vtable,
    store: *const lir.LirStore,
    erased_arg_desc_offsets: []const lir.LIR.ErasedArgDescOffset,
    erased_arg_desc_params: []const lir.LIR.ErasedArgDescParam,

    /// Layout store for resolving composite type layouts (records, tuples).
    /// Set by the evaluator before calling generateCode.
    layout_store: ?*const layout.Store = null,
    /// Optional compiler-plugin stamp bytes exported by generated shared libs.
    plugin_stamp_bytes: ?[]const u8 = null,
    plugin_stamp_alignment: u32 = 1,

    builder: ?*LlvmBuilder = null,
    wip: ?*LlvmBuilder.WipFunction = null,
    roc_ops_arg: ?LlvmBuilder.Value = null,
    test_context_arg: ?LlvmBuilder.Value = null,
    ret_ptr_arg: ?LlvmBuilder.Value = null,
    args_ptr_arg: ?LlvmBuilder.Value = null,
    capture_ptr_arg: ?LlvmBuilder.Value = null,
    reuse_ptr_arg: ?LlvmBuilder.Value = null,
    ret_desc_ptr_arg: ?LlvmBuilder.Value = null,
    current_runtime_ret_desc: ?LocalId = null,
    current_ret_layout: layout.Idx = .zst,
    /// Source statement whose machine instructions are currently being emitted.
    /// Default-platform crash lowering consumes its explicit inline-scope chain.
    current_source_stmt: ?CFStmtId = null,

    proc_registry: std.AutoHashMap(u32, LlvmBuilder.Function.Index),
    builtin_functions: std.StringHashMap(LlvmBuilder.Function.Index),
    /// Per-function scratch slot RC-helper calls copy their argument into,
    /// so the argument local's own slot address never escapes into the call.
    /// Follows `wip`: saved and restored wherever the active function swaps.
    rc_arg_scratch: ?LlvmBuilder.Value = null,
    /// Shims that give cold-path builtin calls the preserve_most convention,
    /// keyed by builtin symbol name. See `callBuiltin`.
    cold_shims: std.StringHashMap(ColdShim),
    /// How many enclosing cold switch arms the statement being emitted sits
    /// under. Non-zero routes builtin calls through preserve_most shims.
    cold_depth: u32 = 0,
    static_bytes: std.StringHashMap(LlvmBuilder.Value),
    static_refcounted_backings: std.AutoHashMap(u32, LlvmBuilder.Value),
    static_data_globals: std.AutoHashMap(u32, LlvmBuilder.Value),
    runtime_error_func: ?LlvmBuilder.Function.Index = null,
    rc_helpers: std.AutoHashMap(u64, RcHelperEntry),
    /// Atomic helpers required by relocations in the separately emitted
    /// readonly static-data object.
    static_data_rc_helpers: []const layout.RcHelperKey = &.{},
    /// Procedures a static-data object names by symbol. Only these need
    /// external linkage under the symbol ABI; the rest stay internal so the
    /// LLVM pipeline can see every call site and specialize across them.
    static_data_procs: []const LirProcSpecId = &.{},
    boxy_capture_drop_helpers: std.AutoHashMap(u64, BoxyCaptureDropHelper),
    boxy_dict_thunks: std.AutoHashMap(u32, LlvmBuilder.Function.Index),
    join_points: std.AutoHashMap(u32, JoinInfo),
    compiled_joins: std.AutoHashMap(u32, void),
    stmt_incoming_counts: std.AutoHashMap(u32, u32),
    stmt_entry_blocks: std.AutoHashMap(u32, LlvmBuilder.Function.Block.Index),
    loop_continue_blocks: std.ArrayList(LlvmBuilder.Function.Block.Index),
    loop_break_blocks: std.ArrayList(LlvmBuilder.Function.Block.Index),
    local_slots: []LocalSlot = &.{},
    deferred_str_captures: []?DeferredStrCapture = &.{},
    /// Number of non-null entries in `deferred_str_captures`. Deferred
    /// captures are rare, and several hooks run once per local write or once
    /// per join; the count lets those hooks skip their whole-table scans in
    /// the overwhelmingly common all-empty state, which otherwise turn
    /// quadratic in a proc's local count.
    deferred_str_capture_count: usize = 0,
    /// Indices of `deferred_str_captures` slots that may be non-null. The
    /// slot array spans every LIR local, so the clear-all and scan-all
    /// operations that run per jump and per call must touch only the handful
    /// of live captures rather than the whole program's local space. Entries
    /// go stale when a single capture is cleared; readers skip null slots.
    deferred_str_capture_actives: std.ArrayList(u32) = .empty,
    string_counter: u32 = 0,
    /// When true the module is built with DWARF debug info: a compile unit,
    /// one subprogram per proc, and per-statement line locations from the
    /// LIR store's source-location tables.
    emit_debug_info: bool = false,
    /// Emit local variable declarations for source-level debugger inspection.
    emit_local_debug_info: bool = false,
    /// Build-only default-platform Linux executables link a small runtime
    /// object that owns process startup diagnostics and signal handling.
    enable_default_platform_runtime: bool = false,
    /// Synthetic default-platform apps lower the default echo host call to
    /// direct platform writes instead of calling an external host function.
    enable_default_platform_hosted_calls: bool = false,
    /// Synthetic default-platform apps preserve source proc names and local
    /// debug locations for crash and stack-overflow diagnostics.
    enable_default_platform_diagnostics: bool = false,
    /// DW_AT_producer for the compile unit. Carries the compiler version so
    /// debugger formatters can detect when a binary was built by a different
    /// roc than the formatter was written for.
    debug_producer: []const u8 = "roc",
    debug_compile_unit: LlvmBuilder.Metadata.Optional = .none,
    debug_enums_fwd_ref: LlvmBuilder.Metadata.Optional = .none,
    debug_globals_fwd_ref: LlvmBuilder.Metadata.Optional = .none,
    current_subprogram: LlvmBuilder.Metadata.Optional = .none,
    current_debug_file: u32 = SourceLoc.no_file,
    /// Virtual source procedures and call-site chains for LIR inline scopes.
    /// These are cleared at each physical procedure because the outermost
    /// call-site scope is that procedure's concrete `DISubprogram`.
    debug_inline_subprograms: std.AutoHashMap(u32, LlvmBuilder.Metadata),
    debug_inline_callsites: std.AutoHashMap(u32, LlvmBuilder.Metadata),
    /// Debug type metadata per layout index, memoized per module build.
    debug_types: std.AutoHashMap(u32, LlvmBuilder.Metadata),
    expect_err_region_global: ?LlvmBuilder.Value = null,
    /// Evaluator entrypoints install the explicit in-process Boxy function
    /// table here before calling any generated procedure.
    boxy_fn_table_global: ?LlvmBuilder.Value = null,
    /// Set as soon as this module lowers a Boxy operation. Linked entrypoints
    /// use it to initialize the sidecar runtime and register dispatch thunks.
    boxy_runtime_used: bool = false,

    /// Errors reported while building LLVM IR.
    pub const Error = error{
        OutOfMemory,
        CompilationFailed,
        UnsupportedLowLevel,
    };

    /// Owned serialized LLVM bitcode produced by this backend.
    pub const GenerateResult = struct {
        bitcode: []const u32,
        allocator: Allocator,

        /// Releases the serialized bitcode buffer.
        pub fn deinit(self: *GenerateResult) void {
            self.allocator.free(self.bitcode);
        }
    };

    const BuiltinSymbolMode = enum {
        bitcode,
        native_object,
    };

    const ProcSymbolMode = enum {
        local_index,
        lir_symbol,
    };

    const RocOpsCallback = enum {
        dbg,
        expect_failed,
        crashed,
    };

    const LocalSlot = struct {
        ptr: LlvmBuilder.Value,
        layout_idx: layout.Idx,
        size: u32,
        alignment: LlvmBuilder.Alignment,
        allocated: bool,
    };

    const BoxyListElementDesc = struct {
        elem_layout: layout.Idx,
        desc: lir.LIR.BoxyDescRef,
    };

    const JoinInfo = struct {
        block: LlvmBuilder.Function.Block.Index,
        params: LocalSpan,
        body: CFStmtId,
    };

    const StrFromUtf8LayoutInfo = struct {
        ok_tag: u16,
        err_tag: u16,
        outer_disc_offset: u32,
        outer_disc_size: u32,
        err_index_offset: u32,
        err_problem_offset: u32,
        inner_disc_offset: u32,
        inner_disc_size: u32,
        inner_bad_utf8_tag: u32,
    };

    const StrSplitFirstLayoutInfo = struct {
        after_offset: u32,
        before_offset: u32,
        found_offset: u32,
    };

    const StrSplitLastLayoutInfo = struct {
        after_offset: u32,
        before_offset: u32,
        found_offset: u32,
    };

    const StrDropPrefixCaselessAsciiLayoutInfo = struct {
        after_offset: u32,
        found_offset: u32,
    };

    const RcHelperEntry = struct {
        key: layout.RcHelperKey,
        atomicity: RcAtomicity,
        function: LlvmBuilder.Function.Index,
        compiled: bool = false,
    };

    const BoxyCaptureDropHelper = struct {
        capture_layout: layout.Idx,
        desc_field_offset: u32,
        function: LlvmBuilder.Function.Index,
        compiled: bool = false,
    };

    const ArgOrder = struct {
        index: usize,
        alignment: u32,
        size: u32,
    };

    const ResolvedBase = struct {
        ptr: LlvmBuilder.Value,
        layout_idx: layout.Idx,
    };

    /// Initializes the backend for the host target.
    pub fn init(
        allocator: Allocator,
        store: *const lir.LirStore,
        erased_arg_desc_offsets: []const lir.LIR.ErasedArgDescOffset,
        erased_arg_desc_params: []const lir.LIR.ErasedArgDescParam,
    ) MonoLlvmCodeGen {
        return .{
            .allocator = allocator,
            .target = builtin.target,
            .triple = getLlvmTriple(builtin.target),
            .data_layout = getLlvmDataLayout(builtin.target),
            .store = store,
            .erased_arg_desc_offsets = erased_arg_desc_offsets,
            .erased_arg_desc_params = erased_arg_desc_params,
            .proc_registry = std.AutoHashMap(u32, LlvmBuilder.Function.Index).init(allocator),
            .builtin_functions = std.StringHashMap(LlvmBuilder.Function.Index).init(allocator),
            .cold_shims = std.StringHashMap(ColdShim).init(allocator),
            .static_bytes = std.StringHashMap(LlvmBuilder.Value).init(allocator),
            .static_refcounted_backings = std.AutoHashMap(u32, LlvmBuilder.Value).init(allocator),
            .static_data_globals = std.AutoHashMap(u32, LlvmBuilder.Value).init(allocator),
            .rc_helpers = std.AutoHashMap(u64, RcHelperEntry).init(allocator),
            .boxy_capture_drop_helpers = std.AutoHashMap(u64, BoxyCaptureDropHelper).init(allocator),
            .boxy_dict_thunks = std.AutoHashMap(u32, LlvmBuilder.Function.Index).init(allocator),
            .join_points = std.AutoHashMap(u32, JoinInfo).init(allocator),
            .compiled_joins = std.AutoHashMap(u32, void).init(allocator),
            .stmt_incoming_counts = std.AutoHashMap(u32, u32).init(allocator),
            .stmt_entry_blocks = std.AutoHashMap(u32, LlvmBuilder.Function.Block.Index).init(allocator),
            .loop_continue_blocks = .empty,
            .loop_break_blocks = .empty,
            .debug_inline_subprograms = std.AutoHashMap(u32, LlvmBuilder.Metadata).init(allocator),
            .debug_inline_callsites = std.AutoHashMap(u32, LlvmBuilder.Metadata).init(allocator),
            .debug_types = std.AutoHashMap(u32, LlvmBuilder.Metadata).init(allocator),
        };
    }

    /// Initializes the backend for an explicit target.
    pub fn initWithTarget(
        allocator: Allocator,
        store: *const lir.LirStore,
        erased_arg_desc_offsets: []const lir.LIR.ErasedArgDescOffset,
        erased_arg_desc_params: []const lir.LIR.ErasedArgDescParam,
        target: std.Target,
    ) MonoLlvmCodeGen {
        var self = init(allocator, store, erased_arg_desc_offsets, erased_arg_desc_params);
        self.target = target;
        self.triple = getLlvmTriple(target);
        self.data_layout = getLlvmDataLayout(target);
        return self;
    }

    /// Initializes the backend for a relocatable object linked with target builtins.
    pub fn initForLinkedObject(
        allocator: Allocator,
        store: *const lir.LirStore,
        erased_arg_desc_offsets: []const lir.LIR.ErasedArgDescOffset,
        erased_arg_desc_params: []const lir.LIR.ErasedArgDescParam,
        target: std.Target,
    ) MonoLlvmCodeGen {
        // Linked objects use the symbol ABI: hosted functions are direct
        // extern calls and no RocOps reaches compiled code from the host.
        var self = initWithTarget(allocator, store, erased_arg_desc_offsets, erased_arg_desc_params, target);
        self.builtin_symbol_mode = .native_object;
        self.proc_symbol_mode = .lir_symbol;
        self.host_call_mode = .extern_symbols;
        return self;
    }

    /// Releases backend-owned scratch maps.
    pub fn deinit(self: *MonoLlvmCodeGen) void {
        self.debug_types.deinit();
        self.debug_inline_callsites.deinit();
        self.debug_inline_subprograms.deinit();
        self.proc_registry.deinit();
        self.builtin_functions.deinit();
        self.cold_shims.deinit();
        self.clearStaticBytes();
        self.static_bytes.deinit();
        self.static_refcounted_backings.deinit();
        self.static_data_globals.deinit();
        self.rc_helpers.deinit();
        self.boxy_capture_drop_helpers.deinit();
        self.boxy_dict_thunks.deinit();
        self.join_points.deinit();
        self.compiled_joins.deinit();
        self.stmt_incoming_counts.deinit();
        self.stmt_entry_blocks.deinit();
        self.loop_continue_blocks.deinit(self.allocator);
        self.loop_break_blocks.deinit(self.allocator);
    }

    /// Clears per-module caches while retaining allocated capacity.
    pub fn reset(self: *MonoLlvmCodeGen) void {
        self.proc_registry.clearRetainingCapacity();
        self.builtin_functions.clearRetainingCapacity();
        self.cold_shims.clearRetainingCapacity();
        self.cold_depth = 0;
        self.clearStaticBytes();
        self.static_refcounted_backings.clearRetainingCapacity();
        self.static_data_globals.clearRetainingCapacity();
        self.rc_helpers.clearRetainingCapacity();
        self.boxy_capture_drop_helpers.clearRetainingCapacity();
        self.boxy_dict_thunks.clearRetainingCapacity();
        self.join_points.clearRetainingCapacity();
        self.compiled_joins.clearRetainingCapacity();
        self.stmt_incoming_counts.clearRetainingCapacity();
        self.stmt_entry_blocks.clearRetainingCapacity();
        self.loop_continue_blocks.clearRetainingCapacity();
        self.loop_break_blocks.clearRetainingCapacity();
        self.string_counter = 0;
        self.runtime_error_func = null;
        self.boxy_fn_table_global = null;
        self.debug_compile_unit = .none;
        self.debug_enums_fwd_ref = .none;
        self.debug_globals_fwd_ref = .none;
        self.current_subprogram = .none;
        self.current_debug_file = SourceLoc.no_file;
        self.current_source_stmt = null;
        self.debug_inline_subprograms.clearRetainingCapacity();
        self.debug_inline_callsites.clearRetainingCapacity();
        self.debug_types.clearRetainingCapacity();
        self.expect_err_region_global = null;
        self.boxy_runtime_used = false;
    }

    fn clearStaticBytes(self: *MonoLlvmCodeGen) void {
        var it = self.static_bytes.keyIterator();
        while (it.next()) |key| {
            self.allocator.free(key.*);
        }
        self.static_bytes.clearRetainingCapacity();
    }

    /// Generates a single eval-style module for `root_proc`.
    pub fn generateCode(
        self: *MonoLlvmCodeGen,
        root_proc: LirProcSpecId,
        result_layout: layout.Idx,
    ) Error!GenerateResult {
        const proc = self.store.getProcSpec(root_proc);
        const arg_layouts = try self.procArgLayouts(proc, .explicit);
        defer self.allocator.free(arg_layouts);
        const EvalEntrypoint = struct {
            symbol_name: []const u8,
            proc: LirProcSpecId,
            arg_layouts: []const layout.Idx,
            ret_layout: layout.Idx,
        };
        const entrypoints = [_]EvalEntrypoint{.{
            .symbol_name = "roc_eval",
            .proc = root_proc,
            .arg_layouts = arg_layouts,
            .ret_layout = result_layout,
        }};
        return self.generateEntrypointModule("roc_eval_module", entrypoints[0..]);
    }

    /// Generates a module with exported wrappers for the requested entrypoints.
    /// An entrypoint the generated interpreter shim exposes: the natural
    /// C-ABI wrapper marshals into interpreter buffers and dispatches by
    /// ordinal through roc_entrypoint.
    pub const ShimEntrypoint = struct {
        symbol_name: []const u8,
        entry_index: u32,
        arg_layouts: []const layout.Idx,
        ret_layout: layout.Idx,
    };

    const ShimTarget = struct {
        entry_index: u32,
        image: ?struct { value: LlvmBuilder.Value, len: usize },
    };

    /// Generate the interpreter platform shim module: natural C-ABI entrypoint
    /// wrappers under the provides symbols (dispatching into the prelinked
    /// interpreter), the hosted dispatch table built from the platform's
    /// hosted-section symbols, and optionally the embedded LIR image.
    pub fn generateInterpreterShimModule(
        self: *MonoLlvmCodeGen,
        module_name: []const u8,
        entrypoints: []const ShimEntrypoint,
        hosted_symbols: []const []const u8,
        image: ?[]const u8,
        default_run_start: bool,
    ) Error!GenerateResult {
        self.reset();

        var builder = try self.createBuilder(module_name);
        defer builder.deinit();

        self.builder = &builder;
        defer self.builder = null;

        const ptr_ty = builder.ptrType(.default) catch return error.OutOfMemory;
        const usize_ty: LlvmBuilder.Type = if (self.targetWordSize() == 8) .i64 else .i32;

        // Hosted dispatch table: extern declarations for each hosted symbol,
        // collected into roc_shim_hosted_fns/roc_shim_hosted_count for the
        // interpreter's RocOps.
        var fn_consts = std.ArrayList(LlvmBuilder.Constant).empty;
        defer fn_consts.deinit(self.allocator);
        const dummy_fn_ty = builder.fnType(.void, &.{}, .normal) catch return error.OutOfMemory;
        for (hosted_symbols) |symbol| {
            // Hosted functions the app never references leave null entries;
            // dispatch can only reach indices that have LIR hosted procs.
            if (symbol.len == 0) {
                try fn_consts.append(self.allocator, builder.nullConst(ptr_ty) catch return error.OutOfMemory);
                continue;
            }
            const fn_name = builder.strtabString(symbol) catch return error.OutOfMemory;
            const func = builder.addFunction(dummy_fn_ty, fn_name, .default) catch return error.OutOfMemory;
            func.setLinkage(.extern_weak, &builder);
            try fn_consts.append(self.allocator, func.toConst(&builder));
        }
        const table_len = @max(hosted_symbols.len, 1);
        if (hosted_symbols.len == 0) {
            try fn_consts.append(self.allocator, builder.nullConst(ptr_ty) catch return error.OutOfMemory);
        }
        const table_ty = builder.arrayType(table_len, ptr_ty) catch return error.OutOfMemory;
        const table_var = builder.addVariable(builder.strtabString(shim_symbols.roc_shim_hosted_fns_table) catch return error.OutOfMemory, table_ty, .default) catch return error.OutOfMemory;
        table_var.ptrConst(&builder).global.setLinkage(.internal, &builder);
        table_var.setMutability(.constant, &builder);
        table_var.setInitializer(builder.arrayConst(table_ty, fn_consts.items) catch return error.OutOfMemory, &builder) catch return error.OutOfMemory;

        const table_ptr_var = builder.addVariable(builder.strtabString(shim_symbols.roc_shim_hosted_fns) catch return error.OutOfMemory, ptr_ty, .default) catch return error.OutOfMemory;
        table_ptr_var.setMutability(.constant, &builder);
        table_ptr_var.setInitializer(table_var.toConst(&builder), &builder) catch return error.OutOfMemory;

        const count_var = builder.addVariable(builder.strtabString(shim_symbols.roc_shim_hosted_count) catch return error.OutOfMemory, usize_ty, .default) catch return error.OutOfMemory;
        count_var.setMutability(.constant, &builder);
        count_var.setInitializer(builder.intConst(usize_ty, hosted_symbols.len) catch return error.OutOfMemory, &builder) catch return error.OutOfMemory;

        // Embedded LIR image bytes, when building a standalone interpreter binary.
        const image_ref: @FieldType(ShimTarget, "image") = if (image) |bytes| .{
            .value = try self.staticBytes(bytes),
            .len = bytes.len,
        } else null;

        for (entrypoints) |entrypoint| {
            try self.generateCAbiEntrypointWrapper(
                entrypoint.symbol_name,
                null,
                entrypoint.arg_layouts,
                entrypoint.ret_layout,
                .{ .entry_index = entrypoint.entry_index, .image = image_ref },
            );
        }

        if (default_run_start) {
            try self.emitDefaultRunStartModuleAsm(&builder);
        }

        return .{
            .bitcode = try self.serializeBuilderToBitcode(&builder),
            .allocator = self.allocator,
        };
    }

    fn emitDefaultRunStartModuleAsm(self: *MonoLlvmCodeGen, builder: *LlvmBuilder) Error!void {
        if (self.target.os.tag != .linux) return error.CompilationFailed;

        var aw: std.Io.Writer.Allocating = .init(self.allocator);
        defer aw.deinit();
        const w = &aw.writer;

        if (self.target.cpu.arch == .x86_64) {
            w.print(
                \\.text
                \\.globl _start
                \\.type _start,@function
                \\_start:
                \\    mov %rsp, %rbx
                \\    and $-16, %rsp
                \\    call {s}
                \\    mov (%rbx), %rdi
                \\    lea 8(%rbx), %rsi
                \\    call {s}
                \\    mov %rax, %rdi
                \\    mov $60, %rax
                \\    syscall
                \\    ud2
                \\.size _start, .-_start
                \\
            , .{ shim_symbols.roc_default_runtime_init, shim_symbols.roc_shim_default_main }) catch return error.OutOfMemory;
        } else if (self.target.cpu.arch == .aarch64) {
            w.print(
                \\.text
                \\.globl _start
                \\.type _start,%function
                \\_start:
                \\    mov x19, sp
                \\    bl {s}
                \\    ldr x0, [x19]
                \\    add x1, x19, #8
                \\    bl {s}
                \\    mov x8, #94
                \\    svc #0
                \\    brk #0
                \\.size _start, .-_start
                \\
            , .{ shim_symbols.roc_default_runtime_init, shim_symbols.roc_shim_default_main }) catch return error.OutOfMemory;
        } else {
            return error.CompilationFailed;
        }

        builder.finishModuleAsm(&aw) catch return error.OutOfMemory;
    }

    pub fn generateEntrypointModule(
        self: *MonoLlvmCodeGen,
        module_name: []const u8,
        entrypoints: []const Entrypoint,
    ) Error!GenerateResult {
        self.reset();

        var builder = try self.createBuilder(module_name);
        defer builder.deinit();

        self.builder = &builder;
        defer self.builder = null;

        if (self.host_call_mode == .vtable) {
            _ = try self.boxyFnTableGlobal();
        }

        if (!builder.strip) {
            try self.setupDebugInfo(&builder, module_name);
            if (self.target.ofmt == .elf) try self.embedGdbScript(&builder);
        }

        const procs = self.store.getProcSpecs();
        try self.compileAllProcSpecs(procs);
        for (self.static_data_rc_helpers) |helper_key| {
            _ = try self.declareRcHelper(helper_key, .atomic);
        }
        try self.compilePendingRcHelpers();
        try self.compilePendingBoxyCaptureDropHelpers();
        try self.compilePendingColdShims();

        for (entrypoints) |entrypoint| {
            try self.generateEntrypointWrapper(
                entrypoint.symbol_name,
                entrypoint.proc,
                entrypoint.arg_layouts,
                entrypoint.ret_layout,
                entrypoint.abi,
            );
        }

        try self.emitPluginStampFunction();

        if (self.enable_default_platform_runtime) {
            try self.emitDefaultPlatformBacktraceTable();
        }

        if (!builder.strip) {
            const empty_tuple = builder.metadataTuple(&.{}) catch return error.OutOfMemory;
            builder.resolveDebugForwardReference(self.debug_enums_fwd_ref.unwrap().?, empty_tuple);
            builder.resolveDebugForwardReference(self.debug_globals_fwd_ref.unwrap().?, empty_tuple);
        }

        return .{
            .bitcode = try self.serializeBuilderToBitcode(&builder),
            .allocator = self.allocator,
        };
    }

    /// Creates the compile unit, registers it in `llvm.dbg.cu`, and sets the
    /// module flags DWARF emission requires.
    fn setupDebugInfo(self: *MonoLlvmCodeGen, builder: *LlvmBuilder, module_name: []const u8) Error!void {
        const cu_file_name = if (self.store.sourceFileCount() > 0)
            self.store.sourceFileName(0)
        else
            module_name;
        const cu_file = builder.debugFile(
            builder.metadataString(cu_file_name) catch return error.OutOfMemory,
            builder.metadataString(".") catch return error.OutOfMemory,
        ) catch return error.OutOfMemory;

        self.debug_enums_fwd_ref = (builder.debugForwardReference() catch return error.OutOfMemory).toOptional();
        self.debug_globals_fwd_ref = (builder.debugForwardReference() catch return error.OutOfMemory).toOptional();

        const compile_unit = builder.debugCompileUnit(
            cu_file,
            builder.metadataString(self.debug_producer) catch return error.OutOfMemory,
            self.debug_enums_fwd_ref.unwrap().?,
            self.debug_globals_fwd_ref.unwrap().?,
            .{ .optimized = false },
        ) catch return error.OutOfMemory;
        self.debug_compile_unit = compile_unit.toOptional();
        builder.addNamedMetadata(
            builder.string("llvm.dbg.cu") catch return error.OutOfMemory,
            &.{compile_unit},
        ) catch return error.OutOfMemory;

        const behavior_warning = builder.metadataConstant(
            builder.intConst(.i32, 2) catch return error.OutOfMemory,
        ) catch return error.OutOfMemory;
        const behavior_max = builder.metadataConstant(
            builder.intConst(.i32, 7) catch return error.OutOfMemory,
        ) catch return error.OutOfMemory;
        const debug_info_version = builder.metadataTuple(&.{
            behavior_warning,
            (builder.metadataString("Debug Info Version") catch return error.OutOfMemory).toMetadata(),
            builder.metadataConstant(builder.intConst(.i32, 3) catch return error.OutOfMemory) catch return error.OutOfMemory,
        }) catch return error.OutOfMemory;
        const dwarf_version = builder.metadataTuple(&.{
            behavior_max,
            (builder.metadataString("Dwarf Version") catch return error.OutOfMemory).toMetadata(),
            builder.metadataConstant(builder.intConst(.i32, 4) catch return error.OutOfMemory) catch return error.OutOfMemory,
        }) catch return error.OutOfMemory;
        builder.addNamedMetadata(
            builder.string("llvm.module.flags") catch return error.OutOfMemory,
            &.{ debug_info_version, dwarf_version },
        ) catch return error.OutOfMemory;
    }

    /// Inlines the gdb pretty-printer script into the binary's
    /// .debug_gdb_scripts section (entry kind 4 = inlined Python text), so
    /// gdb auto-loads formatters that match the compiler that built the
    /// binary. The section is non-allocatable ("MS" flags), so it survives
    /// --gc-sections and never gets mapped at runtime.
    fn embedGdbScript(self: *MonoLlvmCodeGen, builder: *LlvmBuilder) Error!void {
        const script = @embedFile("debugger/roc_gdb.py");
        var aw: std.Io.Writer.Allocating = .init(self.allocator);
        defer aw.deinit();
        const w = &aw.writer;
        w.writeAll(
            \\.pushsection ".debug_gdb_scripts","MS",@progbits,1
            \\.byte 4
            \\.ascii "roc-formatters\n"
            \\
        ) catch return error.OutOfMemory;
        var lines = std.mem.splitScalar(u8, script, '\n');
        while (lines.next()) |line| {
            w.writeAll(".ascii \"") catch return error.OutOfMemory;
            for (line) |byte| {
                switch (byte) {
                    '"' => w.writeAll("\\\"") catch return error.OutOfMemory,
                    '\\' => w.writeAll("\\\\") catch return error.OutOfMemory,
                    else => w.writeByte(byte) catch return error.OutOfMemory,
                }
            }
            w.writeAll("\\n\"\n") catch return error.OutOfMemory;
        }
        w.writeAll(
            \\.byte 0
            \\.popsection
            \\
        ) catch return error.OutOfMemory;
        builder.finishModuleAsm(&aw) catch return error.OutOfMemory;
    }

    fn emitDefaultPlatformBacktraceTable(self: *MonoLlvmCodeGen) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const ptr_ty = builder.ptrType(.default) catch return error.OutOfMemory;
        const usize_ty = self.ptrSizedIntType();
        const entry_ty = builder.structType(.normal, &.{ usize_ty, usize_ty, ptr_ty, usize_ty, ptr_ty, usize_ty, .i32, .i32 }) catch return error.OutOfMemory;

        var entries = std.ArrayList(LlvmBuilder.Constant).empty;
        defer entries.deinit(self.allocator);
        try entries.ensureTotalCapacity(self.allocator, @max(self.proc_registry.count(), 1));

        var proc_iter = self.proc_registry.iterator();
        while (proc_iter.next()) |entry| {
            const proc_id: LirProcSpecId = @enumFromInt(entry.key_ptr.*);
            const proc = self.store.getProcSpec(proc_id);
            const loc = self.store.procLoc(proc_id);
            var allocated_name: ?[]u8 = null;
            defer if (allocated_name) |name| self.allocator.free(name);
            const name = self.store.procDebugName(proc_id) orelse blk: {
                const symbol_name = try std.fmt.allocPrint(self.allocator, "roc__proc_{x}", .{proc.name.raw()});
                allocated_name = symbol_name;
                break :blk symbol_name;
            };
            const file = if (loc.file == SourceLoc.no_file or loc.file >= self.store.sourceFileCount())
                ""
            else
                self.store.sourceFileName(loc.file);

            const name_ptr = (try self.staticBytes(name)).toConst().?;
            const file_ptr = (try self.staticBytes(file)).toConst().?;
            const start_addr = builder.castConst(.ptrtoint, entry.value_ptr.*.toConst(builder), usize_ty) catch return error.OutOfMemory;

            entries.appendAssumeCapacity(builder.structConst(entry_ty, &.{
                start_addr,
                builder.intConst(usize_ty, 0) catch return error.OutOfMemory,
                name_ptr,
                builder.intConst(usize_ty, name.len) catch return error.OutOfMemory,
                file_ptr,
                builder.intConst(usize_ty, file.len) catch return error.OutOfMemory,
                builder.intConst(.i32, loc.line) catch return error.OutOfMemory,
                builder.intConst(.i32, loc.column) catch return error.OutOfMemory,
            }) catch return error.OutOfMemory);
        }

        if (entries.items.len == 0) {
            entries.appendAssumeCapacity(builder.structConst(entry_ty, &.{
                builder.intConst(usize_ty, 0) catch return error.OutOfMemory,
                builder.intConst(usize_ty, 0) catch return error.OutOfMemory,
                builder.nullConst(ptr_ty) catch return error.OutOfMemory,
                builder.intConst(usize_ty, 0) catch return error.OutOfMemory,
                builder.nullConst(ptr_ty) catch return error.OutOfMemory,
                builder.intConst(usize_ty, 0) catch return error.OutOfMemory,
                builder.intConst(.i32, 0) catch return error.OutOfMemory,
                builder.intConst(.i32, 0) catch return error.OutOfMemory,
            }) catch return error.OutOfMemory);
        }

        const table_ty = builder.arrayType(entries.items.len, entry_ty) catch return error.OutOfMemory;
        const table_data = builder.addVariable(builder.strtabString("roc_default_backtrace_table_data") catch return error.OutOfMemory, table_ty, .default) catch return error.OutOfMemory;
        table_data.ptrConst(builder).global.setLinkage(.internal, builder);
        table_data.setMutability(.constant, builder);
        table_data.setInitializer(builder.arrayConst(table_ty, entries.items) catch return error.OutOfMemory, builder) catch return error.OutOfMemory;

        const table_var = builder.addVariable(builder.strtabString(shim_symbols.roc_default_backtrace_table) catch return error.OutOfMemory, ptr_ty, .default) catch return error.OutOfMemory;
        table_var.setMutability(.constant, builder);
        table_var.setInitializer(table_data.toConst(builder), builder) catch return error.OutOfMemory;

        const count_var = builder.addVariable(builder.strtabString(shim_symbols.roc_default_backtrace_count) catch return error.OutOfMemory, usize_ty, .default) catch return error.OutOfMemory;
        count_var.setMutability(.constant, builder);
        count_var.setInitializer(builder.intConst(usize_ty, self.proc_registry.count()) catch return error.OutOfMemory, builder) catch return error.OutOfMemory;
    }

    /// DIFile metadata for one source file table entry (interned by the
    /// builder, so repeated calls are cheap).
    fn debugFileFor(self: *MonoLlvmCodeGen, builder: *LlvmBuilder, file: u32) Error!LlvmBuilder.Metadata {
        const name = if (file == SourceLoc.no_file)
            "<roc-generated>"
        else
            self.store.sourceFileName(file);
        return builder.debugFile(
            builder.metadataString(name) catch return error.OutOfMemory,
            builder.metadataString(".") catch return error.OutOfMemory,
        ) catch return error.OutOfMemory;
    }

    fn procDebugName(
        self: *MonoLlvmCodeGen,
        builder: *LlvmBuilder,
        proc_id: LirProcSpecId,
        proc: LirProcSpec,
    ) Error!LlvmBuilder.Metadata.String {
        if (self.enable_default_platform_diagnostics) {
            if (self.store.procDebugName(proc_id)) |name| {
                return builder.metadataString(name) catch return error.OutOfMemory;
            }
        }
        return try self.procSymbolDebugName(builder, proc_id, proc);
    }

    fn procSymbolDebugName(
        self: *MonoLlvmCodeGen,
        builder: *LlvmBuilder,
        proc_id: LirProcSpecId,
        proc: LirProcSpec,
    ) Error!LlvmBuilder.Metadata.String {
        return switch (self.proc_symbol_mode) {
            .local_index => builder.metadataStringFmt("roc_proc_{d}", .{@intFromEnum(proc_id)}) catch return error.OutOfMemory,
            .lir_symbol => builder.metadataStringFmt("roc__proc_{x}", .{proc.name.raw()}) catch return error.OutOfMemory,
        };
    }

    /// Debug type metadata for a layout, memoized per module build. A forward
    /// reference is registered before children are built so recursive layouts
    /// (e.g. a tag union containing a list of itself) terminate.
    fn debugTypeFor(self: *MonoLlvmCodeGen, builder: *LlvmBuilder, idx: layout.Idx) Error!LlvmBuilder.Metadata {
        if (self.debug_types.get(@intFromEnum(idx))) |existing| return existing;
        const fwd_ref = builder.debugForwardReference() catch return error.OutOfMemory;
        try self.debug_types.put(@intFromEnum(idx), fwd_ref);
        const resolved = try self.buildDebugType(builder, idx);
        builder.resolveDebugForwardReference(fwd_ref, resolved);
        try self.debug_types.put(@intFromEnum(idx), resolved);
        return resolved;
    }

    fn debugUsizeType(self: *MonoLlvmCodeGen, builder: *LlvmBuilder) Error!LlvmBuilder.Metadata {
        const bits: u64 = self.target.ptrBitWidth();
        return builder.debugUnsignedType(
            builder.metadataString(if (bits == 32) "U32" else "U64") catch return error.OutOfMemory,
            bits,
        ) catch return error.OutOfMemory;
    }

    /// `Str` and `List` are both three words starting with a bytes pointer,
    /// but the order of their remaining two fields differs.
    fn debugSequenceType(
        self: *MonoLlvmCodeGen,
        builder: *LlvmBuilder,
        name: []const u8,
        elem_ptr_ty: LlvmBuilder.Metadata,
        second_field: []const u8,
        third_field: []const u8,
        size_bits: u64,
        align_bits: u64,
    ) Error!LlvmBuilder.Metadata {
        const usize_ty = try self.debugUsizeType(builder);
        const word_bits: u64 = self.target.ptrBitWidth();
        const members = [_]LlvmBuilder.Metadata{
            builder.debugMemberType(
                builder.metadataString("bytes") catch return error.OutOfMemory,
                null,
                self.debug_compile_unit.unwrap(),
                0,
                elem_ptr_ty,
                word_bits,
                word_bits,
                0,
            ) catch return error.OutOfMemory,
            builder.debugMemberType(
                builder.metadataString(second_field) catch return error.OutOfMemory,
                null,
                self.debug_compile_unit.unwrap(),
                0,
                usize_ty,
                word_bits,
                word_bits,
                word_bits,
            ) catch return error.OutOfMemory,
            builder.debugMemberType(
                builder.metadataString(third_field) catch return error.OutOfMemory,
                null,
                self.debug_compile_unit.unwrap(),
                0,
                usize_ty,
                word_bits,
                word_bits,
                word_bits * 2,
            ) catch return error.OutOfMemory,
        };
        return builder.debugStructType(
            builder.metadataString(name) catch return error.OutOfMemory,
            null,
            self.debug_compile_unit.unwrap(),
            0,
            null,
            size_bits,
            align_bits,
            builder.metadataTuple(&members) catch return error.OutOfMemory,
        ) catch return error.OutOfMemory;
    }

    fn buildDebugType(self: *MonoLlvmCodeGen, builder: *LlvmBuilder, idx: layout.Idx) Error!LlvmBuilder.Metadata {
        const lay = self.layoutValue(idx);
        const sa = self.sizeAlignOf(idx);
        const size_bits: u64 = @as(u64, sa.size) * 8;
        const align_bits: u64 = @as(u64, @intCast(sa.alignment.toByteUnits())) * 8;
        const word_bits: u64 = self.target.ptrBitWidth();

        switch (lay.tag) {
            .scalar => {
                const scalar = lay.getScalar();
                switch (scalar.tag) {
                    .str => {
                        const u8_ty = builder.debugUnsignedType(
                            builder.metadataString("U8") catch return error.OutOfMemory,
                            8,
                        ) catch return error.OutOfMemory;
                        const bytes_ptr = builder.debugPointerType(
                            null,
                            null,
                            null,
                            0,
                            u8_ty,
                            word_bits,
                            word_bits,
                            0,
                        ) catch return error.OutOfMemory;
                        return try self.debugSequenceType(builder, "Str", bytes_ptr, "capacity_or_alloc_ptr", "length", size_bits, align_bits);
                    },
                    .int => {
                        const precision = scalar.getInt();
                        const name = @tagName(precision);
                        var upper_buf: [4]u8 = undefined;
                        const upper = std.ascii.upperString(&upper_buf, name);
                        const bits: u64 = @as(u64, precision.size()) * 8;
                        return switch (precision) {
                            .i8, .i16, .i32, .i64, .i128 => builder.debugSignedType(
                                builder.metadataString(upper) catch return error.OutOfMemory,
                                bits,
                            ) catch return error.OutOfMemory,
                            .u8, .u16, .u32, .u64, .u128 => builder.debugUnsignedType(
                                builder.metadataString(upper) catch return error.OutOfMemory,
                                bits,
                            ) catch return error.OutOfMemory,
                        };
                    },
                    .frac => return switch (scalar.getFrac()) {
                        .f32 => builder.debugFloatType(
                            builder.metadataString("F32") catch return error.OutOfMemory,
                            32,
                        ) catch return error.OutOfMemory,
                        .f64 => builder.debugFloatType(
                            builder.metadataString("F64") catch return error.OutOfMemory,
                            64,
                        ) catch return error.OutOfMemory,
                        .dec => builder.debugSignedType(
                            builder.metadataString("Dec") catch return error.OutOfMemory,
                            128,
                        ) catch return error.OutOfMemory,
                    },
                    .vector => {
                        const vector = scalar.getVector();
                        const lane_ty = if (vector.isSigned())
                            builder.debugSignedType(
                                builder.metadataString(@tagName(vector.lanePrecision())) catch return error.OutOfMemory,
                                vector.laneBits(),
                            ) catch return error.OutOfMemory
                        else
                            builder.debugUnsignedType(
                                builder.metadataString(@tagName(vector.lanePrecision())) catch return error.OutOfMemory,
                                vector.laneBits(),
                            ) catch return error.OutOfMemory;
                        return builder.debugVectorType(
                            builder.metadataString(@tagName(vector)) catch return error.OutOfMemory,
                            null,
                            self.debug_compile_unit.unwrap(),
                            0,
                            lane_ty,
                            128,
                            128,
                            null,
                        ) catch return error.OutOfMemory;
                    },
                    .opaque_ptr => return builder.debugPointerType(
                        builder.metadataString("OpaquePtr") catch return error.OutOfMemory,
                        null,
                        null,
                        0,
                        null,
                        word_bits,
                        word_bits,
                        0,
                    ) catch return error.OutOfMemory,
                }
            },
            .box, .box_of_zst, .erased_box => {
                const elem_ty: ?LlvmBuilder.Metadata = if (lay.tag == .box)
                    try self.debugTypeFor(builder, lay.getIdx())
                else
                    null;
                return builder.debugPointerType(
                    builder.metadataString("Box") catch return error.OutOfMemory,
                    null,
                    null,
                    0,
                    elem_ty,
                    word_bits,
                    word_bits,
                    0,
                ) catch return error.OutOfMemory;
            },
            .ptr => {
                const elem_ty = try self.debugTypeFor(builder, lay.getIdx());
                return builder.debugPointerType(
                    builder.metadataString("Ptr") catch return error.OutOfMemory,
                    null,
                    null,
                    0,
                    elem_ty,
                    word_bits,
                    word_bits,
                    0,
                ) catch return error.OutOfMemory;
            },
            .list, .list_of_zst => {
                const elem_ty: LlvmBuilder.Metadata = if (lay.tag == .list)
                    try self.debugTypeFor(builder, lay.getIdx())
                else
                    builder.debugUnsignedType(
                        builder.metadataString("U8") catch return error.OutOfMemory,
                        8,
                    ) catch return error.OutOfMemory;
                const elem_ptr = builder.debugPointerType(
                    null,
                    null,
                    null,
                    0,
                    elem_ty,
                    word_bits,
                    word_bits,
                    0,
                ) catch return error.OutOfMemory;
                return try self.debugSequenceType(builder, "List", elem_ptr, "length", "capacity_or_alloc_ptr", size_bits, align_bits);
            },
            .struct_ => {
                const struct_idx = lay.getStruct().idx;
                const data = self.layouts().getStructData(struct_idx);
                const sorted_fields = self.layouts().struct_fields.sliceRange(data.getFields());
                // Padding spacers are not real members; describe only named fields.
                var named_count: usize = 0;
                for (0..sorted_fields.len) |i| {
                    if (!sorted_fields.get(@intCast(i)).is_padding) named_count += 1;
                }
                const members = try self.allocator.alloc(LlvmBuilder.Metadata, named_count);
                defer self.allocator.free(members);
                var member_index: usize = 0;
                for (0..sorted_fields.len) |sorted_index| {
                    const field = sorted_fields.get(@intCast(sorted_index));
                    if (field.is_padding) continue;
                    const field_layout = field.layout;
                    const field_offset = self.layouts().getStructFieldOffset(struct_idx, @intCast(sorted_index));
                    const field_sa = self.sizeAlignOf(field_layout);
                    members[member_index] = builder.debugMemberType(
                        builder.metadataStringFmt("f{d}", .{field.index}) catch return error.OutOfMemory,
                        null,
                        self.debug_compile_unit.unwrap(),
                        0,
                        try self.debugTypeFor(builder, field_layout),
                        @as(u64, field_sa.size) * 8,
                        @as(u64, @intCast(field_sa.alignment.toByteUnits())) * 8,
                        @as(u64, field_offset) * 8,
                    ) catch return error.OutOfMemory;
                    member_index += 1;
                }
                return builder.debugStructType(
                    builder.metadataString("Record") catch return error.OutOfMemory,
                    null,
                    self.debug_compile_unit.unwrap(),
                    0,
                    null,
                    size_bits,
                    align_bits,
                    builder.metadataTuple(members) catch return error.OutOfMemory,
                ) catch return error.OutOfMemory;
            },
            .tag_union => {
                const data = self.layouts().getTagUnionData(lay.getTagUnion().idx);
                var members: std.ArrayList(LlvmBuilder.Metadata) = .empty;
                defer members.deinit(self.allocator);
                if (data.discriminant_size > 0) {
                    const disc_bits = @as(u64, data.discriminant_size) * 8;
                    try members.append(self.allocator, builder.debugMemberType(
                        builder.metadataString("discriminant") catch return error.OutOfMemory,
                        null,
                        self.debug_compile_unit.unwrap(),
                        0,
                        builder.debugUnsignedType(
                            builder.metadataString("U8") catch return error.OutOfMemory,
                            disc_bits,
                        ) catch return error.OutOfMemory,
                        disc_bits,
                        disc_bits,
                        @as(u64, data.discriminant_offset.get(self.layouts().targetUsize())) * 8,
                    ) catch return error.OutOfMemory);
                }
                return builder.debugStructType(
                    builder.metadataString("TagUnion") catch return error.OutOfMemory,
                    null,
                    self.debug_compile_unit.unwrap(),
                    0,
                    null,
                    size_bits,
                    align_bits,
                    builder.metadataTuple(members.items) catch return error.OutOfMemory,
                ) catch return error.OutOfMemory;
            },
            .closure, .erased_callable, .zst => {
                const name = switch (lay.tag) {
                    .closure => "Closure",
                    .erased_callable => "ErasedCallable",
                    .zst => "Unit",
                    .scalar, .box, .box_of_zst, .erased_box, .list, .list_of_zst, .struct_, .tag_union, .ptr => unreachable,
                };
                return builder.debugStructType(
                    builder.metadataString(name) catch return error.OutOfMemory,
                    null,
                    self.debug_compile_unit.unwrap(),
                    0,
                    null,
                    size_bits,
                    align_bits,
                    null,
                ) catch return error.OutOfMemory;
            },
        }
    }

    /// Emits a dbg.declare for every named local in the proc's frame so
    /// debuggers can show Roc variables by their source names.
    fn declareFrameLocals(self: *MonoLlvmCodeGen, proc: LirProcSpec, proc_line: u32) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const scope = self.current_subprogram.unwrap() orelse return;
        const file = try self.debugFileFor(builder, self.current_debug_file);
        const empty_expr = builder.debugExpression(&.{}) catch return error.OutOfMemory;
        const previous_debug_location = wip.debug_location;
        wip.debug_location = .{ .location = .{
            .line = proc_line,
            .column = if (proc_line == 0) 0 else 1,
            .scope = scope.toOptional(),
            .inlined_at = .none,
        } };
        defer wip.debug_location = previous_debug_location;

        const frame_locals = self.store.getLocalSpan(proc.frame_locals);
        for (0..frame_locals.len) |i| {
            const local_id = GuardedList.at(frame_locals, i);
            const name = self.store.localName(local_id) orelse continue;
            const local_slot = self.local_slots[@intFromEnum(local_id)];
            const variable = builder.debugLocalVar(
                builder.metadataString(name) catch return error.OutOfMemory,
                file,
                scope,
                proc_line,
                try self.debugTypeFor(builder, local_slot.layout_idx),
            ) catch return error.OutOfMemory;
            _ = wip.callIntrinsic(
                .normal,
                .none,
                .@"dbg.declare",
                &.{},
                &.{
                    (wip.debugValue(local_slot.ptr) catch return error.OutOfMemory).toValue(),
                    variable.toValue(),
                    empty_expr.toValue(),
                },
                "",
            ) catch return error.OutOfMemory;
        }
    }

    /// Declares and compiles every procedure in dependency-index order.
    pub fn compileAllProcSpecs(self: *MonoLlvmCodeGen, procs: []const LirProcSpec) Error!void {
        for (procs, 0..) |proc, i| {
            if (proc.is_static_initializer) continue;
            try self.declareProcSpec(@enumFromInt(@as(u32, @intCast(i))), proc);
        }
        try self.declareRuntimeErrorHelper();
        try self.compileRuntimeErrorHelper();
        for (procs, 0..) |proc, i| {
            if (proc.is_static_initializer) continue;
            try self.compileProcBody(@enumFromInt(@as(u32, @intCast(i))), proc);
        }
        if (self.boxy_runtime_used) try self.generateBoxyDictProcThunks(procs);
    }

    fn generateBoxyDictProcThunks(self: *MonoLlvmCodeGen, procs: []const LirProcSpec) Error!void {
        for (procs, 0..) |proc, i| {
            if (proc.is_static_initializer or proc.abi == .erased_callable or proc.hosted != null or proc.body == null) continue;
            try self.generateBoxyDictProcThunk(@enumFromInt(@as(u32, @intCast(i))), proc);
        }
    }

    fn generateBoxyDictProcThunk(self: *MonoLlvmCodeGen, proc_id: LirProcSpecId, proc: LirProcSpec) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const ptr_ty = try self.ptrType();
        const fn_ty = builder.fnType(.void, &.{ ptr_ty, ptr_ty, ptr_ty, ptr_ty, ptr_ty }, .normal) catch return error.OutOfMemory;
        const name = builder.strtabStringFmt("roc_boxy_dict_thunk_{d}", .{@intFromEnum(proc_id)}) catch return error.OutOfMemory;
        const func = builder.addFunction(fn_ty, name, .default) catch return error.OutOfMemory;
        func.setLinkage(.internal, builder);
        var attrs: LlvmBuilder.FunctionAttributes.Wip = .{};
        defer attrs.deinit(builder);
        try self.addGeneratedFunctionStackProbeAttrs(&attrs);
        func.setAttributes(attrs.finish(builder) catch return error.OutOfMemory, builder);
        try self.boxy_dict_thunks.put(@intFromEnum(proc_id), func);

        const outer_wip = self.wip;
        const outer_roc_ops = self.roc_ops_arg;
        const outer_test_context = self.test_context_arg;
        const outer_ret = self.ret_ptr_arg;
        const outer_args = self.args_ptr_arg;
        const outer_capture = self.capture_ptr_arg;
        const outer_ret_desc_ptr = self.ret_desc_ptr_arg;
        const outer_runtime_ret_desc = self.current_runtime_ret_desc;
        const outer_ret_layout = self.current_ret_layout;
        const outer_slots = self.local_slots;
        defer {
            self.wip = outer_wip;
            self.roc_ops_arg = outer_roc_ops;
            self.test_context_arg = outer_test_context;
            self.ret_ptr_arg = outer_ret;
            self.args_ptr_arg = outer_args;
            self.capture_ptr_arg = outer_capture;
            self.ret_desc_ptr_arg = outer_ret_desc_ptr;
            self.current_runtime_ret_desc = outer_runtime_ret_desc;
            self.current_ret_layout = outer_ret_layout;
            self.local_slots = outer_slots;
        }

        var wip = LlvmBuilder.WipFunction.init(builder, .{ .function = func, .strip = true }) catch return error.OutOfMemory;
        defer wip.deinit();
        self.wip = &wip;
        self.roc_ops_arg = wip.arg(0);
        self.test_context_arg = wip.arg(1);
        self.ret_ptr_arg = wip.arg(3);
        self.args_ptr_arg = wip.arg(2);
        self.capture_ptr_arg = null;
        self.ret_desc_ptr_arg = null;
        self.current_runtime_ret_desc = null;
        self.current_ret_layout = proc.ret_layout;
        self.local_slots = &.{};

        const entry = wip.block(0, "entry") catch return error.OutOfMemory;
        wip.cursor = .{ .block = entry };
        const params = self.store.getLocalSpan(proc.args);
        const arg_layouts = try self.procArgLayouts(proc, .all);
        defer self.allocator.free(arg_layouts);
        const offsets = try self.computeArgOffsets(arg_layouts, true);
        defer self.allocator.free(offsets);
        const args_buf = try self.allocArgBuffer(arg_layouts, true);
        const raw_args = wip.arg(2);
        for (0..params.len) |i| {
            const size = self.layoutByteSize(arg_layouts[i]);
            if (size == 0) continue;
            const source_ptr = try self.loadPointer(try self.offsetPtr(raw_args, @intCast(i * self.targetWordSize())));
            try self.copyBytes(try self.offsetPtr(args_buf, offsets[i]), source_ptr, size, self.alignmentForLayout(arg_layouts[i]));
        }

        const proc_fn = self.proc_registry.get(@intFromEnum(proc_id)) orelse return error.CompilationFailed;
        const runtime_out_desc = if (proc.runtime_ret_desc != null)
            try self.boxyOutDescPtr("dict_thunk_runtime_desc")
        else
            null;
        try self.callProcFunctionIndex(proc_fn, proc, wip.arg(3), args_buf, runtime_out_desc, false);

        const return_desc = if (runtime_out_desc) |runtime_desc_ptr|
            try self.loadPointer(runtime_desc_ptr)
        else if (proc.ret_desc) |desc|
            switch (desc) {
                .static => try self.resolveBoxyDesc(desc),
                .local => |local| blk: {
                    var param_index: ?usize = null;
                    for (0..params.len) |i| {
                        if (GuardedList.at(params, i) == local) {
                            param_index = i;
                            break;
                        }
                    }
                    if (param_index) |index| {
                        const source_ptr = try self.loadPointer(try self.offsetPtr(raw_args, @intCast(index * self.targetWordSize())));
                        break :blk try self.loadPointer(source_ptr);
                    }
                    return error.CompilationFailed;
                },
                .runtime, .dict_method_arg, .dict_method_hidden => return error.CompilationFailed,
            }
        else
            try self.boxyNullPtr();
        try self.storePointer(wip.arg(4), return_desc);
        _ = wip.retVoid() catch return error.OutOfMemory;
        try self.finishCurrentWipFunction();
    }

    fn declareRuntimeErrorHelper(self: *MonoLlvmCodeGen) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const ptr_ty = try self.ptrType();
        const fn_ty = builder.fnType(.void, &.{ptr_ty}, .normal) catch return error.OutOfMemory;
        const func = builder.addFunction(fn_ty, builder.strtabString("roc_runtime_error") catch return error.OutOfMemory, .default) catch return error.OutOfMemory;
        func.setLinkage(.internal, builder);

        var attrs: LlvmBuilder.FunctionAttributes.Wip = .{};
        defer attrs.deinit(builder);
        try self.addGeneratedFunctionStackProbeAttrs(&attrs);
        try attrs.addFnAttr(.cold, builder);
        try attrs.addFnAttr(.@"noinline", builder);
        // Linux AArch64 eval tests return from crash callbacks to avoid
        // longjmping through LLVM-generated frames. Every other target lowers
        // `emitCrashBytes` to `unreachable`, so tell LLVM this cold helper does
        // not return; otherwise the hot caller must conservatively preserve
        // state for a control-flow edge that cannot happen.
        if (!(self.target.cpu.arch == .aarch64 and self.target.os.tag == .linux)) {
            try attrs.addFnAttr(.noreturn, builder);
        }
        func.setAttributes(attrs.finish(builder) catch return error.OutOfMemory, builder);

        self.runtime_error_func = func;
    }

    fn compileRuntimeErrorHelper(self: *MonoLlvmCodeGen) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const func = self.runtime_error_func orelse return error.CompilationFailed;

        const outer_wip = self.wip;
        const outer_rc_scratch = self.rc_arg_scratch;
        const outer_roc_ops = self.roc_ops_arg;
        const outer_test_context = self.test_context_arg;
        const outer_ret = self.ret_ptr_arg;
        const outer_args = self.args_ptr_arg;
        const outer_capture = self.capture_ptr_arg;
        const outer_reuse = self.reuse_ptr_arg;
        const outer_ret_desc_ptr = self.ret_desc_ptr_arg;
        const outer_runtime_ret_desc = self.current_runtime_ret_desc;
        const outer_ret_layout = self.current_ret_layout;
        const outer_slots = self.local_slots;
        defer {
            self.wip = outer_wip;
            self.rc_arg_scratch = outer_rc_scratch;
            self.roc_ops_arg = outer_roc_ops;
            self.test_context_arg = outer_test_context;
            self.ret_ptr_arg = outer_ret;
            self.args_ptr_arg = outer_args;
            self.capture_ptr_arg = outer_capture;
            self.reuse_ptr_arg = outer_reuse;
            self.ret_desc_ptr_arg = outer_ret_desc_ptr;
            self.current_runtime_ret_desc = outer_runtime_ret_desc;
            self.current_ret_layout = outer_ret_layout;
            self.local_slots = outer_slots;
        }

        var wip = LlvmBuilder.WipFunction.init(builder, .{ .function = func, .strip = true }) catch return error.OutOfMemory;
        defer wip.deinit();
        self.wip = &wip;
        self.rc_arg_scratch = null;
        self.roc_ops_arg = wip.arg(0);
        self.test_context_arg = null;
        self.ret_ptr_arg = null;
        self.args_ptr_arg = null;
        self.capture_ptr_arg = null;
        self.reuse_ptr_arg = null;
        self.ret_desc_ptr_arg = null;
        self.current_runtime_ret_desc = null;
        self.current_ret_layout = .zst;
        self.local_slots = &.{};

        const entry = wip.block(0, "entry") catch return error.OutOfMemory;
        wip.cursor = .{ .block = entry };
        try self.emitCrashBytes("hit a runtime error");
        try self.finishCurrentWipFunction();
    }

    fn declareProcSpec(self: *MonoLlvmCodeGen, proc_id: LirProcSpecId, proc: LirProcSpec) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const ptr_ty = builder.ptrType(.default) catch return error.OutOfMemory;
        // In-process evaluation threads its explicit test invocation context
        // through erased callables as well as ordinary procedures. The symbol
        // ABI has no such context, so its callable convention remains
        // (ops, ret, args, capture, reuse, out_desc). The vtable convention
        // inserts test_context after ops.
        const params: []const LlvmBuilder.Type = if (proc.abi == .erased_callable and self.host_call_mode == .vtable)
            &.{ ptr_ty, ptr_ty, ptr_ty, ptr_ty, ptr_ty, ptr_ty, ptr_ty }
        else if (proc.abi == .erased_callable)
            &.{ ptr_ty, ptr_ty, ptr_ty, ptr_ty, ptr_ty, ptr_ty }
        else if (self.host_call_mode == .extern_symbols)
            if (proc.runtime_ret_desc != null)
                &.{ ptr_ty, ptr_ty, ptr_ty }
            else
                &.{ ptr_ty, ptr_ty }
        else if (proc.runtime_ret_desc != null)
            &.{ ptr_ty, ptr_ty, ptr_ty, ptr_ty, ptr_ty }
        else
            &.{ ptr_ty, ptr_ty, ptr_ty, ptr_ty };
        const fn_ty = builder.fnType(.void, params, .normal) catch return error.OutOfMemory;
        const name = try self.procFunctionName(builder, proc_id, proc);
        const func = builder.addFunction(fn_ty, name, .default) catch return error.OutOfMemory;
        func.setLinkage(if (self.procNeedsExternalLinkage(proc_id)) .external else .internal, builder);
        var attrs_wip: LlvmBuilder.FunctionAttributes.Wip = .{};
        defer attrs_wip.deinit(builder);
        try self.addGeneratedFunctionStackProbeAttrs(&attrs_wip);
        try attrs_wip.addFnAttr(.inlinehint, builder);
        // Every parameter except the return slot is a distinct object no
        // callee can reach another way: RocOps is host-provided and never
        // stored in a Roc value, the argument pack is a fresh caller-local
        // area holding by-value copies, and a capture record's bytes are
        // never also passed as an argument. The return slot stays
        // unannotated: a return-slot variant can aim it at a reused box
        // interior that an argument value also reaches.
        const ret_param_index: usize = if (proc.abi == .erased_callable and self.host_call_mode == .vtable)
            2
        else if (proc.abi == .erased_callable)
            1
        else if (self.host_call_mode == .extern_symbols)
            0
        else
            2;
        for (0..params.len) |param_index| {
            if (param_index == ret_param_index) continue;
            try attrs_wip.addParamAttr(param_index, .@"noalias", builder);
        }
        // The argument pack is only ever copied out of, and its address never
        // outlives the call, so calls leave it unchanged and it can stay a
        // caller-local object across them.
        try attrs_wip.addParamAttr(ret_param_index + 1, .readonly, builder);
        try attrs_wip.addParamAttr(ret_param_index + 1, .nocapture, builder);
        if (self.enable_default_platform_runtime or self.enable_default_platform_diagnostics) {
            if (self.enable_default_platform_runtime) {
                try attrs_wip.addFnAttr(.{ .string = .{
                    .kind = builder.string("frame-pointer") catch return error.OutOfMemory,
                    .value = builder.string("all") catch return error.OutOfMemory,
                } }, builder);
            }
            if (self.enable_default_platform_diagnostics) {
                try attrs_wip.addFnAttr(.@"noinline", builder);
                try attrs_wip.addFnAttr(.{ .string = .{
                    .kind = builder.string("disable-tail-calls") catch return error.OutOfMemory,
                    .value = builder.string("true") catch return error.OutOfMemory,
                } }, builder);
            }
        }
        func.setAttributes(attrs_wip.finish(builder) catch return error.OutOfMemory, builder);
        try self.proc_registry.put(@intFromEnum(proc_id), func);
    }

    fn addGeneratedFunctionStackProbeAttrs(
        self: *MonoLlvmCodeGen,
        attrs: *LlvmBuilder.FunctionAttributes.Wip,
    ) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        if (self.target.cpu.arch != .x86_64 and self.target.cpu.arch != .aarch64) return;

        if (self.target.os.tag != .windows) {
            try attrs.addFnAttr(.{ .string = .{
                .kind = builder.string("probe-stack") catch return error.OutOfMemory,
                .value = builder.string("inline-asm") catch return error.OutOfMemory,
            } }, builder);
        }
        try attrs.addFnAttr(.{ .string = .{
            .kind = builder.string("stack-probe-size") catch return error.OutOfMemory,
            .value = builder.string("4096") catch return error.OutOfMemory,
        } }, builder);
    }

    /// Whether an object outside this module can name this procedure. Only a
    /// procedure a static-data relocation points at can; entrypoints reach
    /// their procedure through a wrapper compiled into this same module.
    fn procNeedsExternalLinkage(self: *const MonoLlvmCodeGen, proc_id: LirProcSpecId) bool {
        if (self.proc_symbol_mode != .lir_symbol) return false;
        for (self.static_data_procs) |referenced| {
            if (referenced == proc_id) return true;
        }
        return false;
    }

    fn procFunctionName(
        self: *MonoLlvmCodeGen,
        builder: *LlvmBuilder,
        proc_id: LirProcSpecId,
        proc: LirProcSpec,
    ) Error!LlvmBuilder.StrtabString {
        return switch (self.proc_symbol_mode) {
            .local_index => builder.strtabStringFmt("roc_proc_{d}", .{@intFromEnum(proc_id)}) catch return error.OutOfMemory,
            .lir_symbol => blk: {
                const name = std.fmt.allocPrint(self.allocator, "roc__proc_{x}", .{proc.name.raw()}) catch return error.OutOfMemory;
                defer self.allocator.free(name);
                break :blk try self.exportedFunctionName(builder, name);
            },
        };
    }

    fn compileProcBody(self: *MonoLlvmCodeGen, proc_id: LirProcSpecId, proc: LirProcSpec) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const func = self.proc_registry.get(@intFromEnum(proc_id)) orelse return error.CompilationFailed;

        const outer_wip = self.wip;
        const outer_rc_scratch = self.rc_arg_scratch;
        const outer_roc_ops = self.roc_ops_arg;
        const outer_test_context = self.test_context_arg;
        const outer_ret = self.ret_ptr_arg;
        const outer_args = self.args_ptr_arg;
        const outer_capture = self.capture_ptr_arg;
        const outer_reuse = self.reuse_ptr_arg;
        const outer_ret_desc_ptr = self.ret_desc_ptr_arg;
        const outer_runtime_ret_desc = self.current_runtime_ret_desc;
        const outer_ret_layout = self.current_ret_layout;
        const outer_slots = self.local_slots;
        const outer_deferred_str_captures = self.deferred_str_captures;
        const outer_deferred_str_capture_count = self.deferred_str_capture_count;
        const outer_deferred_str_capture_actives = self.deferred_str_capture_actives;
        defer {
            self.deferred_str_capture_actives.deinit(self.allocator);
            self.deferred_str_capture_actives = outer_deferred_str_capture_actives;
            self.wip = outer_wip;
            self.rc_arg_scratch = outer_rc_scratch;
            self.roc_ops_arg = outer_roc_ops;
            self.test_context_arg = outer_test_context;
            self.ret_ptr_arg = outer_ret;
            self.args_ptr_arg = outer_args;
            self.capture_ptr_arg = outer_capture;
            self.reuse_ptr_arg = outer_reuse;
            self.ret_desc_ptr_arg = outer_ret_desc_ptr;
            self.current_runtime_ret_desc = outer_runtime_ret_desc;
            self.current_ret_layout = outer_ret_layout;
            self.local_slots = outer_slots;
            self.deferred_str_captures = outer_deferred_str_captures;
            self.deferred_str_capture_count = outer_deferred_str_capture_count;
        }

        self.join_points.clearRetainingCapacity();
        self.compiled_joins.clearRetainingCapacity();
        self.stmt_incoming_counts.clearRetainingCapacity();
        self.stmt_entry_blocks.clearRetainingCapacity();
        self.loop_continue_blocks.clearRetainingCapacity();
        self.loop_break_blocks.clearRetainingCapacity();
        self.debug_inline_subprograms.clearRetainingCapacity();
        self.debug_inline_callsites.clearRetainingCapacity();

        const outer_subprogram = self.current_subprogram;
        const outer_debug_file = self.current_debug_file;
        defer {
            self.current_subprogram = outer_subprogram;
            self.current_debug_file = outer_debug_file;
        }
        self.current_subprogram = .none;
        self.current_debug_file = SourceLoc.no_file;
        if (!builder.strip) {
            const proc_loc = self.store.procLoc(proc_id);
            const file = try self.debugFileFor(builder, proc_loc.file);
            const name_str = if (self.enable_default_platform_diagnostics)
                try self.procDebugName(builder, proc_id, proc)
            else
                try self.procSymbolDebugName(builder, proc_id, proc);
            const linkage_name_str = if (self.enable_default_platform_diagnostics)
                try self.procSymbolDebugName(builder, proc_id, proc)
            else
                name_str;
            const subprogram = builder.debugSubprogram(
                file,
                name_str,
                linkage_name_str,
                proc_loc.line,
                proc_loc.line,
                builder.debugSubroutineType(null) catch return error.OutOfMemory,
                .{
                    .di_flags = .{},
                    .sp_flags = .{
                        .Definition = true,
                        .LocalToUnit = self.proc_symbol_mode != .lir_symbol,
                    },
                },
                self.debug_compile_unit.unwrap().?,
            ) catch return error.OutOfMemory;
            func.setSubprogram(subprogram, builder);
            self.current_subprogram = subprogram.toOptional();
            self.current_debug_file = proc_loc.file;
        }

        var wip = LlvmBuilder.WipFunction.init(builder, .{ .function = func, .strip = builder.strip }) catch return error.OutOfMemory;
        defer wip.deinit();
        self.wip = &wip;
        self.rc_arg_scratch = null;

        const entry = wip.block(0, "entry") catch return error.OutOfMemory;
        wip.cursor = .{ .block = entry };

        if (proc.abi != .erased_callable and self.host_call_mode == .extern_symbols) {
            // No RocOps parameter under the symbol ABI. Builtins helper
            // signatures still carry an ops slot, which their extern flavor
            // ignores; feed those calls a null constant.
            const ptr_ty = builder.ptrType(.default) catch return error.OutOfMemory;
            self.roc_ops_arg = builder.nullValue(ptr_ty) catch return error.OutOfMemory;
            self.test_context_arg = null;
            self.ret_ptr_arg = wip.arg(0);
            self.args_ptr_arg = wip.arg(1);
            self.capture_ptr_arg = null;
            self.reuse_ptr_arg = null;
            self.ret_desc_ptr_arg = if (proc.runtime_ret_desc != null) wip.arg(2) else null;
        } else {
            self.roc_ops_arg = wip.arg(0);
            if (proc.abi == .erased_callable) {
                if (self.host_call_mode == .vtable) {
                    self.test_context_arg = wip.arg(1);
                    self.ret_ptr_arg = wip.arg(2);
                    self.args_ptr_arg = wip.arg(3);
                    self.capture_ptr_arg = wip.arg(4);
                    self.reuse_ptr_arg = wip.arg(5);
                    self.ret_desc_ptr_arg = wip.arg(6);
                } else {
                    self.test_context_arg = null;
                    self.ret_ptr_arg = wip.arg(1);
                    self.args_ptr_arg = wip.arg(2);
                    self.capture_ptr_arg = wip.arg(3);
                    self.reuse_ptr_arg = wip.arg(4);
                    self.ret_desc_ptr_arg = wip.arg(5);
                }
            } else {
                self.test_context_arg = wip.arg(1);
                self.ret_ptr_arg = wip.arg(2);
                self.args_ptr_arg = wip.arg(3);
                self.capture_ptr_arg = null;
                self.reuse_ptr_arg = null;
                self.ret_desc_ptr_arg = if (proc.runtime_ret_desc != null) wip.arg(4) else null;
            }
        }
        self.current_runtime_ret_desc = proc.runtime_ret_desc;
        self.current_ret_layout = proc.ret_layout;

        self.local_slots = try self.allocator.alloc(LocalSlot, self.store.localCount());
        defer self.allocator.free(self.local_slots);
        self.deferred_str_captures = try self.allocator.alloc(?DeferredStrCapture, self.store.localCount());
        defer self.allocator.free(self.deferred_str_captures);
        self.deferred_str_capture_actives = .empty;
        @memset(self.deferred_str_captures, null);
        try self.allocProcLocalSlots(proc);
        try self.unpackProcArgs(proc);
        if (proc.boxy_runtime_entry) try self.emitBoxyRuntimeInit();
        if (!builder.strip and self.emit_local_debug_info) {
            try self.declareFrameLocals(proc, self.store.procLoc(proc_id).line);
        }

        if (proc.hosted) |hosted| {
            try self.emitHostedProcBody(hosted, proc);
        } else {
            const body = proc.body orelse return error.CompilationFailed;
            try self.collectStmtIncomingCounts(body);
            const compiled_direct_tce_loop = try self.compileDirectEntryTceLoop(proc, body);
            if (!compiled_direct_tce_loop) {
                try self.compileStmt(body);
            }
            if (!self.currentBlockHasTerminator()) {
                _ = wip.retVoid() catch return error.OutOfMemory;
            }
        }

        try self.finishCurrentWipFunction();
    }

    /// Symbol-ABI entrypoint wrapper: exported under the platform's provides
    /// symbol with the entrypoint's natural C ABI. The wrapper marshals its
    /// C-ABI parameters into the internal argument buffer, calls the entry
    /// proc with a null RocOps (compiled code reaches the host through extern
    /// symbols, never through a context parameter), and returns per the ABI.
    fn generateCAbiEntrypointWrapper(
        self: *MonoLlvmCodeGen,
        symbol_name: []const u8,
        entry_proc: ?LirProcSpecId,
        arg_layouts: []const layout.Idx,
        ret_layout: layout.Idx,
        shim: ?ShimTarget,
    ) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const proc_fn: ?LlvmBuilder.Function.Index = if (entry_proc) |proc_id|
            self.proc_registry.get(@intFromEnum(proc_id)) orelse return error.CompilationFailed
        else
            null;
        const ptr_ty = builder.ptrType(.default) catch return error.OutOfMemory;

        var arena_state = std.heap.ArenaAllocator.init(self.allocator);
        defer arena_state.deinit();
        const arena = arena_state.allocator();

        const lowered = layout.abi.lower(arena, self.layouts(), self.abiTarget(), arg_layouts, ret_layout, false) catch return error.OutOfMemory;

        var param_types = std.ArrayList(LlvmBuilder.Type).empty;
        defer param_types.deinit(self.allocator);
        var attrs_wip: LlvmBuilder.FunctionAttributes.Wip = .{};
        defer attrs_wip.deinit(builder);

        var ret_ty: LlvmBuilder.Type = .void;
        var ret_registers: ?layout.abi.RegisterPlacement = null;
        var ret_is_indirect = false;
        switch (lowered.ret) {
            .none => {},
            .indirect => {
                const r_ty = try self.memoryLlvmTypeForLayout(builder, ret_layout);
                try attrs_wip.addParamAttr(param_types.items.len, .{ .sret = r_ty }, builder);
                try param_types.append(self.allocator, ptr_ty);
                ret_is_indirect = true;
            },
            .registers => |registers| {
                ret_registers = registers;
                ret_ty = try self.cAbiRegisterCarrierType(builder, registers);
            },
        }

        for (lowered.args, arg_layouts) |placement, arg_layout| {
            switch (placement) {
                .none => {},
                .indirect => {
                    if (self.hostedIndirectArgUsesByval()) {
                        const a_ty = try self.memoryLlvmTypeForLayout(builder, arg_layout);
                        try attrs_wip.addParamAttr(param_types.items.len, .{ .byval = a_ty }, builder);
                    }
                    try param_types.append(self.allocator, ptr_ty);
                },
                .registers => |registers| try self.appendCAbiRegisterParamTypes(builder, &attrs_wip, &param_types, registers),
            }
        }

        const wrapper_ty = builder.fnType(ret_ty, param_types.items, .normal) catch return error.OutOfMemory;
        const wrapper_name = try self.exportedFunctionName(builder, symbol_name);
        const wrapper = builder.addFunction(wrapper_ty, wrapper_name, .default) catch return error.OutOfMemory;
        wrapper.setLinkage(.external, builder);
        try self.addGeneratedFunctionStackProbeAttrs(&attrs_wip);
        wrapper.setAttributes(attrs_wip.finish(builder) catch return error.OutOfMemory, builder);
        self.configureExportCallConv(wrapper, builder);

        const outer_wip = self.wip;
        const outer_rc_scratch = self.rc_arg_scratch;
        const outer_roc_ops = self.roc_ops_arg;
        const outer_test_context = self.test_context_arg;
        defer {
            self.wip = outer_wip;
            self.rc_arg_scratch = outer_rc_scratch;
            self.roc_ops_arg = outer_roc_ops;
            self.test_context_arg = outer_test_context;
        }

        var wip = LlvmBuilder.WipFunction.init(builder, .{ .function = wrapper, .strip = true }) catch return error.OutOfMemory;
        defer wip.deinit();
        self.wip = &wip;
        self.rc_arg_scratch = null;

        const entry = wip.block(0, "entry") catch return error.OutOfMemory;
        wip.cursor = .{ .block = entry };

        const ops_value = if (shim != null) blk: {
            // The interpreter needs a real RocOps; the prelinked shim builds
            // one over the host's extern symbols.
            const get_ops_ty = builder.fnType(ptr_ty, &.{}, .normal) catch return error.OutOfMemory;
            const get_ops = try self.declareExternSymbol(shim_symbols.roc_shim_get_ops, get_ops_ty);
            break :blk wip.call(.normal, .ccc, .none, get_ops_ty, get_ops.toValue(builder), &.{}, "") catch return error.OutOfMemory;
        } else builder.nullValue(ptr_ty) catch return error.OutOfMemory;
        self.roc_ops_arg = ops_value;
        self.test_context_arg = null;

        var param_cursor: u32 = 0;
        const ret_slot = if (ret_is_indirect) blk: {
            const sret_param = wip.arg(param_cursor);
            param_cursor += 1;
            break :blk sret_param;
        } else try self.allocArgBuffer(&.{ret_layout}, false);

        const args_buf = try self.allocArgBuffer(arg_layouts, true);
        const offsets = try self.computeArgOffsets(arg_layouts, true);
        defer self.allocator.free(offsets);

        for (lowered.args, arg_layouts, offsets) |placement, arg_layout, offset| {
            switch (placement) {
                .none => {},
                .indirect => {
                    const src = wip.arg(param_cursor);
                    param_cursor += 1;
                    const size = self.layoutByteSize(arg_layout);
                    if (size != 0) {
                        try self.copyBytes(try self.offsetPtr(args_buf, offset), src, size, self.alignmentForLayout(arg_layout));
                    }
                },
                .registers => |registers| try self.storeCAbiRegisterParam(
                    builder,
                    registers,
                    &param_cursor,
                    try self.offsetPtr(args_buf, offset),
                    arg_layout,
                ),
            }
        }

        try self.emitBoxyRuntimeInit();

        if (shim) |sh| {
            const idx_value = builder.intValue(.i32, sh.entry_index) catch return error.OutOfMemory;
            const usize_ty: LlvmBuilder.Type = if (self.targetWordSize() == 8) .i64 else .i32;
            if (sh.image) |img| {
                const entry_ty = builder.fnType(.void, &.{ .i32, ptr_ty, ptr_ty, ptr_ty, ptr_ty, usize_ty }, .normal) catch return error.OutOfMemory;
                const entry_fn = try self.declareExternSymbol(shim_symbols.roc_entrypoint_from_image, entry_ty);
                const len_value = builder.intValue(usize_ty, img.len) catch return error.OutOfMemory;
                _ = wip.call(.normal, .ccc, .none, entry_ty, entry_fn.toValue(builder), &.{ idx_value, ops_value, ret_slot, args_buf, img.value, len_value }, "") catch return error.OutOfMemory;
            } else {
                const entry_ty = builder.fnType(.void, &.{ .i32, ptr_ty, ptr_ty, ptr_ty }, .normal) catch return error.OutOfMemory;
                const entry_fn = try self.declareExternSymbol(shim_symbols.roc_entrypoint, entry_ty);
                _ = wip.call(.normal, .ccc, .none, entry_ty, entry_fn.toValue(builder), &.{ idx_value, ops_value, ret_slot, args_buf }, "") catch return error.OutOfMemory;
            }
        } else {
            const proc = self.store.getProcSpec(entry_proc.?);
            const runtime_out_desc = if (proc.runtime_ret_desc != null)
                try self.boxyOutDescPtr("entry_runtime_desc")
            else
                null;
            try self.callProcFunctionIndex(proc_fn.?, proc, ret_slot, args_buf, runtime_out_desc, false);
        }

        if (ret_registers) |registers| {
            _ = wip.ret(try self.loadCAbiRegisterResult(builder, registers, ret_slot, ret_layout)) catch return error.OutOfMemory;
        } else {
            _ = wip.retVoid() catch return error.OutOfMemory;
        }

        try self.finishCurrentWipFunction();
    }

    fn generateEntrypointWrapper(
        self: *MonoLlvmCodeGen,
        symbol_name: []const u8,
        entry_proc: LirProcSpecId,
        arg_layouts: []const layout.Idx,
        ret_layout: layout.Idx,
        abi: EntrypointAbi,
    ) Error!void {
        if (self.host_call_mode == .extern_symbols) {
            return self.generateCAbiEntrypointWrapper(symbol_name, entry_proc, arg_layouts, ret_layout, null);
        }
        const builder = self.builder orelse return error.CompilationFailed;
        const proc_fn = self.proc_registry.get(@intFromEnum(entry_proc)) orelse return error.CompilationFailed;
        const ptr_ty = builder.ptrType(.default) catch return error.OutOfMemory;
        const wrapper_ty = switch (abi) {
            .test_runner => builder.fnType(.void, &.{ ptr_ty, ptr_ty, ptr_ty, ptr_ty, ptr_ty }, .normal) catch return error.OutOfMemory,
            .plugin => builder.fnType(.void, &.{ ptr_ty, ptr_ty, ptr_ty, ptr_ty }, .normal) catch return error.OutOfMemory,
        };
        const wrapper_name = try self.exportedFunctionName(builder, symbol_name);
        const wrapper = builder.addFunction(wrapper_ty, wrapper_name, .default) catch return error.OutOfMemory;
        wrapper.setLinkage(.external, builder);
        var attrs_wip: LlvmBuilder.FunctionAttributes.Wip = .{};
        defer attrs_wip.deinit(builder);
        try self.addGeneratedFunctionStackProbeAttrs(&attrs_wip);
        wrapper.setAttributes(attrs_wip.finish(builder) catch return error.OutOfMemory, builder);
        self.configureExportCallConv(wrapper, builder);

        const outer_wip = self.wip;
        const outer_rc_scratch = self.rc_arg_scratch;
        const outer_roc_ops = self.roc_ops_arg;
        const outer_test_context = self.test_context_arg;
        defer {
            self.wip = outer_wip;
            self.rc_arg_scratch = outer_rc_scratch;
            self.roc_ops_arg = outer_roc_ops;
            self.test_context_arg = outer_test_context;
        }

        var wip = LlvmBuilder.WipFunction.init(builder, .{ .function = wrapper, .strip = builder.strip }) catch return error.OutOfMemory;
        defer wip.deinit();
        self.wip = &wip;
        self.rc_arg_scratch = null;

        const entry = wip.block(0, "entry") catch return error.OutOfMemory;
        wip.cursor = .{ .block = entry };

        const roc_ops = wip.arg(0);
        const test_context = switch (abi) {
            .test_runner => wip.arg(1),
            .plugin => builder.nullValue(ptr_ty) catch return error.OutOfMemory,
        };
        const ret_ptr = wip.arg(switch (abi) {
            .test_runner => 2,
            .plugin => 1,
        });
        const args_ptr = wip.arg(switch (abi) {
            .test_runner => 3,
            .plugin => 2,
        });
        const boxy_fn_table = wip.arg(switch (abi) {
            .test_runner => 4,
            .plugin => 3,
        });
        self.roc_ops_arg = roc_ops;
        self.test_context_arg = test_context;
        try self.storePointer(try self.boxyFnTableGlobal(), boxy_fn_table);

        const args_buf = try self.allocArgBuffer(arg_layouts, true);
        try self.copyEntrypointArgsToInternalBuffer(args_ptr, args_buf, arg_layouts);
        try self.emitBoxyRuntimeInit();
        const proc = self.store.getProcSpec(entry_proc);
        const runtime_out_desc = if (proc.runtime_ret_desc != null)
            try self.boxyOutDescPtr("entry_runtime_desc")
        else
            null;
        try self.callProcFunctionIndex(proc_fn, proc, ret_ptr, args_buf, runtime_out_desc, false);
        _ = wip.retVoid() catch return error.OutOfMemory;
        try self.finishCurrentWipFunction();
    }

    fn emitPluginStampFunction(self: *MonoLlvmCodeGen) Error!void {
        const stamp_bytes = self.plugin_stamp_bytes orelse return;
        const builder = self.builder orelse return error.CompilationFailed;
        const ptr_ty = builder.ptrType(.default) catch return error.OutOfMemory;
        const stamp_ty = builder.arrayType(stamp_bytes.len, .i8) catch return error.OutOfMemory;

        const stamp_var = builder.addVariable(
            builder.strtabString("roc_plugin_stamp_v1_bytes") catch return error.OutOfMemory,
            stamp_ty,
            .default,
        ) catch return error.OutOfMemory;
        stamp_var.ptrConst(builder).global.setLinkage(.internal, builder);
        stamp_var.setMutability(.constant, builder);
        stamp_var.setAlignment(LlvmBuilder.Alignment.fromByteUnits(self.plugin_stamp_alignment), builder);
        stamp_var.setInitializer(
            builder.stringConst(builder.string(stamp_bytes) catch return error.OutOfMemory) catch return error.OutOfMemory,
            builder,
        ) catch return error.OutOfMemory;

        const stamp_fn_ty = builder.fnType(ptr_ty, &.{}, .normal) catch return error.OutOfMemory;
        const stamp_fn_name = try self.exportedFunctionName(builder, "roc_plugin_stamp_v1");
        const stamp_fn = builder.addFunction(stamp_fn_ty, stamp_fn_name, .default) catch return error.OutOfMemory;
        stamp_fn.setLinkage(.external, builder);
        self.configureExportCallConv(stamp_fn, builder);

        const outer_wip = self.wip;
        const outer_rc_scratch = self.rc_arg_scratch;
        defer {
            self.wip = outer_wip;
            self.rc_arg_scratch = outer_rc_scratch;
        }

        var wip = LlvmBuilder.WipFunction.init(builder, .{ .function = stamp_fn, .strip = builder.strip }) catch return error.OutOfMemory;
        defer wip.deinit();
        self.wip = &wip;
        self.rc_arg_scratch = null;

        const entry = wip.block(0, "entry") catch return error.OutOfMemory;
        wip.cursor = .{ .block = entry };
        _ = wip.ret(stamp_var.toValue(builder)) catch return error.OutOfMemory;
        try self.finishCurrentWipFunction();
    }

    fn emitBoxyRuntimeInit(self: *MonoLlvmCodeGen) Error!void {
        if (!self.boxy_runtime_used) return;
        const builder = self.builder orelse return error.CompilationFailed;
        if (self.host_call_mode == .extern_symbols) {
            const wip = self.wip orelse return error.CompilationFailed;
            const fn_ty = builder.fnType(.void, &.{try self.ptrType()}, .normal) catch return error.OutOfMemory;
            const init_fn = try self.declareExternSymbol("roc_boxy_init_embedded", fn_ty);
            _ = wip.call(.normal, .ccc, .none, fn_ty, init_fn.toValue(builder), &.{self.rocOps()}, "") catch return error.OutOfMemory;
        }

        var it = self.boxy_dict_thunks.iterator();
        while (it.next()) |entry| {
            const proc_id: LirProcSpecId = @enumFromInt(entry.key_ptr.*);
            const proc = self.store.getProcSpec(proc_id);
            try self.callBoxyVoid(
                "roc_boxy_register_proc",
                &.{ .i32, try self.ptrType(), .i32, .i64, .i1, .i64 },
                &.{
                    try self.boxyInt(.i32, entry.key_ptr.*),
                    entry.value_ptr.*.toValue(builder),
                    try self.boxyInt(.i32, @intFromEnum(self.layouts().runtimeRepresentationLayoutIdx(proc.ret_layout))),
                    try self.boxyInt(.i64, proc.rc_borrowed_params),
                    try self.boxyInt(.i1, @intFromBool(proc.rc_ret_borrowed)),
                    try self.boxyInt(.i64, proc.rc_ret_lenders),
                },
            );
        }
    }

    fn createBuilder(self: *MonoLlvmCodeGen, name: []const u8) Error!LlvmBuilder {
        return LlvmBuilder.init(.{
            .allocator = self.allocator,
            .strip = !self.emit_debug_info,
            .name = name,
            .target = &self.target,
            .triple = self.triple,
            .data_layout = self.data_layout,
        }) catch return error.OutOfMemory;
    }

    fn exportedFunctionName(self: *MonoLlvmCodeGen, builder: *LlvmBuilder, name: []const u8) Error!LlvmBuilder.StrtabString {
        if (self.target.os.tag != .macos) {
            return builder.strtabString(name) catch return error.OutOfMemory;
        }
        var exact_name_sfa = std.heap.stackFallback(128, self.allocator);
        const exact_name_alloc = exact_name_sfa.get();
        const exact_name = try std.fmt.allocPrint(exact_name_alloc, "\x01_{s}", .{name});
        defer exact_name_alloc.free(exact_name);
        return builder.strtabString(exact_name) catch return error.OutOfMemory;
    }

    fn configureExportCallConv(self: *MonoLlvmCodeGen, func: LlvmBuilder.Function.Index, builder: *LlvmBuilder) void {
        if (self.target.os.tag == .windows) {
            func.ptrConst(builder).global.setDllStorageClass(.dllexport, builder);
        }
        if (self.target.cpu.arch != .x86_64) return;
        if (self.target.os.tag == .windows) {
            func.setCallConv(.win64cc, builder);
        } else {
            func.setCallConv(.x86_64_sysvcc, builder);
        }
    }

    fn serializeBuilderToBitcode(self: *MonoLlvmCodeGen, builder: *LlvmBuilder) Error![]const u32 {
        const producer = LlvmBuilder.Producer{
            .name = "Roc statement LLVM CodeGen",
            .version = .{ .major = 1, .minor = 0, .patch = 0 },
        };
        if (comptime build_options.llvm_keep_ir.len != 0) {
            // Render the IR into a buffer and write it through the CoreCtx
            // filesystem abstraction rather than reaching into the cwd directory
            // handle directly, keeping compiler-core decoupled from the OS I/O layer.
            var ir_text: std.Io.Writer.Allocating = .init(self.allocator);
            defer ir_text.deinit();
            builder.print(&ir_text.writer) catch return error.CompilationFailed;
            CoreCtx.writeFileCwd(std.Options.debug_io, build_options.llvm_keep_ir, ir_text.written()) catch return error.CompilationFailed;
        }
        return builder.toBitcode(self.allocator, producer) catch return error.OutOfMemory;
    }

    const ProcArgLayoutScope = enum {
        all,
        explicit,
    };

    const erased_callable_hidden_param_count = 2;

    fn explicitProcParamCount(abi: lir.LIR.ProcAbi, param_count: usize) Error!usize {
        if (abi != .erased_callable) return param_count;
        if (param_count < erased_callable_hidden_param_count) return error.CompilationFailed;
        return param_count - erased_callable_hidden_param_count;
    }

    fn procArgLayouts(self: *MonoLlvmCodeGen, proc: LirProcSpec, scope: ProcArgLayoutScope) Error![]layout.Idx {
        const params = self.store.getLocalSpan(proc.args);
        const count = switch (scope) {
            .all => params.len,
            .explicit => try explicitProcParamCount(proc.abi, params.len),
        };
        const result = try self.allocator.alloc(layout.Idx, count);
        for (0..count) |i| {
            const local = GuardedList.at(params, i);
            const layout_slot = &result[i];
            layout_slot.* = self.store.getLocal(local).layout_idx;
        }
        return result;
    }

    fn allocProcLocalSlots(self: *MonoLlvmCodeGen, proc: LirProcSpec) Error!void {
        const unallocated = LocalSlot{
            .ptr = undefined,
            .layout_idx = .zst,
            .size = 0,
            .alignment = LlvmBuilder.Alignment.fromByteUnits(1),
            .allocated = false,
        };
        for (self.local_slots) |*local_slot| {
            local_slot.* = unallocated;
        }

        const proc_args = self.store.getLocalSpan(proc.args);
        for (0..proc_args.len) |i| {
            const local_id = GuardedList.at(proc_args, i);
            try self.allocProcLocalSlot(local_id);
        }
        const frame_locals = self.store.getLocalSpan(proc.frame_locals);
        for (0..frame_locals.len) |i| {
            const local_id = GuardedList.at(frame_locals, i);
            try self.allocProcLocalSlot(local_id);
        }
    }

    fn allocProcLocalSlot(self: *MonoLlvmCodeGen, local_id: LocalId) Error!void {
        const local_slot = &self.local_slots[@intFromEnum(local_id)];
        if (local_slot.allocated) return;

        const local = self.store.getLocal(local_id);
        const sa = self.sizeAlignOf(local.layout_idx);
        const alignment = self.llvmAlignment(sa.alignment);
        const ptr = try self.allocEntryBlockSlot(.i8, @max(sa.size, 1), alignment, "local");
        local_slot.* = .{
            .ptr = ptr,
            .layout_idx = local.layout_idx,
            .size = sa.size,
            .alignment = alignment,
            .allocated = true,
        };
    }

    /// Reserve fixed-size storage for one procedure activation.
    ///
    /// LLVM releases `alloca` storage only when the containing function
    /// returns. Emitting one at the current cursor is therefore incorrect when
    /// codegen is visiting a cyclic block: each trip around the cycle would
    /// grow the native stack. Keep every fixed-size backend slot in the entry
    /// block so all control-flow paths reuse the same frame storage.
    fn allocEntryBlockSlot(
        self: *MonoLlvmCodeGen,
        ty: LlvmBuilder.Type,
        element_count: u32,
        alignment: LlvmBuilder.Alignment,
        name: []const u8,
    ) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;

        var resume_cursor = wip.cursor;
        const resume_debug_location = wip.debug_location;
        defer {
            wip.cursor = resume_cursor;
            wip.debug_location = resume_debug_location;
        }

        const entry_block: LlvmBuilder.Function.Block.Index = .entry;
        wip.cursor = .{ .block = entry_block };
        wip.debug_location = .no_location;
        const allocated = wip.alloca(
            .normal,
            ty,
            builder.intValue(.i32, element_count) catch return error.OutOfMemory,
            alignment,
            .default,
            name,
        ) catch return error.OutOfMemory;

        // Inserting at instruction zero shifts an active entry-block cursor.
        if (resume_cursor.block == entry_block) resume_cursor.instruction += 1;
        return allocated;
    }

    fn unpackProcArgs(self: *MonoLlvmCodeGen, proc: LirProcSpec) Error!void {
        const params = self.store.getLocalSpan(proc.args);
        const explicit_count = try explicitProcParamCount(proc.abi, params.len);
        const args_ptr = self.args_ptr_arg orelse return error.CompilationFailed;
        if (proc.abi == .erased_callable) {
            const plan = self.store.getErasedCallArgsPlan(proc.erased_call_args orelse return error.CompilationFailed);
            const offsets = self.store.getErasedCallArgOffsets(plan);
            if (offsets.len != explicit_count) return error.CompilationFailed;
            try self.unpackProcArgsAtOffsets(params, explicit_count, args_ptr, offsets);
        } else {
            const arg_layouts = try self.procArgLayouts(proc, .explicit);
            defer self.allocator.free(arg_layouts);
            const offsets = try self.computeArgOffsets(arg_layouts, true);
            defer self.allocator.free(offsets);
            try self.unpackProcArgsAtOffsets(params, explicit_count, args_ptr, offsets);
        }

        if (proc.abi != .erased_callable) return;

        const capture_param = proc.erased_capture_arg orelse
            llvmInvariantFmt("erased callable adapter has no hidden capture arg", .{});
        if (GuardedList.at(params, explicit_count) != capture_param) {
            llvmInvariantFmt("erased callable capture arg was not the first hidden parameter", .{});
        }
        const capture_ptr = self.capture_ptr_arg orelse return error.CompilationFailed;
        try self.storePointer(self.slot(capture_param).ptr, capture_ptr);

        const reuse_param = GuardedList.at(params, explicit_count + 1);
        const reuse_ptr = self.reuse_ptr_arg orelse return error.CompilationFailed;
        try self.storePointer(self.slot(reuse_param).ptr, reuse_ptr);

        const desc_start: usize = proc.erased_arg_desc_params.start;
        const desc_end = desc_start + proc.erased_arg_desc_params.len;
        if (desc_end > self.erased_arg_desc_params.len) {
            llvmInvariantFmt(
                "erased descriptor-param span [{d}, {d}) exceeded table length {d}",
                .{ desc_start, desc_end, self.erased_arg_desc_params.len },
            );
        }
        const desc_params = self.erased_arg_desc_params[desc_start..desc_end];
        for (desc_params, 0..) |param, param_index| {
            const desc = if (self.erasedArgDescOffsetForKey(proc.erased_arg_desc_offsets, param.key)) |offset|
                try self.loadPointer(try self.offsetPtr(capture_ptr, offset))
            else blk: {
                if (param.source_nested_index == std.math.maxInt(u16)) {
                    llvmInvariantFmt("exact erased descriptor parameter had no capture offset", .{});
                }
                var parent_local: ?LocalId = null;
                for (desc_params[0..param_index]) |candidate| {
                    if (candidate.key.arg_index == param.key.arg_index and
                        candidate.key.descriptor_index == param.source_descriptor_index)
                    {
                        parent_local = candidate.local;
                        break;
                    }
                }
                const parent = parent_local orelse
                    llvmInvariantFmt("projected erased descriptor had no preceding parent parameter", .{});
                break :blk try self.callBoxy(
                    "roc_boxy_nested_desc",
                    try self.ptrType(),
                    &.{ try self.ptrType(), .i32 },
                    &.{
                        try self.loadPointer(self.slot(parent).ptr),
                        try self.boxyInt(.i32, param.source_nested_index),
                    },
                );
            };
            try self.storePointer(self.slot(param.local).ptr, desc);
        }
    }

    fn unpackProcArgsAtOffsets(
        self: *MonoLlvmCodeGen,
        params: anytype,
        explicit_count: usize,
        args_ptr: LlvmBuilder.Value,
        offsets: anytype,
    ) Error!void {
        for (0..explicit_count) |i| {
            const param = GuardedList.at(params, i);
            const param_slot = self.slot(param);
            if (param_slot.size == 0) continue;
            const src = try self.offsetPtr(args_ptr, GuardedList.at(offsets, i));
            try self.copyBytes(param_slot.ptr, src, param_slot.size, param_slot.alignment);
        }
    }

    fn erasedArgDescOffsetForKey(
        self: *const MonoLlvmCodeGen,
        span: lir.LIR.BoxySpan,
        key: lir.LIR.ErasedArgDescKey,
    ) ?u32 {
        const start: usize = span.start;
        const end = start + span.len;
        if (end > self.erased_arg_desc_offsets.len) {
            llvmInvariantFmt(
                "erased descriptor-offset span [{d}, {d}) exceeded table length {d}",
                .{ start, end, self.erased_arg_desc_offsets.len },
            );
        }
        for (self.erased_arg_desc_offsets[start..end]) |entry| {
            if (std.meta.eql(entry.key, key)) return entry.offset;
        }
        return null;
    }

    fn emitHostedProcBody(self: *MonoLlvmCodeGen, hosted: lir.LIR.HostedProc, proc: LirProcSpec) Error!void {
        const params = self.store.getLocalSpan(proc.args);
        const arg_layouts = try self.procArgLayouts(proc, .all);
        defer self.allocator.free(arg_layouts);
        const arg_ptrs = try self.allocator.alloc(LlvmBuilder.Value, params.len);
        defer self.allocator.free(arg_ptrs);
        for (0..params.len) |i| {
            const param = GuardedList.at(params, i);
            arg_ptrs[i] = self.slot(param).ptr;
        }
        const ret_ptr = self.ret_ptr_arg orelse return error.CompilationFailed;
        try self.emitHostedCallCAbi(hosted, arg_ptrs, arg_layouts, ret_ptr, proc.ret_layout);
        const wip = self.wip orelse return error.CompilationFailed;
        _ = wip.retVoid() catch return error.OutOfMemory;
    }

    /// Heap-backed glue carried across the children of one `switch_stmt` while
    /// the explicit work stack drives statement emission. The branch case blocks
    /// were already allocated and the LLVM switch instruction already finished;
    /// these continuations only set the cursor and queue each branch body.
    const SwitchState = struct {
        branches: lir.LirStore.StoreSpanBorrow(lir.CFSwitchBranch, "cf_switch_branches"),
        branch_blocks: []LlvmBuilder.Function.Block.Index,
        default_block: LlvmBuilder.Function.Block.Index,
        default_branch: CFStmtId,
        default_is_cold: bool,
    };

    /// A preserve_most wrapper around one builtin, so a call on a cold path
    /// clobbers almost no registers in its (hot-loop) caller.
    const ColdShim = struct {
        shim: LlvmBuilder.Function.Index,
        target: LlvmBuilder.Function.Index,
        fn_ty: LlvmBuilder.Type,
        /// The wrapped builtin's own signature. Differs from `fn_ty` for
        /// shims that adapt an out-parameter builtin to a by-value return.
        target_fn_ty: LlvmBuilder.Type,
        /// The wrapped builtin writes a three-word list or string through its
        /// first parameter; the shim drops that parameter and returns the
        /// words by value, so the caller's result slot never escapes.
        by_value_out: bool,
        compiled: bool,
    };

    const InitializedPayloadSwitchState = struct {
        initialized_block: LlvmBuilder.Function.Block.Index,
        uninitialized_block: LlvmBuilder.Function.Block.Index,
        initialized_branch: CFStmtId,
        uninitialized_branch: CFStmtId,
    };

    /// Heap-backed glue carried across the children of one `join` statement: the
    /// remainder subtree and (the first time the join is seen) the join body.
    const JoinState = struct {
        key: u32,
        join_block: LlvmBuilder.Function.Block.Index,
        after_block: LlvmBuilder.Function.Block.Index,
        body: CFStmtId,
    };

    const StrMatchBody = struct {
        block: LlvmBuilder.Function.Block.Index,
        stmt: CFStmtId,
        captures: []const DeferredStrCaptureBinding = &.{},
    };

    /// Drives statement-LIR emission with an explicit heap-backed work stack so
    /// arbitrarily deep statement graphs cannot overflow the native stack. A
    /// `.node` item processes one statement; the other variants reproduce the
    /// exact post-children glue of `switch_stmt` and `join` that recursion
    /// previously interleaved. Continuations are pushed before their child so
    /// the child's whole subtree is emitted first, preserving emission order.
    const StmtWork = union(enum) {
        node: CFStmtId,
        switch_branch: struct { state: *SwitchState, index: u32 },
        switch_default: *SwitchState,
        switch_free: *SwitchState,
        cold_region_end,
        initialized_payload_branch: struct { state: *InitializedPayloadSwitchState, initialized: bool },
        initialized_payload_free: *InitializedPayloadSwitchState,
        join_after_remainder: *JoinState,
        join_after_body: *JoinState,
        str_match_body: StrMatchBody,
        boxy_tag_body: struct { block: LlvmBuilder.Function.Block.Index, stmt: CFStmtId },
    };

    fn compileStmt(self: *MonoLlvmCodeGen, stmt_id: CFStmtId) Error!void {
        var sfa = std.heap.stackFallback(64 * @sizeOf(StmtWork), self.allocator);
        const wa = sfa.get();
        var work = std.ArrayList(StmtWork).empty;
        defer work.deinit(wa);
        try work.append(wa, .{ .node = stmt_id });
        while (work.pop()) |item| {
            switch (item) {
                .node => |node_id| try self.compileStmtNode(node_id, wa, &work),
                .switch_branch => |sb| {
                    const wip = self.wip orelse return error.CompilationFailed;
                    wip.cursor = .{ .block = sb.state.branch_blocks[sb.index] };
                    if (sb.index + 1 < sb.state.branch_blocks.len) {
                        try work.append(wa, .{ .switch_branch = .{ .state = sb.state, .index = sb.index + 1 } });
                    } else {
                        try work.append(wa, .{ .switch_default = sb.state });
                    }
                    const branch = GuardedList.at(sb.state.branches, sb.index);
                    try work.append(wa, .{ .node = branch.body });
                },
                .switch_default => |state| {
                    const wip = self.wip orelse return error.CompilationFailed;
                    wip.cursor = .{ .block = state.default_block };
                    try work.append(wa, .{ .switch_free = state });
                    if (state.default_is_cold) {
                        // The default subtree is emitted before this marker
                        // pops, bracketing the region exactly: a jump out to a
                        // join ends the arm, and the join's own body is queued
                        // by its `join_after_remainder`, which sits below the
                        // marker on the stack.
                        self.cold_depth += 1;
                        try work.append(wa, .cold_region_end);
                    }
                    try work.append(wa, .{ .node = state.default_branch });
                },
                .cold_region_end => self.cold_depth -= 1,
                .switch_free => |state| {
                    self.allocator.free(state.branch_blocks);
                    self.allocator.destroy(state);
                },
                .initialized_payload_branch => |branch| {
                    const wip = self.wip orelse return error.CompilationFailed;
                    if (branch.initialized) {
                        wip.cursor = .{ .block = branch.state.initialized_block };
                        try work.append(wa, .{ .initialized_payload_branch = .{ .state = branch.state, .initialized = false } });
                        try work.append(wa, .{ .node = branch.state.initialized_branch });
                    } else {
                        wip.cursor = .{ .block = branch.state.uninitialized_block };
                        try work.append(wa, .{ .initialized_payload_free = branch.state });
                        try work.append(wa, .{ .node = branch.state.uninitialized_branch });
                    }
                },
                .initialized_payload_free => |state| {
                    self.allocator.destroy(state);
                },
                .join_after_remainder => |state| {
                    const wip = self.wip orelse return error.CompilationFailed;
                    if (!self.currentBlockHasTerminator()) _ = wip.br(state.after_block) catch return error.CompilationFailed;
                    if (!self.compiled_joins.contains(state.key)) {
                        try self.compiled_joins.put(state.key, {});
                        wip.cursor = .{ .block = state.join_block };
                        try work.append(wa, .{ .join_after_body = state });
                        try work.append(wa, .{ .node = state.body });
                    } else {
                        wip.cursor = .{ .block = state.after_block };
                        self.allocator.destroy(state);
                    }
                },
                .join_after_body => |state| {
                    const wip = self.wip orelse return error.CompilationFailed;
                    if (!self.currentBlockHasTerminator()) _ = wip.br(state.after_block) catch return error.CompilationFailed;
                    wip.cursor = .{ .block = state.after_block };
                    self.allocator.destroy(state);
                },
                .str_match_body => |body| {
                    const wip = self.wip orelse return error.CompilationFailed;
                    wip.cursor = .{ .block = body.block };
                    self.clearDeferredStrCaptures();
                    for (body.captures) |capture| {
                        try self.installDeferredStrCapture(capture.local, capture.capture);
                    }
                    if (body.captures.len > 0) {
                        self.allocator.free(body.captures);
                    }
                    try work.append(wa, .{ .node = body.stmt });
                },
                .boxy_tag_body => |body| {
                    const wip = self.wip orelse return error.CompilationFailed;
                    wip.cursor = .{ .block = body.block };
                    try work.append(wa, .{ .node = body.stmt });
                },
            }
        }
    }

    fn collectStmtIncomingCounts(self: *MonoLlvmCodeGen, entry: CFStmtId) Error!void {
        self.stmt_incoming_counts.clearRetainingCapacity();
        self.stmt_entry_blocks.clearRetainingCapacity();

        var visited = std.AutoHashMap(u32, void).init(self.allocator);
        defer visited.deinit();

        var stack = std.ArrayList(CFStmtId).empty;
        defer stack.deinit(self.allocator);
        try self.noteStmtIncoming(&stack, entry);

        while (stack.pop()) |stmt_id| {
            const key = @intFromEnum(stmt_id);
            if (visited.contains(key)) continue;
            try visited.put(key, {});

            switch (self.store.getCFStmt(stmt_id)) {
                inline .assign_ref,
                .assign_literal,
                .init_uninitialized,
                .assign_call,
                .assign_call_erased,
                .assign_packed_erased_fn,
                .assign_boxy_desc_ref,
                .assign_boxy_dict_ref,
                .assign_boxy_box,
                .assign_boxy_reuse_box,
                .assign_boxy_unbox,
                .assign_boxy_adapt,
                .assign_boxy_inspect,
                .assign_boxy_eq,
                .assign_boxy_tag,
                .assign_boxy_tag_payload,
                .assign_call_dict,
                .assign_low_level,
                .assign_list,
                .assign_struct,
                .assign_tag,
                .store_struct,
                .store_tag,
                .set_local,
                .debug,
                .expect,
                .comptime_branch_taken,
                .incref,
                .decref,
                .decref_if_initialized,
                .free,
                => |stmt| try self.noteStmtIncoming(&stack, stmt.next),
                .switch_stmt => |switch_stmt| {
                    const branches = self.store.getCFSwitchBranches(switch_stmt.branches);
                    for (0..branches.len) |i| {
                        const branch = GuardedList.at(branches, i);
                        try self.noteStmtIncoming(&stack, branch.body);
                    }
                    try self.noteStmtIncoming(&stack, switch_stmt.default_branch);
                    if (switch_stmt.continuation) |continuation| {
                        try self.noteStmtIncoming(&stack, continuation);
                    }
                },
                .switch_initialized_payload => |switch_stmt| {
                    try self.noteStmtIncoming(&stack, switch_stmt.initialized_branch);
                    try self.noteStmtIncoming(&stack, switch_stmt.uninitialized_branch);
                },
                .str_match => |str_match| {
                    try self.noteStmtIncoming(&stack, str_match.on_match);
                    try self.noteStmtIncoming(&stack, str_match.on_miss);
                },
                .boxy_tag_match => |tag_match| {
                    try self.noteStmtIncoming(&stack, tag_match.on_match);
                    try self.noteStmtIncoming(&stack, tag_match.on_miss);
                },
                .str_match_set => |str_match_set| {
                    const arms = self.store.getStrMatchArms(str_match_set.arms);
                    for (0..arms.len) |i| {
                        const arm = GuardedList.at(arms, i);
                        try self.noteStmtIncoming(&stack, arm.on_match);
                    }
                    try self.noteStmtIncoming(&stack, str_match_set.on_miss);
                },
                .join => |join_stmt| {
                    try self.noteStmtIncoming(&stack, join_stmt.remainder);
                    try self.noteStmtIncoming(&stack, join_stmt.body);
                },
                .runtime_error,
                .comptime_exhaustiveness_failed,
                .loop_continue,
                .loop_break,
                .jump,
                .ret,
                .crash,
                .expect_err,
                => {},
            }
        }
    }

    fn noteStmtIncoming(self: *MonoLlvmCodeGen, stack: *std.ArrayList(CFStmtId), stmt_id: CFStmtId) Error!void {
        const key = @intFromEnum(stmt_id);
        const gop = try self.stmt_incoming_counts.getOrPut(key);
        if (gop.found_existing) {
            gop.value_ptr.* += 1;
        } else {
            gop.value_ptr.* = 1;
        }
        try stack.append(self.allocator, stmt_id);
    }

    fn enterSharedStmtBlock(self: *MonoLlvmCodeGen, stmt_id: CFStmtId) Error!bool {
        const key = @intFromEnum(stmt_id);
        const count = self.stmt_incoming_counts.get(key) orelse 0;
        if (count <= 1) return false;

        const wip = self.wip orelse return error.CompilationFailed;
        if (self.stmt_entry_blocks.get(key)) |block| {
            if (wip.cursor.block != block) {
                _ = wip.br(block) catch return error.OutOfMemory;
            }
            return true;
        }

        const block = wip.block(0, "shared_stmt") catch return error.OutOfMemory;
        try self.stmt_entry_blocks.put(key, block);
        _ = wip.br(block) catch return error.OutOfMemory;
        wip.cursor = .{ .block = block };
        return false;
    }

    /// TCE installs the proc body as `join J { remainder: <entry>, body: old_body }`.
    /// That shape does not need the generic join continuation block: the
    /// remainder is the run-once entry path that branches into the loop body,
    /// and recursive sites jump back there after their explicit
    /// `initialize_join_param` writes. The entry path is a bare `jump J` for a
    /// plain TCE loop; when `scalarize_joins` splits a struct-typed join
    /// parameter (such as a closure's capture record) it seeds the per-field
    /// parameters on the remainder before that jump, so the entry path is a
    /// statement chain ending in `jump J` rather than a single jump.
    fn compileDirectEntryTceLoop(self: *MonoLlvmCodeGen, proc: LirProcSpec, stmt_id: CFStmtId) Error!bool {
        if (proc.tail_transform != .tce) return false;

        const stmt = self.store.getCFStmt(stmt_id);
        if (stmt != .join) return error.CompilationFailed;
        const join_stmt = stmt.join;

        const wip = self.wip orelse return error.CompilationFailed;
        const key = @intFromEnum(join_stmt.id);
        const loop_block = wip.block(0, "tce_loop") catch return error.OutOfMemory;
        try self.join_points.put(key, .{ .block = loop_block, .params = join_stmt.params, .body = join_stmt.body });

        // Emit the run-once entry path, then the loop body. The remainder's
        // terminal `jump J` branches into `loop_block` through `emitJump`, after
        // any seeded join parameters have been initialized in the entry block.
        try self.compileStmt(join_stmt.remainder);
        wip.cursor = .{ .block = loop_block };
        try self.compileStmt(join_stmt.body);
        return true;
    }

    /// Processes a single statement node, queueing successors and nested-body
    /// continuations onto `work` rather than recursing.
    /// Sets the WIP function's ambient debug location from a statement's LIR
    /// source location. Inlined statements use their virtual source procedure
    /// and exact nested call-site chain; compiler-generated statements use line
    /// zero in the physical procedure.
    fn setStmtDebugLocation(self: *MonoLlvmCodeGen, stmt_id: CFStmtId) Error!void {
        const wip = self.wip orelse return;
        if (wip.strip) return;
        if (self.current_subprogram.unwrap() == null) return;
        const loc = self.store.stmtLoc(stmt_id);
        const inline_scope = self.store.stmtInlineScope(stmt_id);
        const scope = if (inline_scope == lir.LIR.InlineScopeId.none)
            self.current_subprogram
        else
            (try self.debugInlineSubprogram(inline_scope)).toOptional();
        const inlined_at = if (inline_scope == lir.LIR.InlineScopeId.none)
            LlvmBuilder.Metadata.Optional.none
        else
            (try self.debugInlineCallsite(inline_scope)).toOptional();
        const has_compatible_location = loc.hasLocation() and
            (inline_scope != lir.LIR.InlineScopeId.none or loc.file == self.current_debug_file);
        wip.debug_location = .{ .location = .{
            .line = if (has_compatible_location) loc.line else 0,
            .column = if (has_compatible_location) loc.column else 0,
            .scope = scope,
            .inlined_at = inlined_at,
        } };
    }

    fn debugInlineSubprogram(self: *MonoLlvmCodeGen, id: lir.LIR.InlineScopeId) Error!LlvmBuilder.Metadata {
        const key = @intFromEnum(id);
        if (self.debug_inline_subprograms.get(key)) |existing| return existing;

        const builder = self.builder orelse return error.CompilationFailed;
        const scope = self.store.inlineScope(id);
        const linkage_name = builder.metadataStringFmt("roc__proc_{x}", .{scope.source_symbol.raw()}) catch return error.OutOfMemory;
        const name = if (scope.source_name.isNone())
            linkage_name
        else
            builder.metadataString(self.store.getString(scope.source_name)) catch return error.OutOfMemory;
        const subprogram = builder.debugSubprogram(
            try self.debugFileFor(builder, scope.source_loc.file),
            name,
            linkage_name,
            scope.source_loc.line,
            scope.source_loc.line,
            builder.debugSubroutineType(null) catch return error.OutOfMemory,
            .{
                .di_flags = .{},
                .sp_flags = .{
                    .Definition = true,
                    .LocalToUnit = true,
                    .Optimized = true,
                },
            },
            self.debug_compile_unit.unwrap().?,
        ) catch return error.OutOfMemory;
        try self.debug_inline_subprograms.put(key, subprogram);
        return subprogram;
    }

    fn debugInlineCallsite(self: *MonoLlvmCodeGen, id: lir.LIR.InlineScopeId) Error!LlvmBuilder.Metadata {
        const key = @intFromEnum(id);
        if (self.debug_inline_callsites.get(key)) |existing| return existing;

        const builder = self.builder orelse return error.CompilationFailed;
        const scope = self.store.inlineScope(id);
        const parent_scope = if (scope.parent == lir.LIR.InlineScopeId.none)
            self.current_subprogram.unwrap().?
        else
            try self.debugInlineSubprogram(scope.parent);
        const parent_callsite = if (scope.parent == lir.LIR.InlineScopeId.none)
            null
        else
            try self.debugInlineCallsite(scope.parent);
        const callsite = builder.debugLocation(
            if (scope.call_site.hasLocation()) scope.call_site.line else 0,
            if (scope.call_site.hasLocation()) scope.call_site.column else 0,
            parent_scope,
            parent_callsite,
        ) catch return error.OutOfMemory;
        try self.debug_inline_callsites.put(key, callsite);
        return callsite;
    }

    fn compileStmtNode(
        self: *MonoLlvmCodeGen,
        stmt_id: CFStmtId,
        wa: Allocator,
        work: *std.ArrayList(StmtWork),
    ) Error!void {
        const stmt = self.store.getCFStmt(stmt_id);
        if (self.currentBlockHasTerminator()) return;
        if (try self.enterSharedStmtBlock(stmt_id)) return;
        const outer_source_stmt = self.current_source_stmt;
        defer self.current_source_stmt = outer_source_stmt;
        self.current_source_stmt = stmt_id;
        try self.setStmtDebugLocation(stmt_id);
        switch (stmt) {
            .assign_ref => |assign| {
                try self.emitAssignRef(assign.target, assign.op);
                try work.append(wa, .{ .node = assign.next });
            },
            .assign_literal => |assign| {
                try self.emitLiteral(assign.target, assign.value);
                try work.append(wa, .{ .node = assign.next });
            },
            .init_uninitialized => |uninit| {
                try work.append(wa, .{ .node = uninit.next });
            },
            .assign_call => |assign| {
                try self.emitDirectCall(assign.target, assign.proc, assign.args, assign.out_desc, assign.is_cold);
                try work.append(wa, .{ .node = assign.next });
            },
            .assign_call_erased => |assign| {
                try self.emitErasedCall(
                    assign.target,
                    assign.closure,
                    assign.args,
                    assign.arg_layouts,
                    assign.arg_descs,
                    assign.arg_desc_keys,
                    assign.arg_plan,
                    assign.result_desc,
                    assign.out_desc,
                    assign.reuse_closure,
                );
                try work.append(wa, .{ .node = assign.next });
            },
            .assign_packed_erased_fn => |assign| {
                try self.emitPackedErasedFn(
                    assign.target,
                    assign.proc,
                    assign.capture,
                    assign.capture_layout,
                    assign.on_drop,
                    assign.result_desc,
                    assign.reuse,
                    assign.reuse_unique,
                );
                try work.append(wa, .{ .node = assign.next });
            },
            .assign_boxy_desc_ref => |assign| {
                try self.emitBoxyDescRef(assign);
                try work.append(wa, .{ .node = assign.next });
            },
            .assign_boxy_dict_ref => |assign| {
                try self.emitBoxyDictRef(assign);
                try work.append(wa, .{ .node = assign.next });
            },
            .assign_boxy_box => |assign| {
                try self.emitBoxyBox(assign);
                try work.append(wa, .{ .node = assign.next });
            },
            .assign_boxy_reuse_box => |assign| {
                try self.copyLocal(assign.target, assign.source);
                try work.append(wa, .{ .node = assign.next });
            },
            .assign_boxy_unbox => |assign| {
                try self.emitBoxyUnbox(assign);
                try work.append(wa, .{ .node = assign.next });
            },
            .assign_boxy_adapt => |assign| {
                try self.emitBoxyAdapt(assign);
                try work.append(wa, .{ .node = assign.next });
            },
            .assign_boxy_inspect => |assign| {
                try self.emitBoxyInspect(assign);
                try work.append(wa, .{ .node = assign.next });
            },
            .assign_boxy_eq => |assign| {
                try self.emitBoxyEq(assign);
                try work.append(wa, .{ .node = assign.next });
            },
            .assign_boxy_tag => |assign| {
                try self.emitBoxyTag(assign);
                try work.append(wa, .{ .node = assign.next });
            },
            .assign_boxy_tag_payload => |assign| {
                try self.emitBoxyTagPayload(assign);
                try work.append(wa, .{ .node = assign.next });
            },
            .boxy_tag_match => |tag_match| try self.emitBoxyTagMatch(tag_match, wa, work),
            .assign_call_dict => |assign| {
                try self.emitBoxyCallDict(assign);
                try work.append(wa, .{ .node = assign.next });
            },
            .assign_low_level => |assign| {
                try self.emitLowLevel(assign.target, assign.op, assign.args, assign.unique_args, assign.interchangeable);
                try work.append(wa, .{ .node = assign.next });
            },
            .assign_list => |assign| {
                try self.emitListLiteral(assign.target, assign.elems);
                try work.append(wa, .{ .node = assign.next });
            },
            .assign_struct => |assign| {
                try self.emitStructLiteral(assign.target, assign.fields);
                try work.append(wa, .{ .node = assign.next });
            },
            .assign_tag => |assign| {
                try self.emitTagLiteral(assign.target, assign.discriminant, assign.payload);
                try work.append(wa, .{ .node = assign.next });
            },
            .store_struct => |assign| {
                try self.emitStoreStruct(assign.dest, assign.struct_layout, assign.fields);
                try work.append(wa, .{ .node = assign.next });
            },
            .store_tag => |assign| {
                try self.emitStoreTag(assign.dest, assign.tag_layout, assign.discriminant, assign.payload);
                try work.append(wa, .{ .node = assign.next });
            },
            .set_local => |assign| {
                try self.copyLocal(assign.target, assign.value);
                try work.append(wa, .{ .node = assign.next });
            },
            .debug => |debug_stmt| {
                try self.materializeLocalIfDeferred(debug_stmt.message);
                try self.callBuiltinVoid(builtinSymbol(.dbg_str), &.{ try self.ptrType(), try self.ptrType() }, &.{ self.slot(debug_stmt.message).ptr, self.rocOps() });
                try work.append(wa, .{ .node = debug_stmt.next });
            },
            .expect => |expect_stmt| {
                try self.emitExpect(expect_stmt.condition);
                try work.append(wa, .{ .node = expect_stmt.next });
            },
            .runtime_error => {
                try self.emitRuntimeError();
            },
            .comptime_exhaustiveness_failed => {
                try self.emitCrashBytes("compile-time exhaustiveness failure reached runtime code");
            },
            .comptime_branch_taken => |marker| {
                try work.append(wa, .{ .node = marker.next });
            },
            .incref => |inc| {
                try self.emitExplicitRcStmt(inc.rc, .incref, inc.value, inc.count, inc.atomicity);
                try work.append(wa, .{ .node = inc.next });
            },
            .decref => |dec| {
                try self.emitExplicitRcStmt(dec.rc, .decref, dec.value, 1, dec.atomicity);
                try work.append(wa, .{ .node = dec.next });
            },
            .decref_if_initialized => |dec| {
                try self.emitDecrefIfInitialized(dec.cond, dec.cond_mask, dec.value, dec.rc, dec.atomicity);
                try work.append(wa, .{ .node = dec.next });
            },
            .free => |free_stmt| {
                try self.emitExplicitRcStmt(free_stmt.rc, .free, free_stmt.value, 1, free_stmt.atomicity);
                try work.append(wa, .{ .node = free_stmt.next });
            },
            .switch_stmt => |sw| try self.emitSwitch(sw, wa, work),
            .switch_initialized_payload => |sw| try self.emitInitializedPayloadSwitch(sw, wa, work),
            .str_match => |str_match| try self.emitStrMatch(str_match, wa, work),
            .str_match_set => |str_match_set| try self.emitStrMatchSet(str_match_set, wa, work),
            .loop_continue => try self.emitLoopContinue(),
            .loop_break => try self.emitLoopBreak(),
            .join => |join_stmt| try self.emitJoin(join_stmt, wa, work),
            .jump => |jump_stmt| try self.emitJump(jump_stmt),
            .ret => |ret_stmt| try self.emitReturn(ret_stmt.value),
            .crash => |crash_stmt| switch (crash_stmt.msg) {
                .literal => |literal| try self.emitCrashBytes(self.store.getString(literal)),
                .local => |local| try self.emitCrashLocal(local),
            },
            .expect_err => |expect_err_stmt| {
                try self.materializeLocalIfDeferred(expect_err_stmt.message);
                const wip = self.wip orelse return error.CompilationFailed;
                const builder = self.builder orelse return error.CompilationFailed;
                const region_start = builder.intValue(.i32, expect_err_stmt.region.start.offset) catch return error.OutOfMemory;
                const region_end = builder.intValue(.i32, expect_err_stmt.region.end.offset) catch return error.OutOfMemory;

                const context = self.testInvocationContext();
                const flag = builder.intValue(.i32, 1) catch return error.OutOfMemory;
                const align4 = LlvmBuilder.Alignment.fromByteUnits(4);
                _ = wip.store(.normal, flag, context, align4) catch return error.OutOfMemory;
                _ = wip.store(.normal, region_start, try self.offsetPtr(context, 4), align4) catch return error.OutOfMemory;
                _ = wip.store(.normal, region_end, try self.offsetPtr(context, 8), align4) catch return error.OutOfMemory;

                try self.callBuiltinVoid(
                    builtinSymbol(.expect_err_str),
                    &.{ try self.ptrType(), .i32, .i32, try self.ptrType() },
                    &.{
                        self.slot(expect_err_stmt.message).ptr,
                        region_start,
                        region_end,
                        self.rocOps(),
                    },
                );
                // Linux AArch64 eval tests handle crashes by returning to the Zig host.
                // Longjmping through LLVM-generated frames is not reliable on that target.
                if (self.target.cpu.arch == .aarch64 and self.target.os.tag == .linux) {
                    _ = wip.retVoid() catch return error.OutOfMemory;
                } else {
                    _ = wip.@"unreachable"() catch return error.OutOfMemory;
                }
            },
        }
    }

    fn emitAssignRef(self: *MonoLlvmCodeGen, target: LocalId, op: lir.LIR.RefOp) Error!void {
        const target_slot = self.slot(target);
        switch (op) {
            .local => |source| try self.copyLocal(target, source),
            .list_reinterpret => |ref| try self.copyLocal(target, ref.backing_ref),
            .nominal => |ref| try self.copyLocal(target, ref.backing_ref),
            .discriminant => |ref| {
                try self.prepareLocalWrite(target);
                try self.materializeLocalIfDeferred(ref.source);
                const base = try self.resolveTagBase(ref.source);
                const discrim = try self.readTagDiscriminant(base.ptr, base.layout_idx);
                try self.storeIntToLayout(target_slot.ptr, discrim, target_slot.layout_idx);
            },
            .field => |ref| {
                try self.prepareLocalWrite(target);
                try self.materializeLocalIfDeferred(ref.source);
                const base = try self.resolveStructBase(ref.source);
                const base_layout = self.layoutValue(base.layout_idx);
                if (base_layout.tag != .struct_) return error.CompilationFailed;
                const offset = self.layouts().getStructFieldOffsetByOriginalIndex(base_layout.getStruct().idx, ref.field_idx);
                const field_layout = self.layouts().getStructFieldLayoutByOriginalIndex(base_layout.getStruct().idx, ref.field_idx);
                const src = try self.offsetPtr(base.ptr, offset);
                try self.copyBytes(target_slot.ptr, src, self.layoutByteSize(field_layout), self.alignmentForLayout(field_layout));
            },
            .tag_payload => |ref| {
                try self.prepareLocalWrite(target);
                try self.materializeLocalIfDeferred(ref.source);
                const base = try self.resolveTagBase(ref.source);
                const payload_layout = self.tagPayloadLayout(base.layout_idx, ref.tag_discriminant);
                const payload_layout_val = self.layoutValue(payload_layout);
                var src = base.ptr;
                var copy_layout = payload_layout;
                if (payload_layout_val.tag == .struct_) {
                    const offset = self.layouts().getStructFieldOffsetByOriginalIndex(payload_layout_val.getStruct().idx, ref.payload_idx);
                    src = try self.offsetPtr(base.ptr, offset);
                    copy_layout = self.layouts().getStructFieldLayoutByOriginalIndex(payload_layout_val.getStruct().idx, ref.payload_idx);
                }
                try self.copyBytes(target_slot.ptr, src, self.layoutByteSize(copy_layout), self.alignmentForLayout(copy_layout));
            },
            .tag_payload_struct => |ref| {
                try self.prepareLocalWrite(target);
                try self.materializeLocalIfDeferred(ref.source);
                const base = try self.resolveTagBase(ref.source);
                const payload_layout = self.tagPayloadLayout(base.layout_idx, ref.tag_discriminant);
                try self.copyBytes(target_slot.ptr, base.ptr, self.layoutByteSize(payload_layout), self.alignmentForLayout(payload_layout));
            },
        }
    }

    fn emitLiteral(self: *MonoLlvmCodeGen, target: LocalId, value: lir.LIR.LiteralValue) Error!void {
        try self.prepareLocalWrite(target);
        const slot_v = self.slot(target);
        switch (value) {
            .i64_literal => |lit| try self.storeIntLiteral(slot_v.ptr, slot_v.layout_idx, lit.value),
            .i128_literal => |lit| try self.storeI128Literal(slot_v.ptr, slot_v.layout_idx, lit.value),
            .f64_literal => |lit| try self.storeFloatLiteral(slot_v.ptr, .f64, lit),
            .f32_literal => |lit| try self.storeFloatLiteral(slot_v.ptr, .f32, lit),
            .dec_literal => |lit| try self.storeI128Literal(slot_v.ptr, .dec, lit),
            .str_literal => |str_idx| try self.emitStrLiteral(slot_v.ptr, str_idx),
            .boxy_dynamic_num_literal => |lit| try self.emitBoxyDynamicLiteral(target, lit.value, lit.desc, lit.default_layout, false),
            .boxy_dynamic_frac_literal => |lit| try self.emitBoxyDynamicLiteral(target, lit.dec_bits, lit.desc, lit.default_layout, true),
            .static_data => |id| try self.emitStaticDataLiteral(slot_v, id),
            .bytes_literal => |bytes_idx| try self.emitBytesLiteral(slot_v.ptr, bytes_idx),
            .null_ptr => {
                if (slot_v.size > 0) try self.zeroBytes(slot_v.ptr, slot_v.size);
            },
            .proc_ref => |proc_id| {
                const func = self.proc_registry.get(@intFromEnum(proc_id)) orelse return error.CompilationFailed;
                try self.storePointer(slot_v.ptr, func.toValue(self.builder.?));
            },
        }
    }

    fn boxyNullPtr(self: *MonoLlvmCodeGen) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        return builder.nullValue(try self.ptrType()) catch return error.OutOfMemory;
    }

    fn boxyInt(self: *MonoLlvmCodeGen, ty: LlvmBuilder.Type, value: anytype) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        return builder.intValue(ty, value) catch return error.OutOfMemory;
    }

    fn callBoxy(
        self: *MonoLlvmCodeGen,
        name: []const u8,
        ret_type: LlvmBuilder.Type,
        param_types: []const LlvmBuilder.Type,
        args: []const LlvmBuilder.Value,
    ) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        self.boxy_runtime_used = true;
        const fn_ty = builder.fnType(ret_type, param_types, .normal) catch return error.OutOfMemory;
        const fn_ptr = if (self.host_call_mode == .extern_symbols) blk: {
            const func = try self.declareExternSymbol(name, fn_ty);
            break :blk func.toValue(builder);
        } else blk: {
            const boxy_fn = boxyBuiltinFnForSymbol(name) orelse
                llvmInvariantFmt("unknown Boxy runtime symbol {s}", .{name});
            const table = try self.loadPointer(try self.boxyFnTableGlobal());
            const entry = try self.offsetPtr(table, @intFromEnum(boxy_fn) * self.targetWordSize());
            break :blk try self.loadPointer(entry);
        };
        return wip.call(.normal, .ccc, .none, fn_ty, fn_ptr, args, "") catch return error.OutOfMemory;
    }

    fn boxyFnTableGlobal(self: *MonoLlvmCodeGen) Error!LlvmBuilder.Value {
        if (self.boxy_fn_table_global) |global| return global;
        const builder = self.builder orelse return error.CompilationFailed;
        const ptr_ty = try self.ptrType();
        const variable = builder.addVariable(
            builder.strtabString("roc_eval_boxy_fn_table") catch return error.OutOfMemory,
            ptr_ty,
            .default,
        ) catch return error.OutOfMemory;
        variable.ptrConst(builder).global.setLinkage(.internal, builder);
        variable.setInitializer(builder.nullConst(ptr_ty) catch return error.OutOfMemory, builder) catch return error.OutOfMemory;
        const global = variable.toValue(builder);
        self.boxy_fn_table_global = global;
        return global;
    }

    fn boxyBuiltinFnForSymbol(name: []const u8) ?BoxyBuiltinFn {
        inline for (@typeInfo(BoxyBuiltinFn).@"enum".fields) |field| {
            const boxy_fn: BoxyBuiltinFn = @enumFromInt(field.value);
            if (std.mem.eql(u8, name, comptime boxy_fn.symbolName())) return boxy_fn;
        }
        return null;
    }

    fn callBoxyVoid(
        self: *MonoLlvmCodeGen,
        name: []const u8,
        param_types: []const LlvmBuilder.Type,
        args: []const LlvmBuilder.Value,
    ) Error!void {
        _ = try self.callBoxy(name, .void, param_types, args);
    }

    fn boxyValuePtr(self: *MonoLlvmCodeGen, local: LocalId) Error!LlvmBuilder.Value {
        try self.materializeLocalIfDeferred(local);
        return if (self.slot(local).size == 0) try self.boxyNullPtr() else self.slot(local).ptr;
    }

    fn resolveBoxyDesc(self: *MonoLlvmCodeGen, desc: lir.LIR.BoxyDescRef) Error!LlvmBuilder.Value {
        const ptr_ty = try self.ptrType();
        return switch (desc) {
            .static => |desc_id| try self.callBoxy(
                "roc_boxy_static_desc",
                ptr_ty,
                &.{.i32},
                &.{try self.boxyInt(.i32, @intFromEnum(desc_id))},
            ),
            .local => |local| blk: {
                try self.materializeLocalIfDeferred(local);
                break :blk try self.loadPointer(self.slot(local).ptr);
            },
            .dict_method_arg => |projection| try self.callBoxy(
                "roc_boxy_dict_method_arg_desc",
                ptr_ty,
                &.{ ptr_ty, .i32, .i32, .i32 },
                &.{
                    try self.resolveBoxyDict(.{ .local = projection.dict }),
                    try self.boxyInt(.i32, projection.method_slot),
                    try self.boxyInt(.i32, @intFromEnum(projection.method)),
                    try self.boxyInt(.i32, projection.arg_index),
                },
            ),
            .dict_method_hidden => |projection| try self.callBoxy(
                "roc_boxy_dict_method_hidden_desc",
                ptr_ty,
                &.{ ptr_ty, .i32, .i32, .i32, .i32 },
                &.{
                    try self.resolveBoxyDict(.{ .local = projection.dict }),
                    try self.boxyInt(.i32, projection.method_slot),
                    try self.boxyInt(.i32, @intFromEnum(projection.method)),
                    try self.boxyInt(.i32, projection.hidden_index),
                    try self.boxyInt(.i32, @intFromEnum(projection.shape)),
                },
            ),
            .runtime => error.CompilationFailed,
        };
    }

    fn resolveBoxyDict(self: *MonoLlvmCodeGen, dict: lir.LIR.BoxyDictRef) Error!LlvmBuilder.Value {
        const ptr_ty = try self.ptrType();
        return switch (dict) {
            .static => |dict_id| try self.callBoxy(
                "roc_boxy_static_dict",
                ptr_ty,
                &.{.i32},
                &.{try self.boxyInt(.i32, @intFromEnum(dict_id))},
            ),
            .local => |local| blk: {
                try self.materializeLocalIfDeferred(local);
                break :blk try self.loadPointer(self.slot(local).ptr);
            },
        };
    }

    fn storeBoxyOutDesc(self: *MonoLlvmCodeGen, target: LocalId, out_desc_ptr: LlvmBuilder.Value) Error!void {
        const desc_ref = self.store.getLocal(target).boxy_desc orelse return;
        const desc_local = desc_ref.localOrNull() orelse return;
        try self.prepareLocalWrite(desc_local);
        try self.storePointer(self.slot(desc_local).ptr, try self.loadPointer(out_desc_ptr));
    }

    fn emitBoxyDescRef(self: *MonoLlvmCodeGen, assign: anytype) Error!void {
        try self.prepareLocalWrite(assign.target);
        const ptr_ty = try self.ptrType();
        const captures = self.store.getLocalSpan(assign.captures);
        var desc: LlvmBuilder.Value = undefined;

        if (assign.tag_residual_for) |target_ref| {
            if (assign.nested_index != null or assign.box_payload_layout != null or assign.tag_payload != null or assign.tag_ext or captures.len != 0) return error.CompilationFailed;
            desc = try self.callBoxy(
                "roc_boxy_tag_residual_desc",
                ptr_ty,
                &.{ ptr_ty, ptr_ty },
                &.{ try self.resolveBoxyDesc(assign.desc), try self.resolveBoxyDesc(target_ref) },
            );
        } else if (captures.len == 0) {
            desc = try self.resolveBoxyDesc(assign.desc);
        } else {
            const desc_id = switch (assign.desc) {
                .static => |id| id,
                .local, .runtime, .dict_method_arg, .dict_method_hidden => return error.CompilationFailed,
            };
            const wip = self.wip orelse return error.CompilationFailed;
            const ids = try self.allocEntryBlockSlot(.i32, @intCast(captures.len), LlvmBuilder.Alignment.fromByteUnits(4), "boxy_capture_ids");
            const descs = try self.allocEntryBlockSlot(ptr_ty, @intCast(captures.len), self.targetPointerAlignment(), "boxy_capture_descs");
            for (0..captures.len) |i| {
                const capture = GuardedList.at(captures, i);
                const id_ptr = try self.offsetPtr(ids, @intCast(i * 4));
                _ = wip.store(.normal, try self.boxyInt(.i32, @intFromEnum(capture)), id_ptr, LlvmBuilder.Alignment.fromByteUnits(4)) catch return error.OutOfMemory;
                try self.storePointer(try self.offsetPtr(descs, @intCast(i * self.targetWordSize())), try self.resolveBoxyDesc(.{ .local = capture }));
            }
            desc = try self.callBoxy(
                "roc_boxy_desc_copy",
                ptr_ty,
                &.{ .i32, ptr_ty, ptr_ty, self.ptrSizedIntType() },
                &.{
                    try self.boxyInt(.i32, @intFromEnum(desc_id)),
                    ids,
                    descs,
                    try self.boxyInt(self.ptrSizedIntType(), captures.len),
                },
            );
        }

        const projection_count = @intFromBool(assign.nested_index != null) +
            @intFromBool(assign.box_payload_layout != null) +
            @intFromBool(assign.tag_payload != null) + @intFromBool(assign.tag_ext);
        if (projection_count > 1) return error.CompilationFailed;
        if (assign.box_payload_layout) |box_layout| {
            desc = try self.callBoxy(
                "roc_boxy_box_payload_desc",
                ptr_ty,
                &.{ ptr_ty, .i32 },
                &.{ desc, try self.boxyInt(.i32, @intFromEnum(box_layout)) },
            );
        } else if (assign.nested_index) |nested_index| {
            desc = try self.callBoxy(
                "roc_boxy_nested_desc",
                ptr_ty,
                &.{ ptr_ty, .i32 },
                &.{ desc, try self.boxyInt(.i32, nested_index) },
            );
        } else if (assign.tag_payload) |payload| {
            desc = try self.callBoxy(
                "roc_boxy_tag_payload_desc",
                ptr_ty,
                &.{ ptr_ty, .i32, .i32 },
                &.{
                    desc,
                    try self.boxyInt(.i32, @intFromEnum(payload.tag_name)),
                    try self.boxyInt(.i32, payload.payload_index),
                },
            );
        } else if (assign.tag_ext) {
            desc = try self.callBoxy("roc_boxy_tag_ext_desc", ptr_ty, &.{ptr_ty}, &.{desc});
        }
        try self.storePointer(self.slot(assign.target).ptr, desc);
    }

    fn emitBoxyDictRef(self: *MonoLlvmCodeGen, assign: anytype) Error!void {
        try self.prepareLocalWrite(assign.target);
        try self.storePointer(self.slot(assign.target).ptr, try self.resolveBoxyDict(assign.dict));
    }

    fn boxyOutDescPtr(self: *MonoLlvmCodeGen, name: []const u8) Error!LlvmBuilder.Value {
        return try self.allocEntryBlockSlot(try self.ptrType(), 1, self.targetPointerAlignment(), name);
    }

    fn emitBoxyBox(self: *MonoLlvmCodeGen, assign: anytype) Error!void {
        try self.prepareLocalWrite(assign.target);
        const payload_desc_ref = assign.payload_desc orelse return error.CompilationFailed;
        const ptr_ty = try self.ptrType();
        const out_desc = try self.boxyOutDescPtr("boxy_box_desc");
        try self.callBoxyVoid(
            "roc_boxy_box",
            &.{ ptr_ty, ptr_ty, ptr_ty, .i32, ptr_ty, ptr_ty, .i8, .i32 },
            &.{
                try self.boxyValuePtr(assign.target),
                out_desc,
                try self.boxyValuePtr(assign.payload),
                try self.boxyInt(.i32, @intFromEnum(assign.payload_layout)),
                if (assign.source_desc) |desc| try self.resolveBoxyDesc(desc) else try self.boxyNullPtr(),
                try self.resolveBoxyDesc(payload_desc_ref),
                try self.boxyInt(.i8, @intFromEnum(assign.payload_mode)),
                try self.boxyInt(.i32, @intFromEnum(self.localLayout(assign.target))),
            },
        );
        try self.storeBoxyOutDesc(assign.target, out_desc);
    }

    fn emitBoxyUnbox(self: *MonoLlvmCodeGen, assign: anytype) Error!void {
        try self.prepareLocalWrite(assign.target);
        const ptr_ty = try self.ptrType();
        const out_desc = try self.boxyOutDescPtr("boxy_unbox_desc");
        try self.callBoxyVoid(
            "roc_boxy_unbox",
            &.{ ptr_ty, ptr_ty, ptr_ty, .i32, ptr_ty, ptr_ty, .i32, .i8 },
            &.{
                try self.boxyValuePtr(assign.target),
                out_desc,
                try self.boxyValuePtr(assign.source),
                try self.boxyInt(.i32, @intFromEnum(self.localLayout(assign.source))),
                try self.resolveBoxyDesc(assign.source_desc),
                if (assign.target_desc) |desc| try self.resolveBoxyDesc(desc) else try self.boxyNullPtr(),
                try self.boxyInt(.i32, @intFromEnum(assign.target_layout)),
                try self.boxyInt(.i8, @intFromEnum(assign.source_mode)),
            },
        );
        try self.storeBoxyOutDesc(assign.target, out_desc);
    }

    fn emitBoxyAdapt(self: *MonoLlvmCodeGen, assign: anytype) Error!void {
        try self.prepareLocalWrite(assign.target);
        const ptr_ty = try self.ptrType();
        const out_desc = try self.boxyOutDescPtr("boxy_adapt_desc");
        try self.callBoxyVoid(
            "roc_boxy_adapt",
            &.{ ptr_ty, ptr_ty, ptr_ty, ptr_ty, ptr_ty, .i32, .i8 },
            &.{
                try self.boxyValuePtr(assign.target),
                out_desc,
                try self.boxyValuePtr(assign.source),
                if (assign.source_desc) |desc| try self.resolveBoxyDesc(desc) else try self.boxyNullPtr(),
                if (assign.target_desc) |desc| try self.resolveBoxyDesc(desc) else try self.boxyNullPtr(),
                try self.boxyInt(.i32, @intFromEnum(assign.adapter)),
                try self.boxyInt(.i8, @intFromEnum(assign.source_mode)),
            },
        );
        try self.storeBoxyOutDesc(assign.target, out_desc);
    }

    fn emitBoxyInspect(self: *MonoLlvmCodeGen, assign: anytype) Error!void {
        try self.prepareLocalWrite(assign.target);
        const ptr_ty = try self.ptrType();
        try self.callBoxyVoid(
            "roc_boxy_inspect",
            &.{ ptr_ty, ptr_ty, ptr_ty, .i32, ptr_ty },
            &.{
                try self.boxyValuePtr(assign.target),
                if (self.host_call_mode == .vtable) self.testInvocationContext() else try self.boxyNullPtr(),
                try self.boxyValuePtr(assign.source),
                try self.boxyInt(.i32, @intFromEnum(self.localLayout(assign.source))),
                try self.resolveBoxyDesc(assign.source_desc),
            },
        );
    }

    fn emitBoxyEq(self: *MonoLlvmCodeGen, assign: anytype) Error!void {
        try self.prepareLocalWrite(assign.target);
        const ptr_ty = try self.ptrType();
        const result = try self.callBoxy(
            "roc_boxy_eq",
            .i1,
            &.{ ptr_ty, ptr_ty, .i32, ptr_ty },
            &.{
                try self.boxyValuePtr(assign.lhs),
                try self.boxyValuePtr(assign.rhs),
                try self.boxyInt(.i32, @intFromEnum(self.localLayout(assign.lhs))),
                try self.resolveBoxyDesc(assign.source_desc),
            },
        );
        try self.storeBool(self.slot(assign.target).ptr, result);
    }

    fn emitBoxyTag(self: *MonoLlvmCodeGen, assign: anytype) Error!void {
        try self.prepareLocalWrite(assign.target);
        const ptr_ty = try self.ptrType();
        try self.callBoxyVoid(
            "roc_boxy_tag",
            &.{ ptr_ty, ptr_ty, .i32, ptr_ty, .i32, ptr_ty, .i8, .i32 },
            &.{
                try self.boxyValuePtr(assign.target),
                try self.resolveBoxyDesc(assign.target_desc),
                try self.boxyInt(.i32, @intFromEnum(assign.tag_name)),
                if (assign.payload) |payload| try self.boxyValuePtr(payload) else try self.boxyNullPtr(),
                try self.boxyInt(.i32, @intFromEnum(assign.payload_layout)),
                if (assign.payload_desc) |desc| try self.resolveBoxyDesc(desc) else try self.boxyNullPtr(),
                try self.boxyInt(.i8, @intFromEnum(assign.payload_mode)),
                try self.boxyInt(.i32, @intFromEnum(self.localLayout(assign.target))),
            },
        );
    }

    fn emitBoxyTagPayload(self: *MonoLlvmCodeGen, assign: anytype) Error!void {
        try self.prepareLocalWrite(assign.target);
        const ptr_ty = try self.ptrType();
        const out_desc = try self.boxyOutDescPtr("boxy_tag_payload_desc");
        try self.callBoxyVoid(
            "roc_boxy_tag_payload",
            &.{ ptr_ty, ptr_ty, ptr_ty, .i32, ptr_ty, .i32, .i32, .i32, .i8 },
            &.{
                try self.boxyValuePtr(assign.target),
                out_desc,
                try self.boxyValuePtr(assign.source),
                try self.boxyInt(.i32, @intFromEnum(self.localLayout(assign.source))),
                try self.resolveBoxyDesc(assign.source_desc),
                try self.boxyInt(.i32, @intFromEnum(assign.tag_name)),
                try self.boxyInt(.i32, assign.payload_index),
                try self.boxyInt(.i32, @intFromEnum(self.localLayout(assign.target))),
                try self.boxyInt(.i8, @intFromEnum(assign.source_mode)),
            },
        );
        if (assign.target_desc) |desc_local| {
            try self.prepareLocalWrite(desc_local);
            try self.storePointer(self.slot(desc_local).ptr, try self.loadPointer(out_desc));
        }
    }

    fn emitBoxyTagMatch(self: *MonoLlvmCodeGen, tag_match: anytype, wa: Allocator, work: *std.ArrayList(StmtWork)) Error!void {
        const ptr_ty = try self.ptrType();
        const wip = self.wip orelse return error.CompilationFailed;
        const matches = try self.callBoxy(
            "roc_boxy_tag_match",
            .i1,
            &.{ ptr_ty, .i32, ptr_ty, .i32 },
            &.{
                try self.boxyValuePtr(tag_match.source),
                try self.boxyInt(.i32, @intFromEnum(self.localLayout(tag_match.source))),
                try self.resolveBoxyDesc(tag_match.source_desc),
                try self.boxyInt(.i32, @intFromEnum(tag_match.tag_name)),
            },
        );
        const on_match = wip.block(0, "boxy_tag_match") catch return error.OutOfMemory;
        const on_miss = wip.block(0, "boxy_tag_miss") catch return error.OutOfMemory;
        _ = wip.brCond(matches, on_match, on_miss, .none) catch return error.OutOfMemory;
        try work.append(wa, .{ .boxy_tag_body = .{ .block = on_miss, .stmt = tag_match.on_miss } });
        try work.append(wa, .{ .boxy_tag_body = .{ .block = on_match, .stmt = tag_match.on_match } });
    }

    fn emitBoxyCallDict(self: *MonoLlvmCodeGen, assign: anytype) Error!void {
        try self.prepareLocalWrite(assign.target);
        if (self.builder == null) return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const ptr_ty = try self.ptrType();
        const arg_locals = self.store.getLocalSpan(assign.args);
        const arg_desc_locals = self.store.getLocalSpan(assign.arg_descs);
        const hidden_locals = self.store.getLocalSpan(assign.hidden_args);
        if (arg_desc_locals.len != arg_locals.len) return error.CompilationFailed;
        const word = self.targetWordSize();
        const layout_offset = word;
        const desc_offset = std.mem.alignForward(u32, layout_offset + 4, word);
        const arg_stride = desc_offset + word;

        const args_ptr = if (arg_locals.len == 0) try self.boxyNullPtr() else blk: {
            const raw = try self.allocEntryBlockSlot(.i8, @intCast(arg_locals.len * arg_stride), self.targetPointerAlignment(), "boxy_call_args");
            for (0..arg_locals.len) |i| {
                const local = GuardedList.at(arg_locals, i);
                const desc_local = GuardedList.at(arg_desc_locals, i);
                const entry = try self.offsetPtr(raw, @intCast(i * arg_stride));
                try self.storePointer(entry, try self.boxyValuePtr(local));
                _ = wip.store(.normal, try self.boxyInt(.i32, @intFromEnum(self.localLayout(local))), try self.offsetPtr(entry, layout_offset), LlvmBuilder.Alignment.fromByteUnits(4)) catch return error.OutOfMemory;
                const desc = try self.loadPointer(self.slot(desc_local).ptr);
                try self.storePointer(try self.offsetPtr(entry, desc_offset), desc);
            }
            break :blk raw;
        };

        const hidden_ptr = if (hidden_locals.len == 0) try self.boxyNullPtr() else blk: {
            const raw = try self.allocEntryBlockSlot(ptr_ty, @intCast(hidden_locals.len), self.targetPointerAlignment(), "boxy_hidden_args");
            for (0..hidden_locals.len) |i| {
                const local = GuardedList.at(hidden_locals, i);
                try self.storePointer(try self.offsetPtr(raw, @intCast(i * word)), try self.loadPointer(self.slot(local).ptr));
            }
            break :blk raw;
        };

        const out_desc = try self.boxyOutDescPtr("boxy_call_desc");
        try self.callBoxyVoid(
            "roc_boxy_call_dict",
            &.{ ptr_ty, ptr_ty, ptr_ty, ptr_ty, .i32, .i32, ptr_ty, self.ptrSizedIntType(), ptr_ty, self.ptrSizedIntType(), ptr_ty, .i32 },
            &.{
                try self.boxyValuePtr(assign.target),
                out_desc,
                if (self.host_call_mode == .vtable) self.testInvocationContext() else try self.boxyNullPtr(),
                try self.resolveBoxyDict(assign.dict),
                try self.boxyInt(.i32, assign.method_slot),
                try self.boxyInt(.i32, @intFromEnum(assign.method)),
                args_ptr,
                try self.boxyInt(self.ptrSizedIntType(), arg_locals.len),
                hidden_ptr,
                try self.boxyInt(self.ptrSizedIntType(), hidden_locals.len),
                if (assign.result_desc) |ref| try self.resolveBoxyDesc(ref) else try self.boxyNullPtr(),
                try self.boxyInt(.i32, @intFromEnum(self.localLayout(assign.target))),
            },
        );
        try self.storeBoxyOutDesc(assign.target, out_desc);
    }

    fn emitBoxyDynamicLiteral(
        self: *MonoLlvmCodeGen,
        target: LocalId,
        value: i128,
        desc: lir.LIR.BoxyDescRef,
        default_layout: layout.Idx,
        fractional: bool,
    ) Error!void {
        const ptr_ty = try self.ptrType();
        const literal_ptr = try self.allocEntryBlockSlot(.i128, 1, LlvmBuilder.Alignment.fromByteUnits(16), "boxy_literal");
        try self.storeI128Literal(literal_ptr, .i128, value);
        const out_desc = try self.boxyOutDescPtr("boxy_literal_desc");
        try self.callBoxyVoid(
            if (fractional) "roc_boxy_dynamic_frac_literal_ref" else "roc_boxy_dynamic_num_literal_ref",
            &.{ ptr_ty, ptr_ty, ptr_ty, ptr_ty, .i32, .i32 },
            &.{
                try self.boxyValuePtr(target),
                out_desc,
                literal_ptr,
                try self.resolveBoxyDesc(desc),
                try self.boxyInt(.i32, @intFromEnum(default_layout)),
                try self.boxyInt(.i32, @intFromEnum(self.localLayout(target))),
            },
        );
        try self.storeBoxyOutDesc(target, out_desc);
    }

    fn callProcFunctionIndex(
        self: *MonoLlvmCodeGen,
        func: LlvmBuilder.Function.Index,
        proc: LirProcSpec,
        ret_ptr: LlvmBuilder.Value,
        args_ptr: LlvmBuilder.Value,
        out_desc_ptr: ?LlvmBuilder.Value,
        is_cold: bool,
    ) Error!void {
        if ((proc.runtime_ret_desc != null) != (out_desc_ptr != null)) {
            llvmInvariantFmt(
                "procedure descriptor output ({}) did not match callee ABI ({})",
                .{ out_desc_ptr != null, proc.runtime_ret_desc != null },
            );
        }
        if (self.host_call_mode == .extern_symbols) {
            if (out_desc_ptr) |desc_ptr| {
                _ = try self.callFunctionIndex(func, &.{ ret_ptr, args_ptr, desc_ptr }, is_cold);
            } else {
                _ = try self.callFunctionIndex(func, &.{ ret_ptr, args_ptr }, is_cold);
            }
        } else if (out_desc_ptr) |desc_ptr| {
            _ = try self.callFunctionIndex(func, &.{ self.rocOps(), self.testInvocationContext(), ret_ptr, args_ptr, desc_ptr }, is_cold);
        } else {
            _ = try self.callFunctionIndex(func, &.{ self.rocOps(), self.testInvocationContext(), ret_ptr, args_ptr }, is_cold);
        }
    }

    fn emitDirectCall(
        self: *MonoLlvmCodeGen,
        target: LocalId,
        proc_id: LirProcSpecId,
        args: LocalSpan,
        out_desc: ?LocalId,
        is_cold: bool,
    ) Error!void {
        try self.prepareLocalWrite(target);
        const proc = self.store.getProcSpec(proc_id);
        if ((out_desc != null) != (proc.runtime_ret_desc != null)) {
            llvmInvariantFmt(
                "direct call to proc {d} descriptor output ({}) did not match callee ABI ({})",
                .{ @intFromEnum(proc_id), out_desc != null, proc.runtime_ret_desc != null },
            );
        }
        const arg_locals = self.store.getLocalSpan(args);
        try self.materializeLocalSpanIfDeferred(arg_locals);
        const param_locals = self.store.getLocalSpan(proc.args);
        if (arg_locals.len != param_locals.len) return error.CompilationFailed;
        if (proc.hosted) |hosted| {
            const arg_layouts = try self.allocator.alloc(layout.Idx, arg_locals.len);
            defer self.allocator.free(arg_layouts);
            for (0..param_locals.len) |i| {
                const param = GuardedList.at(param_locals, i);
                arg_layouts[i] = self.store.getLocal(param).layout_idx;
            }
            const arg_ptrs = try self.allocator.alloc(LlvmBuilder.Value, arg_locals.len);
            defer self.allocator.free(arg_ptrs);
            for (0..arg_locals.len) |i| {
                const arg_local = GuardedList.at(arg_locals, i);
                arg_ptrs[i] = self.slot(arg_local).ptr;
            }
            try self.emitHostedCallCAbi(hosted, arg_ptrs, arg_layouts, self.slot(target).ptr, self.localLayout(target));
            return;
        }

        const arg_layouts = try self.allocator.alloc(layout.Idx, arg_locals.len);
        defer self.allocator.free(arg_layouts);
        for (0..param_locals.len) |i| {
            const param = GuardedList.at(param_locals, i);
            arg_layouts[i] = self.store.getLocal(param).layout_idx;
        }
        const args_buf = try self.allocArgBuffer(arg_layouts, true);
        try self.packRocArgsFromLocals(args_buf, arg_locals, arg_layouts);
        const func = self.proc_registry.get(@intFromEnum(proc_id)) orelse return error.CompilationFailed;
        const out_desc_ptr = if (proc.runtime_ret_desc != null)
            try self.boxyOutDescPtr("direct_call_desc")
        else
            null;
        try self.callProcFunctionIndex(func, proc, self.slot(target).ptr, args_buf, out_desc_ptr, is_cold);
        if (out_desc) |desc_local| {
            try self.prepareLocalWrite(desc_local);
            try self.storePointer(self.slot(desc_local).ptr, try self.loadPointer(out_desc_ptr.?));
        }
    }

    fn emitErasedCall(
        self: *MonoLlvmCodeGen,
        target: LocalId,
        closure: LocalId,
        args: LocalSpan,
        arg_layouts: lir.LIR.BoxySpan,
        arg_descs: LocalSpan,
        arg_desc_keys: lir.LIR.BoxySpan,
        arg_plan: lir.LIR.ErasedCallArgsPlanId,
        result_desc: ?lir.LIR.BoxyDescRef,
        out_desc: ?LocalId,
        reuse_closure: bool,
    ) Error!void {
        try self.prepareLocalWrite(target);
        try self.materializeLocalIfDeferred(closure);
        const builder = self.builder orelse return error.CompilationFailed;
        const ptr_ty = try self.ptrType();
        const closure_ptr = try self.loadPointer(self.slot(closure).ptr);
        const fn_ptr = try self.loadPointer(closure_ptr);
        const capture_ptr = try self.offsetPtr(closure_ptr, builtins.erased_callable.capture_offset);
        const reuse_ptr = if (reuse_closure) closure_ptr else builder.nullValue(ptr_ty) catch return error.OutOfMemory;
        const arg_locals = self.store.getLocalSpan(args);
        const arg_desc_locals = self.store.getLocalSpan(arg_descs);
        if (arg_locals.len != arg_layouts.len) {
            llvmInvariantFmt(
                "erased call passed {d} arguments but {d} runtime layouts",
                .{ arg_locals.len, arg_layouts.len },
            );
        }
        if (arg_desc_locals.len != arg_desc_keys.len) {
            llvmInvariantFmt(
                "erased call passed {d} descriptors but {d} descriptor keys",
                .{ arg_desc_locals.len, arg_desc_keys.len },
            );
        }
        try self.materializeLocalSpanIfDeferred(arg_locals);
        try self.materializeLocalSpanIfDeferred(arg_desc_locals);
        const source_layouts = try self.allocator.alloc(layout.Idx, arg_locals.len);
        defer self.allocator.free(source_layouts);
        for (0..arg_locals.len) |i| {
            const local = GuardedList.at(arg_locals, i);
            source_layouts[i] = self.layouts().runtimeRepresentationLayoutIdx(self.localLayout(local));
        }
        const args_buf = if (arg_locals.len == 0)
            builder.nullValue(ptr_ty) catch return error.OutOfMemory
        else blk: {
            const plan = self.store.getErasedCallArgsPlan(arg_plan);
            const offsets = self.store.getErasedCallArgOffsets(plan);
            if (offsets.len != arg_locals.len) return error.CompilationFailed;
            const buf = try self.allocEntryBlockSlot(
                .i8,
                @max(plan.size, 1),
                LlvmBuilder.Alignment.fromByteUnits(plan.alignment),
                "erased_args",
            );
            try self.packErasedArgsFromLocals(buf, arg_locals, source_layouts, offsets);
            break :blk buf;
        };
        const arg_descs_ptr = if (arg_desc_locals.len == 0)
            builder.nullValue(ptr_ty) catch return error.OutOfMemory
        else blk: {
            const desc_buf = try self.allocEntryBlockSlot(ptr_ty, @intCast(arg_desc_locals.len), self.targetPointerAlignment(), "erased_arg_descs");
            for (0..arg_desc_locals.len) |i| {
                const desc_local = GuardedList.at(arg_desc_locals, i);
                try self.storePointer(
                    try self.offsetPtr(desc_buf, @intCast(i * self.targetWordSize())),
                    try self.loadPointer(self.slot(desc_local).ptr),
                );
            }
            break :blk desc_buf;
        };
        const ret_ptr = if (self.slot(target).size == 0)
            builder.nullValue(ptr_ty) catch return error.OutOfMemory
        else
            self.slot(target).ptr;
        const result_desc_ptr = if (result_desc) |desc| try self.resolveBoxyDesc(desc) else try self.boxyNullPtr();
        const out_desc_ptr = try self.boxyOutDescPtr("boxy_erased_result_desc");
        try self.callBoxyVoid(
            "roc_boxy_call_erased",
            &.{ ptr_ty, ptr_ty, .i1, ptr_ty, ptr_ty, ptr_ty, ptr_ty, ptr_ty, ptr_ty, ptr_ty, .i32, ptr_ty, .i32, .i32, .i32, .i32 },
            &.{
                self.rocOps(),
                if (self.host_call_mode == .vtable) self.testInvocationContext() else try self.boxyNullPtr(),
                try self.boxyInt(.i1, @intFromBool(self.host_call_mode == .vtable)),
                fn_ptr,
                ret_ptr,
                args_buf,
                capture_ptr,
                reuse_ptr,
                out_desc_ptr,
                result_desc_ptr,
                try self.boxyInt(.i32, @intFromEnum(self.layouts().runtimeRepresentationLayoutIdx(self.localLayout(target)))),
                arg_descs_ptr,
                try self.boxyInt(.i32, arg_desc_keys.start),
                try self.boxyInt(.i32, arg_desc_keys.len),
                try self.boxyInt(.i32, arg_layouts.start),
                try self.boxyInt(.i32, arg_layouts.len),
            },
        );
        if (out_desc) |desc_local| {
            try self.prepareLocalWrite(desc_local);
            try self.storePointer(self.slot(desc_local).ptr, try self.loadPointer(out_desc_ptr));
        }
    }

    fn emitPackedErasedFn(
        self: *MonoLlvmCodeGen,
        target: LocalId,
        proc_id: LirProcSpecId,
        capture: ?LocalId,
        capture_layout: ?layout.Idx,
        on_drop: lir.LIR.ErasedCallableOnDrop,
        result_desc: ?lir.LIR.BoxyDescRef,
        reuse: ?LocalId,
        reuse_unique: bool,
    ) Error!void {
        try self.prepareLocalWrite(target);
        if (capture) |capture_local| try self.materializeLocalIfDeferred(capture_local);
        if (reuse) |reuse_local| try self.materializeLocalIfDeferred(reuse_local);
        const builder = self.builder orelse return error.CompilationFailed;
        const ptr_ty = try self.ptrType();
        const capture_size = if (capture_layout) |idx| self.layoutByteSize(idx) else 0;
        const metadata_offset: u32 = @intCast(builtins.erased_callable.compilerMetadataOffset(capture_size));
        const total_capture_size: u32 = metadata_offset + @sizeOf(builtins.erased_callable.CompilerMetadata);
        const proc_fn = self.proc_registry.get(@intFromEnum(proc_id)) orelse return error.CompilationFailed;
        const null_ptr = builder.nullValue(ptr_ty) catch return error.OutOfMemory;
        const on_drop_value = switch (on_drop) {
            .none => null_ptr,
            .rc_helper => |helper_key| blk: {
                // `on_drop` is selected here at closure creation, which is not
                // an RC statement and makes no thread-confinement claim, so it
                // is always the atomic helper (atomic is always sound).
                break :blk if (try self.declareRcHelper(helper_key, .atomic)) |helper_fn|
                    helper_fn.toValue(builder)
                else
                    null_ptr;
            },
            .boxy_capture => |drop| blk: {
                const helper = try self.declareBoxyCaptureDropHelper(drop.capture_layout, drop.desc_field_offset);
                break :blk helper.toValue(builder);
            },
            .interpreter_context_drop => return error.CompilationFailed,
        };
        const metadata_desc = if (result_desc) |desc| try self.resolveBoxyDesc(desc) else try self.boxyNullPtr();

        const data_ptr = if (reuse) |reuse_local| blk: {
            const capture_src = try self.allocEntryBlockSlot(
                .i8,
                total_capture_size,
                LlvmBuilder.Alignment.fromByteUnits(builtins.erased_callable.capture_alignment),
                "erased_repack_capture",
            );
            if (capture) |capture_local| {
                if (capture_size > 0) {
                    try self.copyBytes(capture_src, self.slot(capture_local).ptr, capture_size, self.alignmentForLayout(capture_layout.?));
                }
            }
            try self.storePointer(
                try self.offsetPtr(capture_src, metadata_offset + @offsetOf(builtins.erased_callable.CompilerMetadata, "result_desc")),
                metadata_desc,
            );
            const update_mode = if (reuse_unique) builtins.utils.UpdateMode.InPlace else builtins.utils.UpdateMode.Immutable;
            break :blk try self.callBuiltin(
                builtinSymbol(.erased_callable_repack),
                ptr_ty,
                &.{ ptr_ty, ptr_ty, ptr_ty, ptr_ty, self.ptrSizedIntType(), .i8, ptr_ty },
                &.{
                    try self.loadPointer(self.slot(reuse_local).ptr),
                    proc_fn.toValue(builder),
                    on_drop_value,
                    capture_src,
                    builder.intValue(self.ptrSizedIntType(), total_capture_size) catch return error.OutOfMemory,
                    builder.intValue(.i8, @intFromEnum(update_mode)) catch return error.OutOfMemory,
                    self.rocOps(),
                },
            );
        } else blk: {
            const payload_size: u64 = builtins.erased_callable.compilerPayloadSize(capture_size);
            const fresh_ptr = try self.callBuiltin(
                builtinSymbol(.allocate_with_refcount),
                ptr_ty,
                &.{ self.ptrSizedIntType(), .i32, .i1, ptr_ty },
                &.{
                    builder.intValue(self.ptrSizedIntType(), payload_size) catch return error.OutOfMemory,
                    builder.intValue(.i32, builtins.erased_callable.payload_alignment) catch return error.OutOfMemory,
                    builder.intValue(.i1, 0) catch return error.OutOfMemory,
                    self.rocOps(),
                },
            );
            try self.storePointer(fresh_ptr, proc_fn.toValue(builder));
            try self.storePointer(try self.offsetPtr(fresh_ptr, self.targetWordSize()), on_drop_value);
            if (capture) |capture_local| {
                if (capture_size > 0) {
                    const capture_dst = try self.offsetPtr(fresh_ptr, builtins.erased_callable.capture_offset);
                    try self.copyBytes(capture_dst, self.slot(capture_local).ptr, capture_size, self.alignmentForLayout(capture_layout.?));
                }
            }
            try self.storePointer(
                try self.offsetPtr(fresh_ptr, builtins.erased_callable.capture_offset + metadata_offset + @offsetOf(builtins.erased_callable.CompilerMetadata, "result_desc")),
                metadata_desc,
            );
            break :blk fresh_ptr;
        };
        if (result_desc != null) {
            const proc_spec = self.store.getProcSpec(proc_id);
            try self.callBoxyVoid(
                "roc_boxy_register_erased_proc",
                &.{ try self.ptrType(), .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32 },
                &.{
                    proc_fn.toValue(builder),
                    try self.boxyInt(.i32, @intFromEnum(proc_id)),
                    try self.boxyInt(.i32, @intFromEnum(self.layouts().runtimeRepresentationLayoutIdx(proc_spec.ret_layout))),
                    try self.boxyInt(.i32, metadata_offset),
                    try self.boxyInt(.i32, proc_spec.erased_arg_layouts.start),
                    try self.boxyInt(.i32, proc_spec.erased_arg_layouts.len),
                    try self.boxyInt(.i32, proc_spec.erased_arg_desc_offsets.start),
                    try self.boxyInt(.i32, proc_spec.erased_arg_desc_offsets.len),
                    try self.boxyInt(.i32, 0),
                },
            );
        }
        try self.storePointer(self.slot(target).ptr, data_ptr);
    }

    fn boxyCaptureDropKey(capture_layout: layout.Idx, desc_field_offset: u32) u64 {
        return (@as(u64, @intFromEnum(capture_layout)) << 32) | desc_field_offset;
    }

    fn declareBoxyCaptureDropHelper(
        self: *MonoLlvmCodeGen,
        capture_layout: layout.Idx,
        desc_field_offset: u32,
    ) Error!LlvmBuilder.Function.Index {
        const key = boxyCaptureDropKey(capture_layout, desc_field_offset);
        if (self.boxy_capture_drop_helpers.get(key)) |entry| return entry.function;

        const builder = self.builder orelse return error.CompilationFailed;
        const ptr_ty = try self.ptrType();
        const fn_ty = builder.fnType(.void, &.{ ptr_ty, ptr_ty }, .normal) catch return error.OutOfMemory;
        const name = builder.strtabStringFmt("roc_boxy_capture_drop_{x}", .{key}) catch return error.OutOfMemory;
        const func = builder.addFunction(fn_ty, name, .default) catch return error.OutOfMemory;
        func.setLinkage(.internal, builder);
        var attrs: LlvmBuilder.FunctionAttributes.Wip = .{};
        defer attrs.deinit(builder);
        try self.addGeneratedFunctionStackProbeAttrs(&attrs);
        func.setAttributes(attrs.finish(builder) catch return error.OutOfMemory, builder);
        try self.boxy_capture_drop_helpers.put(key, .{
            .capture_layout = capture_layout,
            .desc_field_offset = desc_field_offset,
            .function = func,
        });
        return func;
    }

    fn compilePendingBoxyCaptureDropHelpers(self: *MonoLlvmCodeGen) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        var it = self.boxy_capture_drop_helpers.iterator();
        while (it.next()) |map_entry| {
            if (map_entry.value_ptr.compiled) continue;
            const entry = map_entry.value_ptr.*;

            const outer_wip = self.wip;
            const outer_roc_ops = self.roc_ops_arg;
            const outer_ret = self.ret_ptr_arg;
            const outer_args = self.args_ptr_arg;
            const outer_capture = self.capture_ptr_arg;
            const outer_ret_desc_ptr = self.ret_desc_ptr_arg;
            const outer_runtime_ret_desc = self.current_runtime_ret_desc;
            const outer_ret_layout = self.current_ret_layout;
            const outer_slots = self.local_slots;
            defer {
                self.wip = outer_wip;
                self.roc_ops_arg = outer_roc_ops;
                self.ret_ptr_arg = outer_ret;
                self.args_ptr_arg = outer_args;
                self.capture_ptr_arg = outer_capture;
                self.ret_desc_ptr_arg = outer_ret_desc_ptr;
                self.current_runtime_ret_desc = outer_runtime_ret_desc;
                self.current_ret_layout = outer_ret_layout;
                self.local_slots = outer_slots;
            }

            var wip = LlvmBuilder.WipFunction.init(builder, .{ .function = entry.function, .strip = true }) catch return error.OutOfMemory;
            defer wip.deinit();
            self.wip = &wip;
            self.roc_ops_arg = wip.arg(1);
            self.ret_ptr_arg = null;
            self.args_ptr_arg = null;
            self.capture_ptr_arg = wip.arg(0);
            self.ret_desc_ptr_arg = null;
            self.current_runtime_ret_desc = null;
            self.current_ret_layout = .zst;
            self.local_slots = &.{};

            const start = wip.block(0, "entry") catch return error.OutOfMemory;
            const drop = wip.block(0, "drop") catch return error.OutOfMemory;
            const done = wip.block(0, "done") catch return error.OutOfMemory;
            wip.cursor = .{ .block = start };
            const capture = wip.arg(0);
            const is_null = wip.icmp(.eq, capture, try self.boxyNullPtr(), "") catch return error.OutOfMemory;
            _ = wip.brCond(is_null, done, drop, .none) catch return error.OutOfMemory;

            wip.cursor = .{ .block = drop };
            const desc = try self.loadPointer(try self.offsetPtr(capture, entry.desc_field_offset));
            const ptr_ty = try self.ptrType();
            try self.callBoxyVoid(
                "roc_boxy_drop",
                &.{ ptr_ty, .i32, ptr_ty, .i8, .i16, .i8 },
                &.{
                    capture,
                    try self.boxyInt(.i32, @intFromEnum(entry.capture_layout)),
                    desc,
                    try self.boxyInt(.i8, @intFromEnum(layout.RcOp.decref)),
                    try self.boxyInt(.i16, 1),
                    try self.boxyInt(.i8, @intFromEnum(RcAtomicity.atomic)),
                },
            );
            _ = wip.br(done) catch return error.OutOfMemory;

            wip.cursor = .{ .block = done };
            _ = wip.retVoid() catch return error.OutOfMemory;
            try self.finishCurrentWipFunction();
            map_entry.value_ptr.compiled = true;
        }
    }

    fn emitListLiteral(self: *MonoLlvmCodeGen, target: LocalId, elems: LocalSpan) Error!void {
        try self.prepareLocalWrite(target);
        const builder = self.builder orelse return error.CompilationFailed;
        const elem_locals = self.store.getLocalSpan(elems);
        try self.materializeLocalSpanIfDeferred(elem_locals);
        const target_layout = self.localLayout(target);
        const abi = self.boxyAwareBuiltinListAbi(target_layout);
        const target_ptr = self.slot(target).ptr;
        if (elem_locals.len == 0) {
            try self.zeroBytes(target_ptr, self.layoutByteSize(target_layout));
            return;
        }
        if (abi.elem_size == 0) {
            try self.storeListFields(target_ptr, builder.nullValue(try self.ptrType()) catch return error.OutOfMemory, elem_locals.len, 0);
            return;
        }
        try self.callBuiltinVoid(
            builtinSymbol(LowLevelBuiltins.listOp(.list_with_capacity)),
            &.{ try self.ptrType(), .i64, .i32, self.ptrSizedIntType(), .i1, try self.ptrType() },
            &.{
                target_ptr,
                builder.intValue(.i64, elem_locals.len) catch return error.OutOfMemory,
                builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory,
                builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory,
                builder.intValue(.i1, @intFromBool(abi.contains_refcounted)) catch return error.OutOfMemory,
                self.rocOps(),
            },
        );
        const bytes_ptr = try self.loadPointer(target_ptr);
        for (0..elem_locals.len) |i| {
            const elem_local = GuardedList.at(elem_locals, i);
            const dst = try self.offsetPtr(bytes_ptr, @as(u32, @intCast(i)) * abi.elem_size);
            try self.copyBytes(dst, self.slot(elem_local).ptr, abi.elem_size, self.alignmentForLayout(abi.elem_layout_idx.?));
        }
        try self.storeListLen(target_ptr, builder.intValue(self.ptrSizedIntType(), elem_locals.len) catch return error.OutOfMemory);
    }

    fn emitStructLiteral(self: *MonoLlvmCodeGen, target: LocalId, fields: LocalSpan) Error!void {
        try self.prepareLocalWrite(target);
        const field_locals = self.store.getLocalSpan(fields);
        try self.materializeLocalSpanIfDeferred(field_locals);
        const allocated = try self.allocAggregateTarget(target);
        const base_layout = self.layoutValue(allocated.layout_idx);
        if (base_layout.tag != .struct_) return;
        for (0..field_locals.len) |i| {
            const field_local = GuardedList.at(field_locals, i);
            const field_layout = self.layouts().getStructFieldLayoutByOriginalIndex(base_layout.getStruct().idx, @intCast(i));
            const field_size = self.layoutByteSize(field_layout);
            if (field_size == 0) continue;
            const offset = self.layouts().getStructFieldOffsetByOriginalIndex(base_layout.getStruct().idx, @intCast(i));
            const dst = try self.offsetPtr(allocated.ptr, offset);
            try self.copyBytes(dst, self.slot(field_local).ptr, field_size, self.alignmentForLayout(field_layout));
        }
    }

    fn emitTagLiteral(self: *MonoLlvmCodeGen, target: LocalId, discriminant: u16, payload: ?LocalId) Error!void {
        try self.prepareLocalWrite(target);
        if (payload) |payload_local| try self.materializeLocalIfDeferred(payload_local);
        const allocated = try self.allocAggregateTarget(target);
        if (self.layoutByteSize(allocated.layout_idx) > 0) {
            try self.writeTagDiscriminant(allocated.ptr, allocated.layout_idx, discriminant);
        }
        if (payload) |payload_local| {
            const payload_layout = self.tagPayloadLayout(allocated.layout_idx, discriminant);
            const payload_size = self.layoutByteSize(payload_layout);
            if (payload_size > 0) {
                try self.copyBytes(allocated.ptr, self.slot(payload_local).ptr, payload_size, self.alignmentForLayout(payload_layout));
            }
        }
    }

    fn emitStoreStruct(self: *MonoLlvmCodeGen, dest: LocalId, struct_layout: layout.Idx, fields: LocalSpan) Error!void {
        const field_locals = self.store.getLocalSpan(fields);
        try self.materializeLocalSpanIfDeferred(field_locals);

        const base_layout = self.layoutValue(struct_layout);
        if (base_layout.tag != .struct_) return;

        const dst = try self.loadPointer(self.slot(dest).ptr);
        try self.zeroBytes(dst, self.layoutByteSize(struct_layout));
        for (0..field_locals.len) |i| {
            const field_local = GuardedList.at(field_locals, i);
            const field_layout = self.layouts().getStructFieldLayoutByOriginalIndex(base_layout.getStruct().idx, @intCast(i));
            const field_size = self.layoutByteSize(field_layout);
            if (field_size == 0) continue;
            const offset = self.layouts().getStructFieldOffsetByOriginalIndex(base_layout.getStruct().idx, @intCast(i));
            const field_dst = try self.offsetPtr(dst, offset);
            try self.copyBytes(field_dst, self.slot(field_local).ptr, field_size, self.alignmentForLayout(field_layout));
        }
    }

    fn emitStoreTag(self: *MonoLlvmCodeGen, dest: LocalId, tag_layout: layout.Idx, discriminant: u16, payload: ?LocalId) Error!void {
        if (payload) |payload_local| try self.materializeLocalIfDeferred(payload_local);

        const dst = try self.loadPointer(self.slot(dest).ptr);
        const layout_size = self.layoutByteSize(tag_layout);
        if (layout_size == 0) return;

        try self.zeroBytes(dst, layout_size);
        if (payload) |payload_local| {
            const payload_layout = self.tagPayloadLayout(tag_layout, discriminant);
            const payload_size = self.layoutByteSize(payload_layout);
            if (payload_size > 0) {
                try self.copyBytes(dst, self.slot(payload_local).ptr, payload_size, self.alignmentForLayout(payload_layout));
            }
        }
        try self.writeTagDiscriminant(dst, tag_layout, discriminant);
    }

    fn allocAggregateTarget(self: *MonoLlvmCodeGen, target: LocalId) Error!ResolvedBase {
        const builder = self.builder orelse return error.CompilationFailed;
        const target_layout = self.localLayout(target);
        const layout_val = self.layoutValue(target_layout);
        const target_ptr = self.slot(target).ptr;
        if (layout_val.tag == .box) {
            const abi = self.layouts().builtinBoxAbi(target_layout);
            const data_ptr = try self.callBuiltin(
                builtinSymbol(.allocate_with_refcount),
                try self.ptrType(),
                &.{ self.ptrSizedIntType(), .i32, .i1, try self.ptrType() },
                &.{
                    builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory,
                    builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory,
                    builder.intValue(.i1, @intFromBool(abi.contains_refcounted)) catch return error.OutOfMemory,
                    self.rocOps(),
                },
            );
            try self.zeroBytes(data_ptr, abi.elem_size);
            try self.storePointer(target_ptr, data_ptr);
            return .{ .ptr = data_ptr, .layout_idx = abi.elem_layout_idx orelse .zst };
        } else if (layout_val.tag == .box_of_zst) {
            try self.storePointer(target_ptr, builder.nullValue(try self.ptrType()) catch return error.OutOfMemory);
            return .{ .ptr = target_ptr, .layout_idx = .zst };
        } else {
            if (self.slot(target).size > 0) try self.zeroBytes(target_ptr, self.slot(target).size);
            return .{ .ptr = target_ptr, .layout_idx = target_layout };
        }
    }

    fn emitLowLevel(self: *MonoLlvmCodeGen, target: LocalId, op: lir.LowLevel, args: LocalSpan, unique_args: u64, interchangeable: layout.WidthValues(bool)) Error!void {
        try self.prepareLocalWrite(target);
        const arg_locals = self.store.getLocalSpan(args);
        if (!op.acceptsStrViewArgs()) {
            try self.materializeLocalSpanIfDeferred(arg_locals);
        }
        switch (op) {
            .num_plus, .num_minus, .num_times => unreachable,
            .bool_not => {
                const value = try self.loadBool(self.slot(GuardedList.at(arg_locals, 0)).ptr);
                const not_value = (self.wip orelse return error.CompilationFailed).not(value, "") catch return error.OutOfMemory;
                try self.storeBool(self.slot(target).ptr, not_value);
            },
            .num_is_eq => try self.storeBool(self.slot(target).ptr, try self.emitValueEqual(self.slot(GuardedList.at(arg_locals, 0)).ptr, self.slot(GuardedList.at(arg_locals, 1)).ptr, self.localLayout(GuardedList.at(arg_locals, 0)))),
            .num_is_gt, .num_is_gte, .num_is_lt, .num_is_lte => try self.emitNumericCompare(target, op, arg_locals),
            .compare => try self.emitNumericOrderCompare(target, arg_locals),
            .num_int_add_wrap,
            .num_int_add_crash_on_overflow,
            .num_int_add_overflows,
            .num_int_add_proven_cannot_overflow,
            .num_int_sub_wrap,
            .num_int_sub_crash_on_overflow,
            .num_int_sub_overflows,
            .num_int_sub_proven_cannot_overflow,
            .num_int_mul_wrap,
            .num_int_mul_crash_on_overflow,
            .num_int_mul_overflows,
            .num_int_mul_proven_cannot_overflow,
            .num_float_add,
            .num_float_sub,
            .num_float_mul,
            .dec_mul,
            .num_div_by,
            .num_div_by_checked,
            .num_div_trunc_by,
            .num_div_trunc_by_checked,
            .num_rem_by,
            .num_rem_by_checked,
            .num_mod_by,
            .num_mod_by_checked,
            .num_shift_left_by,
            .num_shift_right_by,
            .num_shift_right_zf_by,
            .num_bitwise_and,
            .num_bitwise_or,
            .num_bitwise_xor,
            => try self.emitNumericBinary(target, op, arg_locals),
            .num_bitwise_not => try self.emitNumericBitwiseNot(target, GuardedList.at(arg_locals, 0)),
            .num_count_one_bits,
            .num_count_leading_zero_bits,
            .num_count_trailing_zero_bits,
            => try self.emitNumericBitCount(target, op, GuardedList.at(arg_locals, 0)),
            .num_from_le_bytes_unchecked => try self.emitNumFromLeBytes(target, arg_locals),
            .simd_load_16_unchecked => try self.emitSimdLoad(target, arg_locals),
            .simd_store_16_unchecked => try self.emitSimdStore(target, arg_locals, unique_args),
            .simd_append_16 => try self.emitSimdAppend(target, arg_locals, unique_args),
            .simd_splat,
            .simd_get_lane_unchecked,
            .simd_with_lane_unchecked,
            .simd_to_u128_bits,
            .simd_from_u128_bits,
            .simd_add_wrap,
            .simd_sub_wrap,
            .simd_add_sat,
            .simd_sub_sat,
            .simd_neg_wrap,
            .simd_abs_wrap,
            .simd_min,
            .simd_max,
            .simd_abs_diff,
            .simd_avg_rounded,
            .simd_mul_wrap,
            .simd_mul_high,
            .simd_mul_q15_sat,
            .simd_mul_wide_lo,
            .simd_mul_wide_hi,
            .simd_dot_pairs,
            .simd_dot_pairs_sat,
            .simd_sad,
            .simd_and,
            .simd_or,
            .simd_xor,
            .simd_not,
            .simd_bit_select,
            .simd_eq_lanes,
            .simd_gt_lanes,
            .simd_gte_lanes,
            .simd_bitmask,
            .simd_shl_wrap,
            .simd_shr_wrap,
            .simd_shr_zf_wrap,
            .simd_shr_rounded,
            .simd_interleave_lo,
            .simd_interleave_hi,
            .simd_even_lanes,
            .simd_odd_lanes,
            .simd_reverse_lanes,
            .simd_table_lookup,
            .simd_concat_shift_bytes,
            .simd_widen_lo,
            .simd_widen_hi,
            .simd_pairwise_add_widen,
            .simd_narrow_wrap,
            .simd_narrow_sat,
            .simd_sum_lanes,
            .simd_sum_lanes_wrap,
            .simd_clmul_lo,
            .simd_clmul_hi,
            => try self.emitSimdLowLevel(target, op, arg_locals),
            .num_negate, .num_negate_checked => try self.emitNumericNegate(target, op, GuardedList.at(arg_locals, 0)),
            .num_abs, .num_abs_checked => try self.emitNumericAbs(target, op, GuardedList.at(arg_locals, 0)),
            .num_abs_diff => try self.emitNumericAbsDiff(target, arg_locals),
            .num_pow => if (self.localLayout(target) == .dec)
                try self.emitDecPow(target, arg_locals)
            else
                try self.emitNumericFloatPow(target, arg_locals),
            .num_sqrt => try self.emitNumericSqrt(target, GuardedList.at(arg_locals, 0)),
            .num_sin => try self.emitNumericUnaryMath(target, GuardedList.at(arg_locals, 0), .num_sin),
            .num_cos => try self.emitNumericUnaryMath(target, GuardedList.at(arg_locals, 0), .num_cos),
            .num_tan => try self.emitNumericUnaryMath(target, GuardedList.at(arg_locals, 0), .num_tan),
            .num_asin => try self.emitNumericUnaryMath(target, GuardedList.at(arg_locals, 0), .num_asin),
            .num_acos => try self.emitNumericUnaryMath(target, GuardedList.at(arg_locals, 0), .num_acos),
            .num_atan => try self.emitNumericUnaryMath(target, GuardedList.at(arg_locals, 0), .num_atan),
            .num_floor => try self.emitNumericFloatUnaryIntrinsic(target, GuardedList.at(arg_locals, 0), .floor),
            .num_ceiling => try self.emitNumericFloatUnaryIntrinsic(target, GuardedList.at(arg_locals, 0), .ceil),
            .list_len => try self.storeIntToLayout(self.slot(target).ptr, try self.loadUsize(try self.offsetPtr(self.slot(GuardedList.at(arg_locals, 0)).ptr, self.rocListLenOffset())), self.localLayout(target)),
            .list_capacity => try self.emitListCapacity(target, GuardedList.at(arg_locals, 0)),
            .list_get_unsafe => try self.emitListGetUnsafe(target, arg_locals),
            .list_with_capacity => try self.emitListWithCapacity(target, arg_locals),
            .list_append_unsafe => try self.emitListAppendUnsafe(target, arg_locals),
            .list_concat => try self.emitListConcat(target, arg_locals, unique_args),
            .list_append_range_within => try self.emitListAppendRangeWithin(target, arg_locals, unique_args),
            .list_copy_range_within => try self.emitListCopyRangeWithin(target, arg_locals),
            .list_append_range_within_unsafe => try self.emitListAppendRangeWithinUnsafe(target, arg_locals),
            .list_append_sublist => try self.emitListAppendSublist(target, arg_locals, unique_args),
            .list_append_le_bytes => try self.emitListAppendLeBytes(target, arg_locals, unique_args),
            .list_slack_unique => try self.emitListSlackUnique(target, arg_locals),
            .list_owned_unique => try self.emitListOwnedUnique(target, arg_locals),
            .list_prepend => try self.emitListPrepend(target, arg_locals, unique_args),
            .list_sublist, .list_sublist_borrowed, .list_drop_first, .list_drop_last, .list_take_first, .list_take_last => try self.emitListSublist(target, op, arg_locals, unique_args),
            .list_drop_at => try self.emitListDropAt(target, arg_locals, unique_args),
            .list_swap => try self.emitListSwap(target, arg_locals, unique_args),
            .list_set => try self.emitListSet(target, arg_locals, unique_args),
            // The promotion pass proved arg0 uniquely owned: force the
            // in-place update mode instead of consulting ARC's per-site bit.
            .list_set_in_place_unsafe => try self.emitListSet(target, arg_locals, unique_args | 1),
            .list_replace_unsafe => try self.emitListReplaceUnsafe(target, arg_locals, unique_args),
            .list_map_prepare_reuse => try self.copyBytes(self.slot(target).ptr, self.slot(GuardedList.at(arg_locals, 0)).ptr, self.slot(target).size, self.slot(target).alignment),
            .list_map_can_reuse => try self.emitListMapCanReuse(target, arg_locals, interchangeable),
            .list_map_cast_unsafe => try self.copyBytes(self.slot(target).ptr, self.slot(GuardedList.at(arg_locals, 0)).ptr, self.slot(target).size, self.slot(target).alignment),
            .list_map_extract_unsafe => try self.emitListMapExtractUnsafe(target, arg_locals),
            .list_map_write_unsafe => try self.emitListMapWriteUnsafe(target, arg_locals),
            .list_reverse => try self.emitListReverse(target, arg_locals, unique_args),
            .list_reserve => try self.emitListReserve(target, arg_locals, unique_args),
            .list_release_excess_capacity => try self.emitListReleaseExcess(target, arg_locals, unique_args),
            .list_first, .list_last => try self.emitListFirstLast(target, op, arg_locals),
            .str_is_eq => try self.emitStrIsEq(target, arg_locals),
            .str_is_eq_static_small => try self.emitStrEqStaticSmall(target, arg_locals),
            .str_static_small_word_eq => try self.emitStrStaticSmallWordEq(target, arg_locals),
            .str_static_small_word_caseless_eq => try self.emitStrStaticSmallWordCaselessEq(target, arg_locals),
            .str_contains => try self.emitStrContains(target, arg_locals),
            .str_starts_with => try self.emitStrStartsWith(target, arg_locals),
            .str_ends_with => try self.emitStrEndsWith(target, arg_locals),
            .str_caseless_ascii_equals => try self.emitStrCaselessAsciiEquals(target, arg_locals),
            .str_count_utf8_bytes => try self.emitStrCountUtf8Bytes(target, GuardedList.at(arg_locals, 0)),
            .str_get_utf8_byte_unsafe => try self.emitStrGetUtf8ByteUnsafe(target, arg_locals),
            .str_substring_unsafe => try self.emitStrSubstringUnsafe(target, arg_locals),
            .str_split_first => try self.emitStrSplitFirst(target, arg_locals),
            .str_split_last => try self.emitStrSplitLast(target, arg_locals),
            .str_drop_prefix_caseless_ascii => try self.emitStrDropPrefixCaselessAscii(target, arg_locals),
            .str_concat => try self.emitStrRetBuiltin(target, builtinSymbol(LowLevelBuiltins.strOp(.str_concat)), arg_locals, unique_args),
            .str_trim => try self.emitStrUnaryRetBuiltin(target, builtinSymbol(LowLevelBuiltins.strOp(.str_trim)), GuardedList.at(arg_locals, 0), unique_args),
            .str_trim_start => try self.emitStrUnaryRetBuiltin(target, builtinSymbol(LowLevelBuiltins.strOp(.str_trim_start)), GuardedList.at(arg_locals, 0), unique_args),
            .str_trim_end => try self.emitStrUnaryRetBuiltin(target, builtinSymbol(LowLevelBuiltins.strOp(.str_trim_end)), GuardedList.at(arg_locals, 0), unique_args),
            .str_with_ascii_lowercased => try self.emitStrUnaryRetBuiltin(target, builtinSymbol(LowLevelBuiltins.strOp(.str_with_ascii_lowercased)), GuardedList.at(arg_locals, 0), unique_args),
            .str_with_ascii_uppercased => try self.emitStrUnaryRetBuiltin(target, builtinSymbol(LowLevelBuiltins.strOp(.str_with_ascii_uppercased)), GuardedList.at(arg_locals, 0), unique_args),
            .str_drop_prefix => try self.emitStrDropEdge(target, arg_locals, .prefix),
            .str_drop_suffix => try self.emitStrDropEdge(target, arg_locals, .suffix),
            .str_split_on => try self.emitStrRetBuiltin(target, builtinSymbol(LowLevelBuiltins.strOp(.str_split_on)), arg_locals, null),
            .str_join_with => try self.emitStrJoinWith(target, arg_locals),
            .str_repeat => try self.emitStrRepeat(target, arg_locals),
            .str_with_capacity => try self.emitStrWithCapacity(target, GuardedList.at(arg_locals, 0)),
            .str_reserve => try self.emitStrReserve(target, arg_locals, unique_args),
            .str_release_excess_capacity => try self.emitStrUnaryRetBuiltin(target, builtinSymbol(LowLevelBuiltins.strOp(.str_release_excess_capacity)), GuardedList.at(arg_locals, 0), unique_args),
            .str_to_utf8 => try self.emitStrToUtf8(target, GuardedList.at(arg_locals, 0)),
            .str_from_utf8_lossy => try self.emitStrFromUtf8Lossy(target, GuardedList.at(arg_locals, 0)),
            .str_from_utf8 => try self.emitStrFromUtf8(target, GuardedList.at(arg_locals, 0)),
            .str_inspect => try self.emitStrUnaryRetBuiltin(target, builtinSymbol(LowLevelBuiltins.strOp(.str_inspect)), GuardedList.at(arg_locals, 0), null),
            .dict_pseudo_seed,
            .hasher_finish,
            .hasher_write_bool,
            .hasher_write_u8,
            .hasher_write_u16,
            .hasher_write_u32,
            .hasher_write_u64,
            .hasher_write_u128,
            .hasher_write_i8,
            .hasher_write_i16,
            .hasher_write_i32,
            .hasher_write_i64,
            .hasher_write_i128,
            .hasher_write_f32,
            .hasher_write_f64,
            .hasher_write_dec,
            .hasher_write_bytes,
            .hasher_write_str,
            => try self.emitHasherLowLevel(target, op, arg_locals),
            .crypto_sha256_hash_bytes,
            .crypto_sha256_hasher_empty,
            .crypto_sha256_hasher_write,
            .crypto_sha256_hasher_finish,
            .crypto_blake3_hash_bytes,
            .crypto_blake3_hasher_empty,
            .crypto_blake3_hasher_write,
            .crypto_blake3_hasher_finish,
            => try self.emitCryptoLowLevel(target, op, arg_locals),
            .u8_from_str => try self.emitIntFromStr(target, GuardedList.at(arg_locals, 0), 1, false),
            .i8_from_str => try self.emitIntFromStr(target, GuardedList.at(arg_locals, 0), 1, true),
            .u16_from_str => try self.emitIntFromStr(target, GuardedList.at(arg_locals, 0), 2, false),
            .i16_from_str => try self.emitIntFromStr(target, GuardedList.at(arg_locals, 0), 2, true),
            .u32_from_str => try self.emitIntFromStr(target, GuardedList.at(arg_locals, 0), 4, false),
            .i32_from_str => try self.emitIntFromStr(target, GuardedList.at(arg_locals, 0), 4, true),
            .u64_from_str => try self.emitIntFromStr(target, GuardedList.at(arg_locals, 0), 8, false),
            .i64_from_str => try self.emitIntFromStr(target, GuardedList.at(arg_locals, 0), 8, true),
            .u128_from_str => try self.emitIntFromStr(target, GuardedList.at(arg_locals, 0), 16, false),
            .i128_from_str => try self.emitIntFromStr(target, GuardedList.at(arg_locals, 0), 16, true),
            .dec_from_str => try self.emitDecFromStr(target, GuardedList.at(arg_locals, 0)),
            .f32_from_str => try self.emitFloatFromStr(target, GuardedList.at(arg_locals, 0), 4),
            .f64_from_str => try self.emitFloatFromStr(target, GuardedList.at(arg_locals, 0), 8),
            .u8_to_str, .i8_to_str, .u16_to_str, .i16_to_str, .u32_to_str, .i32_to_str, .u64_to_str, .i64_to_str, .u128_to_str, .i128_to_str => try self.emitIntToStr(target, GuardedList.at(arg_locals, 0)),
            .f32_to_str, .f64_to_str => try self.emitFloatToStr(target, GuardedList.at(arg_locals, 0)),
            .f32_to_bits, .f32_from_bits, .f64_to_bits, .f64_from_bits => try self.emitFloatBitCast(target, op, GuardedList.at(arg_locals, 0)),
            .dec_from_attos, .dec_to_attos => try self.emitDecAttosMove(target, GuardedList.at(arg_locals, 0)),
            .dec_to_str => try self.emitDecToStr(target, GuardedList.at(arg_locals, 0)),
            .num_to_str => try self.emitNumToStr(target, GuardedList.at(arg_locals, 0)),
            .box_box => try self.emitBoxBox(target, GuardedList.at(arg_locals, 0)),
            // Consuming Box.unbox is normalized by ARC into the borrowed load
            // followed by explicit payload incref and box decref statements.
            .box_unbox => unreachable,
            .box_unbox_borrowed => try self.emitBoxUnbox(target, GuardedList.at(arg_locals, 0)),
            .box_prepare_update => try self.emitBoxPrepareUpdate(target, GuardedList.at(arg_locals, 0), unique_args),
            .erased_capture_load => try self.emitErasedCaptureLoad(target, GuardedList.at(arg_locals, 0)),
            .ptr_alloca => try self.emitPtrAlloca(target),
            .box_alloc_zeroed => try self.emitBoxAllocZeroed(target),
            .ptr_store => try self.emitPtrStore(GuardedList.at(arg_locals, 0), GuardedList.at(arg_locals, 1)),
            .ptr_load => try self.emitPtrLoad(target, GuardedList.at(arg_locals, 0)),
            .ptr_cast => try self.emitPtrCast(target, GuardedList.at(arg_locals, 0)),
            .crash => try self.emitCrashBytes("Roc crashed"),
            // Not conversions, and not lowered by this backend.
            .list_split_first,
            .list_split_last,
            .num_log,
            .num_round,
            => return error.UnsupportedLowLevel,
            .u8_to_i8_wrap,
            .u8_to_i8_try,
            .u8_to_i16,
            .u8_to_i32,
            .u8_to_i64,
            .u8_to_i128,
            .u8_to_u16,
            .u8_to_u32,
            .u8_to_u64,
            .u8_to_u128,
            .u8_to_f32,
            .u8_to_f64,
            .u8_to_dec,
            .i8_to_i16,
            .i8_to_i32,
            .i8_to_i64,
            .i8_to_i128,
            .i8_to_u8_wrap,
            .i8_to_u8_try,
            .i8_to_u16_wrap,
            .i8_to_u16_try,
            .i8_to_u32_wrap,
            .i8_to_u32_try,
            .i8_to_u64_wrap,
            .i8_to_u64_try,
            .i8_to_u128_wrap,
            .i8_to_u128_try,
            .i8_to_f32,
            .i8_to_f64,
            .i8_to_dec,
            .u16_to_i8_wrap,
            .u16_to_i8_try,
            .u16_to_i16_wrap,
            .u16_to_i16_try,
            .u16_to_i32,
            .u16_to_i64,
            .u16_to_i128,
            .u16_to_u8_wrap,
            .u16_to_u8_try,
            .u16_to_u32,
            .u16_to_u64,
            .u16_to_u128,
            .u16_to_f32,
            .u16_to_f64,
            .u16_to_dec,
            .i16_to_i8_wrap,
            .i16_to_i8_try,
            .i16_to_i32,
            .i16_to_i64,
            .i16_to_i128,
            .i16_to_u8_wrap,
            .i16_to_u8_try,
            .i16_to_u16_wrap,
            .i16_to_u16_try,
            .i16_to_u32_wrap,
            .i16_to_u32_try,
            .i16_to_u64_wrap,
            .i16_to_u64_try,
            .i16_to_u128_wrap,
            .i16_to_u128_try,
            .i16_to_f32,
            .i16_to_f64,
            .i16_to_dec,
            .u32_to_i8_wrap,
            .u32_to_i8_try,
            .u32_to_i16_wrap,
            .u32_to_i16_try,
            .u32_to_i32_wrap,
            .u32_to_i32_try,
            .u32_to_i64,
            .u32_to_i128,
            .u32_to_u8_wrap,
            .u32_to_u8_try,
            .u32_to_u16_wrap,
            .u32_to_u16_try,
            .u32_to_u64,
            .u32_to_u128,
            .u32_to_f32,
            .u32_to_f64,
            .u32_to_dec,
            .i32_to_i8_wrap,
            .i32_to_i8_try,
            .i32_to_i16_wrap,
            .i32_to_i16_try,
            .i32_to_i64,
            .i32_to_i128,
            .i32_to_u8_wrap,
            .i32_to_u8_try,
            .i32_to_u16_wrap,
            .i32_to_u16_try,
            .i32_to_u32_wrap,
            .i32_to_u32_try,
            .i32_to_u64_wrap,
            .i32_to_u64_try,
            .i32_to_u128_wrap,
            .i32_to_u128_try,
            .i32_to_f32,
            .i32_to_f64,
            .i32_to_dec,
            .u64_to_i8_wrap,
            .u64_to_i8_try,
            .u64_to_i16_wrap,
            .u64_to_i16_try,
            .u64_to_i32_wrap,
            .u64_to_i32_try,
            .u64_to_i64_wrap,
            .u64_to_i64_try,
            .u64_to_i128,
            .u64_to_u8_wrap,
            .u64_to_u8_try,
            .u64_to_u16_wrap,
            .u64_to_u16_try,
            .u64_to_u32_wrap,
            .u64_to_u32_try,
            .u64_to_u128,
            .u64_to_f32,
            .u64_to_f64,
            .u64_to_dec,
            .i64_to_i8_wrap,
            .i64_to_i8_try,
            .i64_to_i16_wrap,
            .i64_to_i16_try,
            .i64_to_i32_wrap,
            .i64_to_i32_try,
            .i64_to_i128,
            .i64_to_u8_wrap,
            .i64_to_u8_try,
            .i64_to_u16_wrap,
            .i64_to_u16_try,
            .i64_to_u32_wrap,
            .i64_to_u32_try,
            .i64_to_u64_wrap,
            .i64_to_u64_try,
            .i64_to_u128_wrap,
            .i64_to_u128_try,
            .i64_to_f32,
            .i64_to_f64,
            .i64_to_dec,
            .u128_to_i8_wrap,
            .u128_to_i8_try,
            .u128_to_i16_wrap,
            .u128_to_i16_try,
            .u128_to_i32_wrap,
            .u128_to_i32_try,
            .u128_to_i64_wrap,
            .u128_to_i64_try,
            .u128_to_i128_wrap,
            .u128_to_i128_try,
            .u128_to_u8_wrap,
            .u128_to_u8_try,
            .u128_to_u16_wrap,
            .u128_to_u16_try,
            .u128_to_u32_wrap,
            .u128_to_u32_try,
            .u128_to_u64_wrap,
            .u128_to_u64_try,
            .u128_to_f32,
            .u128_to_f64,
            .u128_to_dec_try_unsafe,
            .i128_to_i8_wrap,
            .i128_to_i8_try,
            .i128_to_i16_wrap,
            .i128_to_i16_try,
            .i128_to_i32_wrap,
            .i128_to_i32_try,
            .i128_to_i64_wrap,
            .i128_to_i64_try,
            .i128_to_u8_wrap,
            .i128_to_u8_try,
            .i128_to_u16_wrap,
            .i128_to_u16_try,
            .i128_to_u32_wrap,
            .i128_to_u32_try,
            .i128_to_u64_wrap,
            .i128_to_u64_try,
            .i128_to_u128_wrap,
            .i128_to_u128_try,
            .i128_to_f32,
            .i128_to_f64,
            .i128_to_dec_try_unsafe,
            .f32_to_i8_trunc,
            .f32_to_i8_try_unsafe,
            .f32_to_i16_trunc,
            .f32_to_i16_try_unsafe,
            .f32_to_i32_trunc,
            .f32_to_i32_try_unsafe,
            .f32_to_i64_trunc,
            .f32_to_i64_try_unsafe,
            .f32_to_i128_trunc,
            .f32_to_i128_try_unsafe,
            .f32_to_u8_trunc,
            .f32_to_u8_try_unsafe,
            .f32_to_u16_trunc,
            .f32_to_u16_try_unsafe,
            .f32_to_u32_trunc,
            .f32_to_u32_try_unsafe,
            .f32_to_u64_trunc,
            .f32_to_u64_try_unsafe,
            .f32_to_u128_trunc,
            .f32_to_u128_try_unsafe,
            .f32_to_f64,
            .f64_to_i8_trunc,
            .f64_to_i8_try_unsafe,
            .f64_to_i16_trunc,
            .f64_to_i16_try_unsafe,
            .f64_to_i32_trunc,
            .f64_to_i32_try_unsafe,
            .f64_to_i64_trunc,
            .f64_to_i64_try_unsafe,
            .f64_to_i128_trunc,
            .f64_to_i128_try_unsafe,
            .f64_to_u8_trunc,
            .f64_to_u8_try_unsafe,
            .f64_to_u16_trunc,
            .f64_to_u16_try_unsafe,
            .f64_to_u32_trunc,
            .f64_to_u32_try_unsafe,
            .f64_to_u64_trunc,
            .f64_to_u64_try_unsafe,
            .f64_to_u128_trunc,
            .f64_to_u128_try_unsafe,
            .f64_to_f32_wrap,
            .f64_to_f32_try_unsafe,
            .dec_to_i8_trunc,
            .dec_to_i8_try_unsafe,
            .dec_to_i16_trunc,
            .dec_to_i16_try_unsafe,
            .dec_to_i32_trunc,
            .dec_to_i32_try_unsafe,
            .dec_to_i64_trunc,
            .dec_to_i64_try_unsafe,
            .dec_to_i128_trunc,
            .dec_to_u8_trunc,
            .dec_to_u8_try_unsafe,
            .dec_to_u16_trunc,
            .dec_to_u16_try_unsafe,
            .dec_to_u32_trunc,
            .dec_to_u32_try_unsafe,
            .dec_to_u64_trunc,
            .dec_to_u64_try_unsafe,
            .dec_to_u128_trunc,
            .dec_to_u128_try_unsafe,
            .dec_to_f32_wrap,
            .dec_to_f32_try_unsafe,
            .dec_to_f64,
            => try self.emitNumericConversion(target, op, arg_locals),
        }
    }

    fn emitHasherLowLevel(self: *MonoLlvmCodeGen, target: LocalId, op: lir.LowLevel, args: anytype) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;

        const HasherOp = enum(u16) {
            dict_pseudo_seed = @intFromEnum(lir.LowLevel.dict_pseudo_seed),
            hasher_finish = @intFromEnum(lir.LowLevel.hasher_finish),
            hasher_write_bool = @intFromEnum(lir.LowLevel.hasher_write_bool),
            hasher_write_u8 = @intFromEnum(lir.LowLevel.hasher_write_u8),
            hasher_write_u16 = @intFromEnum(lir.LowLevel.hasher_write_u16),
            hasher_write_u32 = @intFromEnum(lir.LowLevel.hasher_write_u32),
            hasher_write_u64 = @intFromEnum(lir.LowLevel.hasher_write_u64),
            hasher_write_i8 = @intFromEnum(lir.LowLevel.hasher_write_i8),
            hasher_write_i16 = @intFromEnum(lir.LowLevel.hasher_write_i16),
            hasher_write_i32 = @intFromEnum(lir.LowLevel.hasher_write_i32),
            hasher_write_i64 = @intFromEnum(lir.LowLevel.hasher_write_i64),
            hasher_write_f32 = @intFromEnum(lir.LowLevel.hasher_write_f32),
            hasher_write_f64 = @intFromEnum(lir.LowLevel.hasher_write_f64),
            hasher_write_u128 = @intFromEnum(lir.LowLevel.hasher_write_u128),
            hasher_write_i128 = @intFromEnum(lir.LowLevel.hasher_write_i128),
            hasher_write_dec = @intFromEnum(lir.LowLevel.hasher_write_dec),
            hasher_write_bytes = @intFromEnum(lir.LowLevel.hasher_write_bytes),
            hasher_write_str = @intFromEnum(lir.LowLevel.hasher_write_str),
        };
        const hasher_op = narrowLowLevel(HasherOp, op) orelse return error.UnsupportedLowLevel;
        const result = switch (hasher_op) {
            .dict_pseudo_seed => blk: {
                if (args.len != 0) return error.CompilationFailed;
                break :blk try self.callBuiltin(builtinSymbol(LowLevelBuiltins.hasherOp(.dict_pseudo_seed)), .i64, &.{}, &.{});
            },
            .hasher_finish => blk: {
                if (args.len != 1) return error.CompilationFailed;
                const seed = try self.loadHasherState(GuardedList.at(args, 0));
                break :blk try self.callBuiltin(builtinSymbol(LowLevelBuiltins.hasherOp(.hasher_finish)), .i64, &.{.i64}, &.{seed});
            },
            .hasher_write_bool,
            .hasher_write_u8,
            .hasher_write_u16,
            .hasher_write_u32,
            .hasher_write_u64,
            .hasher_write_i8,
            .hasher_write_i16,
            .hasher_write_i32,
            .hasher_write_i64,
            => blk: {
                if (args.len != 2) return error.CompilationFailed;
                const seed = try self.loadHasherState(GuardedList.at(args, 0));
                const value_layout = self.localLayout(GuardedList.at(args, 1));
                const value = try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, value_layout), .i64, value_layout.isSigned());
                break :blk try self.callBuiltin(
                    LowLevelBuiltins.hasherOp(op).symbolName(),
                    .i64,
                    &.{ .i64, .i8, .i64, .i8 },
                    &.{
                        seed,
                        builder.intValue(.i8, @intFromEnum(lir.hasherDomain(op))) catch return error.OutOfMemory,
                        value,
                        builder.intValue(.i8, lir.hasherU64Width(op)) catch return error.OutOfMemory,
                    },
                );
            },
            .hasher_write_f32 => blk: {
                if (args.len != 2) return error.CompilationFailed;
                const seed = try self.loadHasherState(GuardedList.at(args, 0));
                const value = try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1)));
                const bits32 = wip.cast(.bitcast, value, .i32, "") catch return error.OutOfMemory;
                const bits64 = try self.coerceScalar(bits32, .i64, false);
                break :blk try self.callBuiltin(builtinSymbol(LowLevelBuiltins.hasherOp(.hasher_write_f32)), .i64, &.{ .i64, .i64 }, &.{ seed, bits64 });
            },
            .hasher_write_f64 => blk: {
                if (args.len != 2) return error.CompilationFailed;
                const seed = try self.loadHasherState(GuardedList.at(args, 0));
                const value = try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1)));
                const bits = wip.cast(.bitcast, value, .i64, "") catch return error.OutOfMemory;
                break :blk try self.callBuiltin(builtinSymbol(LowLevelBuiltins.hasherOp(.hasher_write_f64)), .i64, &.{ .i64, .i64 }, &.{ seed, bits });
            },
            .hasher_write_u128,
            .hasher_write_i128,
            .hasher_write_dec,
            => blk: {
                if (args.len != 2) return error.CompilationFailed;
                const seed = try self.loadHasherState(GuardedList.at(args, 0));
                const value = try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1)));
                const parts = try self.splitI128Value(value);
                break :blk try self.callBuiltin(
                    LowLevelBuiltins.hasherOp(op).symbolName(),
                    .i64,
                    &.{ .i64, .i8, .i64, .i64 },
                    &.{
                        seed,
                        builder.intValue(.i8, @intFromEnum(lir.hasherDomain(op))) catch return error.OutOfMemory,
                        parts.low,
                        parts.high,
                    },
                );
            },
            .hasher_write_bytes => blk: {
                if (args.len != 2) return error.CompilationFailed;
                const seed = try self.loadHasherState(GuardedList.at(args, 0));
                var list_args = try self.rocListArgs1(GuardedList.at(args, 1));
                defer list_args.deinit(self.allocator);
                break :blk try self.callBuiltin(
                    builtinSymbol(LowLevelBuiltins.hasherOp(.hasher_write_bytes)),
                    .i64,
                    &.{ .i64, .i8, try self.ptrType(), self.ptrSizedIntType() },
                    &.{
                        seed,
                        builder.intValue(.i8, @intFromEnum(lir.hasherDomain(op))) catch return error.OutOfMemory,
                        list_args.values.items[0],
                        list_args.values.items[1],
                    },
                );
            },
            .hasher_write_str => blk: {
                if (args.len != 2) return error.CompilationFailed;
                const seed = try self.loadHasherState(GuardedList.at(args, 0));
                var str_args = try self.rocStrArgs1(GuardedList.at(args, 1));
                defer str_args.deinit(self.allocator);
                break :blk try self.callBuiltin(
                    builtinSymbol(LowLevelBuiltins.hasherOp(.hasher_write_str)),
                    .i64,
                    &.{ .i64, try self.ptrType(), self.ptrSizedIntType(), self.ptrSizedIntType() },
                    &.{ seed, str_args.values.items[0], str_args.values.items[1], str_args.values.items[2] },
                );
            },
        };

        try self.storeHasherState(target, result);
    }

    fn emitCryptoLowLevel(self: *MonoLlvmCodeGen, target: LocalId, op: lir.LowLevel, args: anytype) Error!void {
        const Arity = enum { zero, one, two };
        const CryptoInfo = struct {
            name: []const u8,
            arity: Arity,
        };
        const CryptoOp = enum(u16) {
            crypto_sha256_hash_bytes = @intFromEnum(lir.LowLevel.crypto_sha256_hash_bytes),
            crypto_sha256_hasher_empty = @intFromEnum(lir.LowLevel.crypto_sha256_hasher_empty),
            crypto_sha256_hasher_write = @intFromEnum(lir.LowLevel.crypto_sha256_hasher_write),
            crypto_sha256_hasher_finish = @intFromEnum(lir.LowLevel.crypto_sha256_hasher_finish),
            crypto_blake3_hash_bytes = @intFromEnum(lir.LowLevel.crypto_blake3_hash_bytes),
            crypto_blake3_hasher_empty = @intFromEnum(lir.LowLevel.crypto_blake3_hasher_empty),
            crypto_blake3_hasher_write = @intFromEnum(lir.LowLevel.crypto_blake3_hasher_write),
            crypto_blake3_hasher_finish = @intFromEnum(lir.LowLevel.crypto_blake3_hasher_finish),
        };
        const crypto_op = narrowLowLevel(CryptoOp, op) orelse return error.UnsupportedLowLevel;
        const info: CryptoInfo = switch (crypto_op) {
            .crypto_sha256_hash_bytes => .{ .name = builtinSymbol(LowLevelBuiltins.cryptoOp(.crypto_sha256_hash_bytes)), .arity = Arity.one },
            .crypto_sha256_hasher_empty => .{ .name = builtinSymbol(LowLevelBuiltins.cryptoOp(.crypto_sha256_hasher_empty)), .arity = Arity.zero },
            .crypto_sha256_hasher_write => .{ .name = builtinSymbol(LowLevelBuiltins.cryptoOp(.crypto_sha256_hasher_write)), .arity = Arity.two },
            .crypto_sha256_hasher_finish => .{ .name = builtinSymbol(LowLevelBuiltins.cryptoOp(.crypto_sha256_hasher_finish)), .arity = Arity.one },
            .crypto_blake3_hash_bytes => .{ .name = builtinSymbol(LowLevelBuiltins.cryptoOp(.crypto_blake3_hash_bytes)), .arity = Arity.one },
            .crypto_blake3_hasher_empty => .{ .name = builtinSymbol(LowLevelBuiltins.cryptoOp(.crypto_blake3_hasher_empty)), .arity = Arity.zero },
            .crypto_blake3_hasher_write => .{ .name = builtinSymbol(LowLevelBuiltins.cryptoOp(.crypto_blake3_hasher_write)), .arity = Arity.two },
            .crypto_blake3_hasher_finish => .{ .name = builtinSymbol(LowLevelBuiltins.cryptoOp(.crypto_blake3_hasher_finish)), .arity = Arity.one },
        };

        var call_args = CallArgs.init();
        defer call_args.deinit(self.allocator);

        switch (info.arity) {
            .zero => {
                if (args.len != 0) return error.CompilationFailed;
            },
            .one => {
                if (args.len != 1) return error.CompilationFailed;
                call_args = try self.rocListArgs1(GuardedList.at(args, 0));
            },
            .two => {
                if (args.len != 2) return error.CompilationFailed;
                call_args = try self.rocListArgs1(GuardedList.at(args, 0));
                const rhs = try self.rocListArgs1(GuardedList.at(args, 1));
                defer {
                    var owned = rhs;
                    owned.deinit(self.allocator);
                }
                try call_args.types.appendSlice(self.allocator, rhs.types.items);
                try call_args.values.appendSlice(self.allocator, rhs.values.items);
            },
        }

        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid(info.name, call_args.types.items, call_args.values.items);
    }

    fn hasherStatePtr(self: *MonoLlvmCodeGen, local: LocalId) Error!LlvmBuilder.Value {
        const local_layout = self.localLayout(local);
        if (local_layout == .u64) return self.slot(local).ptr;

        const base = try self.resolveStructBase(local);
        const base_layout = self.layoutValue(base.layout_idx);
        if (base_layout.tag != .struct_) return error.CompilationFailed;

        const struct_idx = base_layout.getStruct().idx;
        const state_layout = self.layouts().getStructFieldLayoutByOriginalIndex(struct_idx, 0);
        if (state_layout != .u64) return error.CompilationFailed;

        const state_offset = self.layouts().getStructFieldOffsetByOriginalIndex(struct_idx, 0);
        return self.offsetPtr(base.ptr, state_offset);
    }

    fn loadHasherState(self: *MonoLlvmCodeGen, local: LocalId) Error!LlvmBuilder.Value {
        return self.loadScalar(try self.hasherStatePtr(local), .u64);
    }

    fn storeHasherState(self: *MonoLlvmCodeGen, local: LocalId, value: LlvmBuilder.Value) Error!void {
        try self.storeScalar(try self.hasherStatePtr(local), .u64, value);
    }

    fn emitNumericCompare(self: *MonoLlvmCodeGen, target: LocalId, op: lir.LowLevel, args: anytype) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const layout_idx = self.localLayout(GuardedList.at(args, 0));
        const lhs = try self.loadScalar(self.slot(GuardedList.at(args, 0)).ptr, layout_idx);
        const rhs = try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, layout_idx);
        const cond: LlvmBuilder.Value = if (isFloatLayout(layout_idx)) blk: {
            const cmp_cond: LlvmBuilder.FloatCondition = if (op == .num_is_gt)
                .ogt
            else if (op == .num_is_gte)
                .oge
            else if (op == .num_is_lt)
                .olt
            else if (op == .num_is_lte)
                .ole
            else
                unreachable;
            break :blk wip.fcmp(.normal, cmp_cond, lhs, rhs, "") catch return error.OutOfMemory;
        } else blk: {
            const signed = layout_idx.isSigned();
            const cmp_cond: LlvmBuilder.IntegerCondition = if (op == .num_is_gt)
                if (signed) .sgt else .ugt
            else if (op == .num_is_gte)
                if (signed) .sge else .uge
            else if (op == .num_is_lt)
                if (signed) .slt else .ult
            else if (op == .num_is_lte)
                if (signed) .sle else .ule
            else
                unreachable;
            break :blk wip.icmp(cmp_cond, lhs, rhs, "") catch return error.OutOfMemory;
        };
        try self.storeBool(self.slot(target).ptr, cond);
    }

    fn emitNumericOrderCompare(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const layout_idx = self.localLayout(GuardedList.at(args, 0));
        const lhs = try self.loadScalar(self.slot(GuardedList.at(args, 0)).ptr, layout_idx);
        const rhs = try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, layout_idx);
        const signed = layout_idx.isSigned();
        const gt_cond: LlvmBuilder.Value = wip.icmp(if (signed) .sgt else .ugt, lhs, rhs, "") catch return error.OutOfMemory;
        const lt_cond: LlvmBuilder.Value = wip.icmp(if (signed) .slt else .ult, lhs, rhs, "") catch return error.OutOfMemory;
        const gt = wip.conv(.unsigned, gt_cond, .i8, "") catch return error.OutOfMemory;
        const lt = wip.conv(.unsigned, lt_cond, .i8, "") catch return error.OutOfMemory;
        const lt_tag = wip.bin(.mul, lt, builder.intValue(.i8, 2) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const tag = wip.bin(.add, gt, lt_tag, "") catch return error.OutOfMemory;
        try self.storeIntToLayout(self.slot(target).ptr, tag, self.localLayout(target));
    }

    fn emitNumericBinary(self: *MonoLlvmCodeGen, target: LocalId, op: lir.LowLevel, args: anytype) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const target_layout = self.localLayout(target);

        if (CheckedArithmetic.classify(op)) |family_entry| {
            try self.emitIntegerFamilyBinary(target, family_entry, args);
            return;
        }

        if (op == .num_float_add or op == .num_float_sub or op == .num_float_mul) {
            const lhs_layout = self.localLayout(GuardedList.at(args, 0));
            var lhs = try self.loadScalar(self.slot(GuardedList.at(args, 0)).ptr, lhs_layout);
            var rhs = try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1)));
            const result_ty = self.scalarType(target_layout);
            lhs = try self.coerceScalar(lhs, result_ty, lhs_layout.isSigned());
            rhs = try self.coerceScalar(rhs, result_ty, self.localLayout(GuardedList.at(args, 1)).isSigned());
            const tag: LlvmBuilder.Function.Instruction.Tag = if (op == .num_float_add)
                .fadd
            else if (op == .num_float_sub)
                .fsub
            else
                .fmul;
            const result = wip.bin(tag, lhs, rhs, "") catch return error.OutOfMemory;
            try self.storeScalar(self.slot(target).ptr, target_layout, result);
            return;
        }

        if (op == .dec_mul) {
            try self.emitDecBinary(target, op, args);
            return;
        }

        const checked_op: ?lir.LowLevel = if (CheckedArithmetic.uncheckedOp(op) != null) op else null;
        const plain_op = CheckedArithmetic.uncheckedOp(op) orelse op;
        if (target_layout == .dec) {
            try self.emitDecBinary(target, plain_op, args);
            return;
        }
        const lhs_layout = self.localLayout(GuardedList.at(args, 0));
        var lhs = try self.loadScalar(self.slot(GuardedList.at(args, 0)).ptr, lhs_layout);
        var rhs = try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1)));
        const result_ty = self.scalarType(target_layout);
        lhs = try self.coerceScalar(lhs, result_ty, lhs_layout.isSigned());
        rhs = try self.coerceScalar(rhs, result_ty, self.localLayout(GuardedList.at(args, 1)).isSigned());
        const result = if (isFloatLayout(target_layout)) blk: {
            if (checked_op != null) return error.UnsupportedLowLevel;

            if (plain_op == .num_div_trunc_by) {
                const quotient = wip.bin(.fdiv, lhs, rhs, "") catch return error.OutOfMemory;
                break :blk wip.callIntrinsic(
                    .normal,
                    .none,
                    .trunc,
                    &.{result_ty},
                    &.{quotient},
                    "",
                ) catch return error.OutOfMemory;
            }

            const tag: LlvmBuilder.Function.Instruction.Tag = if (plain_op == .num_div_by)
                .fdiv
            else if (plain_op == .num_rem_by or plain_op == .num_mod_by)
                .frem
            else
                return error.UnsupportedLowLevel;
            break :blk wip.bin(tag, lhs, rhs, "") catch return error.OutOfMemory;
        } else blk: {
            if (plain_op == .num_shift_left_by or plain_op == .num_shift_right_by or plain_op == .num_shift_right_zf_by) {
                const result = try self.emitIntegerShift(plain_op, lhs, rhs, target_layout);
                try self.storeScalar(self.slot(target).ptr, target_layout, result);
                return;
            }

            const signed = target_layout.isSigned();
            if (checked_op) |checked| {
                if (plain_op == .num_div_by or plain_op == .num_div_trunc_by or plain_op == .num_rem_by or plain_op == .num_mod_by) {
                    rhs = try self.emitCheckedIntegerDenominator(checked, plain_op, lhs, rhs, target_layout);
                }
            }

            if (target_layout == .i128 or target_layout == .u128) {
                if (plain_op == .num_div_by or plain_op == .num_div_trunc_by or
                    plain_op == .num_rem_by or plain_op == .num_mod_by)
                {
                    break :blk try self.emitI128DivRem(plain_op, lhs, rhs, target_layout == .u128);
                }
            }

            const tag: LlvmBuilder.Function.Instruction.Tag = if (plain_op == .num_div_by or plain_op == .num_div_trunc_by)
                if (signed) .sdiv else .udiv
            else if (plain_op == .num_rem_by or plain_op == .num_mod_by)
                if (signed) .srem else .urem
            else if (plain_op == .num_bitwise_and)
                .@"and"
            else if (plain_op == .num_bitwise_or)
                .@"or"
            else if (plain_op == .num_bitwise_xor)
                .xor
            else
                return error.UnsupportedLowLevel;
            const raw = wip.bin(tag, lhs, rhs, "") catch return error.OutOfMemory;
            if (plain_op != .num_mod_by or !signed) break :blk raw;

            const zero = builder.zeroInitValue(result_ty) catch return error.OutOfMemory;
            const rem_is_zero = wip.icmp(.eq, raw, zero, "") catch return error.OutOfMemory;
            const rem_negative = wip.icmp(.slt, raw, zero, "") catch return error.OutOfMemory;
            const rhs_negative = wip.icmp(.slt, rhs, zero, "") catch return error.OutOfMemory;
            const sign_differs = wip.bin(.xor, rem_negative, rhs_negative, "") catch return error.OutOfMemory;
            const adjusted = wip.bin(.add, raw, rhs, "") catch return error.OutOfMemory;
            const adjusted_or_raw = wip.select(.normal, sign_differs, adjusted, raw, "") catch return error.OutOfMemory;
            break :blk wip.select(.normal, rem_is_zero, zero, adjusted_or_raw, "") catch return error.OutOfMemory;
        };
        try self.storeScalar(self.slot(target).ptr, target_layout, result);
    }

    const IntegerOverflowResult = struct {
        value: LlvmBuilder.Value,
        overflowed: LlvmBuilder.Value,
    };

    fn emitIntegerFamilyBinary(
        self: *MonoLlvmCodeGen,
        target: LocalId,
        entry: CheckedArithmetic.FamilyEntry,
        args: anytype,
    ) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const operand_layout = self.localLayout(GuardedList.at(args, 0));
        const rhs_layout = self.localLayout(GuardedList.at(args, 1));
        const result_ty = self.scalarType(operand_layout);
        var lhs = try self.loadScalar(self.slot(GuardedList.at(args, 0)).ptr, operand_layout);
        var rhs = try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, rhs_layout);
        lhs = try self.coerceScalar(lhs, result_ty, operand_layout.isSigned());
        rhs = try self.coerceScalar(rhs, result_ty, rhs_layout.isSigned());

        switch (entry.mode) {
            .wrap, .proven_cannot_overflow => {
                const tag = integerFamilyInstruction(entry.operation, entry.mode, operand_layout.isSigned());
                const result = wip.bin(tag, lhs, rhs, "") catch return error.OutOfMemory;
                try self.storeScalar(self.slot(target).ptr, operand_layout, result);
            },
            .crash_on_overflow, .overflows => {
                const overflow_result = try self.emitIntegerOverflowBinary(entry.operation, lhs, rhs, operand_layout);
                if (entry.mode == .crash_on_overflow) {
                    try self.emitCrashIf(overflow_result.overflowed, checkedOverflowMessage(entry.op));
                    try self.storeScalar(self.slot(target).ptr, operand_layout, overflow_result.value);
                } else {
                    try self.storeBool(self.slot(target).ptr, overflow_result.overflowed);
                }
            },
        }
    }

    fn integerFamilyInstruction(
        operation: CheckedArithmetic.Operation,
        mode: CheckedArithmetic.Mode,
        signed: bool,
    ) LlvmBuilder.Function.Instruction.Tag {
        return switch (mode) {
            .wrap => switch (operation) {
                .add => .add,
                .sub => .sub,
                .mul => .mul,
            },
            .proven_cannot_overflow => if (signed)
                switch (operation) {
                    .add => .@"add nsw",
                    .sub => .@"sub nsw",
                    .mul => .@"mul nsw",
                }
            else switch (operation) {
                .add => .@"add nuw",
                .sub => .@"sub nuw",
                .mul => .@"mul nuw",
            },
            .crash_on_overflow, .overflows => unreachable,
        };
    }

    fn emitIntegerOverflowBinary(
        self: *MonoLlvmCodeGen,
        operation: CheckedArithmetic.Operation,
        lhs: LlvmBuilder.Value,
        rhs: LlvmBuilder.Value,
        target_layout: layout.Idx,
    ) Error!IntegerOverflowResult {
        const wip = self.wip orelse return error.CompilationFailed;
        const result_ty = self.scalarType(target_layout);
        if (operation == .mul and (target_layout == .i128 or target_layout == .u128)) {
            return self.callI128MulOverflowBuiltin(lhs, rhs, target_layout == .u128);
        }
        const intrinsic: LlvmBuilder.Intrinsic = switch (operation) {
            .add => if (target_layout.isSigned()) .@"sadd.with.overflow" else .@"uadd.with.overflow",
            .sub => if (target_layout.isSigned()) .@"ssub.with.overflow" else .@"usub.with.overflow",
            .mul => if (target_layout.isSigned()) .@"smul.with.overflow" else .@"umul.with.overflow",
        };
        const overflow_result = wip.callIntrinsic(
            .normal,
            .none,
            intrinsic,
            &.{result_ty},
            &.{ lhs, rhs },
            "",
        ) catch return error.OutOfMemory;
        const result = wip.extractValue(overflow_result, &.{0}, "") catch return error.OutOfMemory;
        const overflowed = wip.extractValue(overflow_result, &.{1}, "") catch return error.OutOfMemory;
        return .{ .value = result, .overflowed = overflowed };
    }

    fn emitCheckedIntegerDenominator(
        self: *MonoLlvmCodeGen,
        checked_op: lir.LowLevel,
        plain_op: lir.LowLevel,
        lhs: LlvmBuilder.Value,
        rhs: LlvmBuilder.Value,
        target_layout: layout.Idx,
    ) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const result_ty = self.scalarType(target_layout);
        const zero = builder.zeroInitValue(result_ty) catch return error.OutOfMemory;
        const one = builder.intValue(result_ty, 1) catch return error.OutOfMemory;

        const rhs_is_zero = wip.icmp(.eq, rhs, zero, "") catch return error.OutOfMemory;
        try self.emitCrashIf(rhs_is_zero, checkedZeroDenominatorMessage(checked_op, target_layout));
        var safe_rhs = wip.select(.normal, rhs_is_zero, one, rhs, "") catch return error.OutOfMemory;

        if (target_layout.isSigned()) {
            const lowest = builder.intValue(result_ty, CheckedArithmetic.signedLowestValue(target_layout) orelse unreachable) catch return error.OutOfMemory;
            const neg_one = builder.intValue(result_ty, -1) catch return error.OutOfMemory;
            const lhs_is_lowest = wip.icmp(.eq, lhs, lowest, "") catch return error.OutOfMemory;
            const rhs_is_neg_one = wip.icmp(.eq, rhs, neg_one, "") catch return error.OutOfMemory;
            const min_div_neg_one = wip.bin(.@"and", lhs_is_lowest, rhs_is_neg_one, "") catch return error.OutOfMemory;
            if (plain_op == .num_div_by or plain_op == .num_div_trunc_by) {
                try self.emitCrashIf(min_div_neg_one, checkedOverflowMessage(checked_op));
            } else if (plain_op != .num_rem_by and plain_op != .num_mod_by) {
                unreachable;
            }
            safe_rhs = wip.select(.normal, min_div_neg_one, one, safe_rhs, "") catch return error.OutOfMemory;
        }

        return safe_rhs;
    }

    fn callI128MulOverflowBuiltin(
        self: *MonoLlvmCodeGen,
        lhs: LlvmBuilder.Value,
        rhs: LlvmBuilder.Value,
        unsigned: bool,
    ) Error!IntegerOverflowResult {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const out_low = try self.allocEntryBlockSlot(.i64, 1, LlvmBuilder.Alignment.fromByteUnits(8), "mul_low");
        const out_high = try self.allocEntryBlockSlot(.i64, 1, LlvmBuilder.Alignment.fromByteUnits(8), "mul_high");
        const lhs_parts = try self.splitI128Value(lhs);
        const rhs_parts = try self.splitI128Value(rhs);
        const overflowed_i32 = try self.callBuiltin(
            LowLevelBuiltins.checkedMul128(unsigned).symbolName(),
            .i32,
            &.{ try self.ptrType(), try self.ptrType(), .i64, .i64, .i64, .i64 },
            &.{ out_low, out_high, lhs_parts.low, lhs_parts.high, rhs_parts.low, rhs_parts.high },
        );
        const overflowed = wip.icmp(.ne, overflowed_i32, builder.intValue(.i32, 0) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const low = wip.load(.normal, .i64, out_low, LlvmBuilder.Alignment.fromByteUnits(8), "") catch return error.OutOfMemory;
        const high = wip.load(.normal, .i64, out_high, LlvmBuilder.Alignment.fromByteUnits(8), "") catch return error.OutOfMemory;
        return .{ .value = try self.combineI128Parts(low, high), .overflowed = overflowed };
    }

    /// 128-bit division, remainder and modulo, routed through the same
    /// decomposed-to-64-bit builtins the dev and wasm backends call.
    ///
    /// No target Roc compiles to has a 128-bit divide instruction, so leaving a
    /// `sdiv`/`udiv`/`srem`/`urem` on i128 in the module makes instruction
    /// selection lower it to a compiler-rt libcall (`__divti3`, `__udivti3`,
    /// ...). Nothing in a Roc object defines those, and a platform host is not
    /// required to carry compiler-rt, so such an object fails to link: the
    /// Windows test hosts, which bundle no compiler-rt, reject it outright.
    fn emitI128DivRem(
        self: *MonoLlvmCodeGen,
        plain_op: lir.LowLevel,
        lhs: LlvmBuilder.Value,
        rhs: LlvmBuilder.Value,
        unsigned: bool,
    ) Error!LlvmBuilder.Value {
        const builtin_fn = if (plain_op == .num_div_by or plain_op == .num_div_trunc_by)
            LowLevelBuiltins.i128DivRem(false, unsigned)
        else if (plain_op == .num_rem_by)
            LowLevelBuiltins.i128DivRem(true, unsigned)
        else if (plain_op == .num_mod_by)
            LowLevelBuiltins.i128Mod(unsigned)
        else
            return error.UnsupportedLowLevel;
        return self.callI128BinaryBuiltin(builtin_fn.symbolName(), lhs, rhs, true);
    }

    fn emitIntegerShift(self: *MonoLlvmCodeGen, op: lir.LowLevel, lhs: LlvmBuilder.Value, rhs: LlvmBuilder.Value, target_layout: layout.Idx) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const result_ty = self.scalarType(target_layout);
        const width = self.intBits(target_layout);
        const rhs_u8 = try self.coerceScalar(rhs, .i8, false);
        const amount = try self.coerceScalar(rhs_u8, result_ty, false);
        // The shift count is taken modulo the bit width. The widths are all
        // powers of two, so that is a bitwise AND with (width - 1). Masking also
        // keeps LLVM's shl/lshr/ashr out of poison territory (their result is
        // poison when the count is >= the bit width).
        const mask = builder.intValue(result_ty, width - 1) catch return error.OutOfMemory;
        const masked_amount = wip.bin(.@"and", amount, mask, "") catch return error.OutOfMemory;
        const tag: LlvmBuilder.Function.Instruction.Tag = if (op == .num_shift_left_by)
            .shl
        else if (op == .num_shift_right_by)
            if (target_layout.isSigned()) .ashr else .lshr
        else if (op == .num_shift_right_zf_by)
            .lshr
        else
            unreachable;
        return wip.bin(tag, lhs, masked_amount, "") catch return error.OutOfMemory;
    }

    fn emitDecBinary(self: *MonoLlvmCodeGen, target: LocalId, op: lir.LowLevel, args: anytype) Error!void {
        const lhs = try self.loadScalar(self.slot(GuardedList.at(args, 0)).ptr, .dec);
        const rhs = try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, .dec);
        const result = if (op == .dec_mul)
            // Dec multiplication rescales its i128 payload and uses its dedicated
            // builtin overflow path; it is not an integer-family multiplication.
            try self.callI128BinaryBuiltin(builtinSymbol(LowLevelBuiltins.decBinaryArith(.dec_mul)), lhs, rhs, true)
        else if (op == .num_div_by)
            try self.callI128BinaryBuiltin(builtinSymbol(LowLevelBuiltins.decBinaryArith(.num_div_by)), lhs, rhs, true)
        else if (op == .num_div_trunc_by)
            try self.callI128BinaryBuiltin(builtinSymbol(LowLevelBuiltins.decBinaryArith(.num_div_trunc_by)), lhs, rhs, true)
            // A Dec's payload is a scaled i128; the truncating remainder and the
            // modulo of the raw payloads are the Dec remainder and modulo, so
            // these route through the same i128 wrappers the dev/wasm backends
            // use. Both wrappers take `roc_ops`.
        else if (op == .num_rem_by)
            try self.callI128BinaryBuiltin(builtinSymbol(LowLevelBuiltins.i128DivRem(true, false)), lhs, rhs, true)
        else if (op == .num_mod_by)
            try self.callI128BinaryBuiltin(builtinSymbol(LowLevelBuiltins.i128Mod(false)), lhs, rhs, true)
        else
            return error.UnsupportedLowLevel;
        try self.storeScalar(self.slot(target).ptr, .dec, result);
    }

    /// Call a builtin that takes two 128-bit operands and writes a 128-bit
    /// result, passing each operand as a 64-bit low/high pair. Dec's payload is
    /// an i128, so its arithmetic wrappers share this shape with the plain i128
    /// ones.
    fn callI128BinaryBuiltin(self: *MonoLlvmCodeGen, name: []const u8, lhs: LlvmBuilder.Value, rhs: LlvmBuilder.Value, pass_roc_ops: bool) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const out_low = try self.allocEntryBlockSlot(.i64, 1, LlvmBuilder.Alignment.fromByteUnits(8), "dec_low");
        const out_high = try self.allocEntryBlockSlot(.i64, 1, LlvmBuilder.Alignment.fromByteUnits(8), "dec_high");
        const lhs_parts = try self.splitI128Value(lhs);
        const rhs_parts = try self.splitI128Value(rhs);
        if (pass_roc_ops) {
            try self.callBuiltinVoid(
                name,
                &.{ try self.ptrType(), try self.ptrType(), .i64, .i64, .i64, .i64, try self.ptrType() },
                &.{ out_low, out_high, lhs_parts.low, lhs_parts.high, rhs_parts.low, rhs_parts.high, self.rocOps() },
            );
        } else {
            try self.callBuiltinVoid(
                name,
                &.{ try self.ptrType(), try self.ptrType(), .i64, .i64, .i64, .i64 },
                &.{ out_low, out_high, lhs_parts.low, lhs_parts.high, rhs_parts.low, rhs_parts.high },
            );
        }

        const low = wip.load(.normal, .i64, out_low, LlvmBuilder.Alignment.fromByteUnits(8), "") catch return error.OutOfMemory;
        const high = wip.load(.normal, .i64, out_high, LlvmBuilder.Alignment.fromByteUnits(8), "") catch return error.OutOfMemory;
        return self.combineI128Parts(low, high);
    }

    fn callDecUnaryBuiltin(self: *MonoLlvmCodeGen, name: []const u8, value: LlvmBuilder.Value) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const out_low = try self.allocEntryBlockSlot(.i64, 1, LlvmBuilder.Alignment.fromByteUnits(8), "dec_low");
        const out_high = try self.allocEntryBlockSlot(.i64, 1, LlvmBuilder.Alignment.fromByteUnits(8), "dec_high");
        const parts = try self.splitI128Value(value);
        try self.callBuiltinVoid(
            name,
            &.{ try self.ptrType(), try self.ptrType(), .i64, .i64, try self.ptrType() },
            &.{ out_low, out_high, parts.low, parts.high, self.rocOps() },
        );

        const low = wip.load(.normal, .i64, out_low, LlvmBuilder.Alignment.fromByteUnits(8), "") catch return error.OutOfMemory;
        const high = wip.load(.normal, .i64, out_high, LlvmBuilder.Alignment.fromByteUnits(8), "") catch return error.OutOfMemory;
        return self.combineI128Parts(low, high);
    }

    const I128Parts = struct {
        low: LlvmBuilder.Value,
        high: LlvmBuilder.Value,
    };

    fn splitI128Value(self: *MonoLlvmCodeGen, value: LlvmBuilder.Value) Error!I128Parts {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const high = wip.bin(.lshr, value, builder.intValue(.i128, 64) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        return .{
            .low = try self.coerceScalar(value, .i64, false),
            .high = try self.coerceScalar(high, .i64, false),
        };
    }

    fn combineI128Parts(self: *MonoLlvmCodeGen, low: LlvmBuilder.Value, high: LlvmBuilder.Value) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const low128 = try self.coerceScalar(low, .i128, false);
        const high128 = try self.coerceScalar(high, .i128, false);
        const shifted_high = wip.bin(.shl, high128, builder.intValue(.i128, 64) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        return wip.bin(.@"or", shifted_high, low128, "") catch return error.OutOfMemory;
    }

    fn emitNumericNegate(self: *MonoLlvmCodeGen, target: LocalId, op: lir.LowLevel, arg: LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const target_layout = self.localLayout(target);
        const checked_op: ?lir.LowLevel = if (CheckedArithmetic.uncheckedOp(op) != null) op else null;
        const value = try self.loadScalar(self.slot(arg).ptr, self.localLayout(arg));
        const result = if (isFloatLayout(target_layout)) blk: {
            if (checked_op != null) return error.UnsupportedLowLevel;
            break :blk wip.un(.fneg, value, "") catch return error.OutOfMemory;
        } else blk: {
            if (checked_op) |checked| {
                const zero = builder.zeroInitValue(value.typeOfWip(wip)) catch return error.OutOfMemory;
                const overflow_result = wip.callIntrinsic(
                    .normal,
                    .none,
                    .@"ssub.with.overflow",
                    &.{value.typeOfWip(wip)},
                    &.{ zero, value },
                    "",
                ) catch return error.OutOfMemory;
                const negated = wip.extractValue(overflow_result, &.{0}, "") catch return error.OutOfMemory;
                const overflowed = wip.extractValue(overflow_result, &.{1}, "") catch return error.OutOfMemory;
                try self.emitCrashIf(overflowed, checkedOverflowMessage(checked));
                break :blk negated;
            }
            break :blk wip.neg(value, "") catch return error.OutOfMemory;
        };
        try self.storeScalar(self.slot(target).ptr, target_layout, result);
    }

    fn emitNumericBitwiseNot(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const target_layout = self.localLayout(target);
        const value = try self.loadScalar(self.slot(arg).ptr, self.localLayout(arg));
        const result = wip.not(value, "") catch return error.OutOfMemory;
        try self.storeScalar(self.slot(target).ptr, target_layout, result);
    }

    /// Lower a bit-counting op (`count_one_bits`/`count_leading_zero_bits`/
    /// `count_trailing_zero_bits`) to the corresponding LLVM intrinsic on the
    /// operand's integer type (i8..i128; LLVM legalizes i128 itself). The
    /// ctlz/cttz `is_zero_poison` flag is FALSE so a zero operand yields the
    /// bit width, matching the spec. The intrinsic result has the operand's
    /// width; `storeScalar` truncates it to the declared U8 return.
    fn emitNumericBitCount(self: *MonoLlvmCodeGen, target: LocalId, op: lir.LowLevel, arg: LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const target_layout = self.localLayout(target);
        const value = try self.loadScalar(self.slot(arg).ptr, self.localLayout(arg));
        const value_ty = value.typeOfWip(wip);
        const zero_is_not_poison = builder.intValue(.i1, 0) catch return error.OutOfMemory;
        const result = if (op == .num_count_one_bits)
            wip.callIntrinsic(
                .normal,
                .none,
                .ctpop,
                &.{value_ty},
                &.{value},
                "",
            ) catch return error.OutOfMemory
        else if (op == .num_count_leading_zero_bits)
            wip.callIntrinsic(
                .normal,
                .none,
                .ctlz,
                &.{value_ty},
                &.{ value, zero_is_not_poison },
                "",
            ) catch return error.OutOfMemory
        else if (op == .num_count_trailing_zero_bits)
            wip.callIntrinsic(
                .normal,
                .none,
                .cttz,
                &.{value_ty},
                &.{ value, zero_is_not_poison },
                "",
            ) catch return error.OutOfMemory
        else
            unreachable;
        try self.storeScalar(self.slot(target).ptr, target_layout, result);
    }

    /// Whether the target machine has an x86 CPU feature.
    ///
    /// SIMD lowerings ask this before reaching for an instruction that is not
    /// in the x86-64 baseline, so a `v1` target takes the architecture-neutral
    /// path instead of emitting an instruction its CPUs do not have. The
    /// resolved feature set is the same one LLVM selects instructions from, so
    /// this cannot disagree with what the target machine will accept.
    fn hasX86Feature(self: *MonoLlvmCodeGen, feature: std.Target.x86.Feature) bool {
        return std.Target.x86.featureSetHas(self.target.cpu.features, feature);
    }

    /// Whether the target machine has an aarch64 CPU feature. See `hasX86Feature`.
    fn hasAarch64Feature(self: *MonoLlvmCodeGen, feature: std.Target.aarch64.Feature) bool {
        return std.Target.aarch64.featureSetHas(self.target.cpu.features, feature);
    }

    /// Whether this target lowers SIMD through x86 vector instructions.
    fn isX86Simd(self: *MonoLlvmCodeGen) bool {
        return self.target.cpu.arch == .x86_64;
    }

    /// Whether this target lowers SIMD through NEON.
    ///
    /// NEON is mandatory in Armv8.0-A, so every aarch64 target has it,
    /// including `v1`.
    fn isAarch64Simd(self: *MonoLlvmCodeGen) bool {
        return self.target.cpu.arch == .aarch64 or self.target.cpu.arch == .aarch64_be;
    }

    fn simdVectorForLayout(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) ?layout.Vector {
        const value_layout = self.layoutValue(layout_idx);
        if (value_layout.tag != .scalar or value_layout.getScalar().tag != .vector) return null;
        return value_layout.getScalar().getVector();
    }

    fn simdType(self: *MonoLlvmCodeGen, vector: layout.Vector) Error!LlvmBuilder.Type {
        return vectorLlvmType(self.builder orelse return error.CompilationFailed, vector);
    }

    fn loadSimdLocal(self: *MonoLlvmCodeGen, local: LocalId) Error!LlvmBuilder.Value {
        const vector = self.simdVectorForLayout(self.localLayout(local)) orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        return wip.load(.normal, try self.simdType(vector), self.slot(local).ptr, self.alignmentForLayout(self.localLayout(local)), "") catch return error.OutOfMemory;
    }

    fn storeSimdLocal(self: *MonoLlvmCodeGen, local: LocalId, value: LlvmBuilder.Value) Error!void {
        const vector = self.simdVectorForLayout(self.localLayout(local)) orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const vector_ty = try self.simdType(vector);
        const stored = if (value.typeOfWip(wip) == vector_ty)
            value
        else
            wip.cast(.bitcast, value, vector_ty, "") catch return error.OutOfMemory;
        _ = wip.store(.normal, stored, self.slot(local).ptr, self.alignmentForLayout(self.localLayout(local))) catch return error.OutOfMemory;
    }

    fn simdShuffleMask(self: *MonoLlvmCodeGen, indices: []const u32) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        var constants: [16]LlvmBuilder.Constant = undefined;
        if (indices.len > constants.len) return error.CompilationFailed;
        for (indices, 0..) |index, i| constants[i] = builder.intConst(.i32, index) catch return error.OutOfMemory;
        const ty = builder.vectorType(.normal, @intCast(indices.len), .i32) catch return error.OutOfMemory;
        return builder.vectorValue(ty, constants[0..indices.len]) catch return error.OutOfMemory;
    }

    /// Read a little-endian integer straight out of a byte list. The result
    /// layout supplies the width, and the caller has already proven the bytes
    /// are in range, so this is one unaligned load. On a big-endian target the
    /// load reads the bytes the other way around, so it is byte-swapped back:
    /// `from_le_bytes` means little-endian everywhere, not host-endian.
    fn emitNumFromLeBytes(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const target_layout = self.localLayout(target);
        const value_ty = self.scalarType(target_layout);
        const bytes = try self.loadPointer(self.slot(GuardedList.at(args, 0)).ptr);
        const index = try self.loadIntegerLocalAsUsize(GuardedList.at(args, 1));
        const source = wip.gep(.inbounds, .i8, bytes, &.{index}, "") catch return error.OutOfMemory;
        var value = wip.load(.normal, value_ty, source, LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
        if (self.target.cpu.arch.endian() == .big) {
            value = wip.callIntrinsic(.normal, .none, .bswap, &.{value_ty}, &.{value}, "") catch return error.OutOfMemory;
        }
        try self.storeScalar(self.slot(target).ptr, target_layout, value);
    }

    fn emitSimdLoad(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const vector = self.simdVectorForLayout(self.localLayout(target)) orelse return error.CompilationFailed;
        const bytes = try self.loadPointer(self.slot(GuardedList.at(args, 0)).ptr);
        const index = try self.loadIntegerLocalAsUsize(GuardedList.at(args, 1));
        const source = wip.gep(.inbounds, .i8, bytes, &.{index}, "") catch return error.OutOfMemory;
        const value = wip.load(.normal, try self.simdType(vector), source, LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
        try self.storeSimdLocal(target, value);
    }

    fn emitSimdStore(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, unique_args: u64) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const vector = try self.loadSimdLocal(GuardedList.at(args, 0));
        const bits = wip.cast(.bitcast, vector, .i128, "") catch return error.OutOfMemory;
        const parts = try self.splitI128Value(bits);
        const list = try self.rocListArgFields(self.slot(GuardedList.at(args, 1)).ptr);
        const index = try self.coerceScalar(
            try self.loadScalar(self.slot(GuardedList.at(args, 2)).ptr, self.localLayout(GuardedList.at(args, 2))),
            .i64,
            false,
        );
        const mode = if ((unique_args & (@as(u64, 1) << 1)) != 0) builtins.utils.UpdateMode.InPlace else builtins.utils.UpdateMode.Immutable;
        try self.callBuiltinVoid(
            builtinSymbol(.simd_store_16),
            &.{ try self.ptrType(), .i64, .i64, try self.ptrType(), self.ptrSizedIntType(), self.ptrSizedIntType(), .i64, .i8, try self.ptrType() },
            &.{ self.slot(target).ptr, parts.low, parts.high, list[0], list[1], list[2], index, builder.intValue(.i8, @intFromEnum(mode)) catch return error.OutOfMemory, self.rocOps() },
        );
    }

    fn emitSimdAppend(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, unique_args: u64) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const vector = try self.loadSimdLocal(GuardedList.at(args, 0));
        const bits = wip.cast(.bitcast, vector, .i128, "") catch return error.OutOfMemory;
        const parts = try self.splitI128Value(bits);
        const list = try self.rocListArgFields(self.slot(GuardedList.at(args, 1)).ptr);
        const mode = if ((unique_args & (@as(u64, 1) << 1)) != 0) builtins.utils.UpdateMode.InPlace else builtins.utils.UpdateMode.Immutable;
        try self.callBuiltinVoid(
            builtinSymbol(.simd_append_16),
            &.{ try self.ptrType(), .i64, .i64, try self.ptrType(), self.ptrSizedIntType(), self.ptrSizedIntType(), .i8, try self.ptrType() },
            &.{ self.slot(target).ptr, parts.low, parts.high, list[0], list[1], list[2], builder.intValue(.i8, @intFromEnum(mode)) catch return error.OutOfMemory, self.rocOps() },
        );
    }

    fn simdRawType(self: *MonoLlvmCodeGen, lane_bits: u16, lane_count: u16) Error!LlvmBuilder.Type {
        const builder = self.builder orelse return error.CompilationFailed;
        return builder.vectorType(.normal, lane_count, builder.intType(lane_bits) catch return error.OutOfMemory) catch return error.OutOfMemory;
    }

    fn simdRawSplat(self: *MonoLlvmCodeGen, lane_bits: u16, lane_count: u16, value: i128) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const lane_ty = builder.intType(lane_bits) catch return error.OutOfMemory;
        return builder.splatValue(try self.simdRawType(lane_bits, lane_count), builder.intConst(lane_ty, value) catch return error.OutOfMemory) catch return error.OutOfMemory;
    }

    fn simdExtendVector(self: *MonoLlvmCodeGen, value: LlvmBuilder.Value, lane_bits: u16, lane_count: u16, signed: bool) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        return wip.cast(if (signed) .sext else .zext, value, try self.simdRawType(lane_bits, lane_count), "") catch return error.OutOfMemory;
    }

    fn simdClampVector(
        self: *MonoLlvmCodeGen,
        value: LlvmBuilder.Value,
        lane_bits: u16,
        lane_count: u16,
        lower: i128,
        upper: i128,
        signed_compare: bool,
    ) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const low = try self.simdRawSplat(lane_bits, lane_count, lower);
        const high = try self.simdRawSplat(lane_bits, lane_count, upper);
        const below = wip.icmp(if (signed_compare) .slt else .ult, value, low, "") catch return error.OutOfMemory;
        const above = wip.icmp(if (signed_compare) .sgt else .ugt, value, high, "") catch return error.OutOfMemory;
        const at_least_low = wip.select(.normal, below, low, value, "") catch return error.OutOfMemory;
        return wip.select(.normal, above, high, at_least_low, "") catch return error.OutOfMemory;
    }

    fn emitSimdLowLevel(self: *MonoLlvmCodeGen, target: LocalId, op: lir.LowLevel, args: anytype) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const target_layout = self.localLayout(target);
        const destination_vector = self.simdVectorForLayout(target_layout);
        var source_vector: ?layout.Vector = null;
        for (0..args.len) |i| {
            source_vector = self.simdVectorForLayout(self.localLayout(GuardedList.at(args, i))) orelse continue;
            break;
        }
        const vector = source_vector orelse destination_vector orelse return error.CompilationFailed;
        const vector_ty = try self.simdType(vector);
        const lane_ty = builder.intType(vector.laneBits()) catch return error.OutOfMemory;

        const simd_op = narrowLowLevel(SimdLowLevel, op) orelse return error.UnsupportedLowLevel;
        switch (simd_op) {
            .simd_splat => {
                const arg = GuardedList.at(args, 0);
                const scalar = try self.coerceScalar(try self.loadScalar(self.slot(arg).ptr, self.localLayout(arg)), lane_ty, self.localLayout(arg).isSigned());
                try self.storeSimdLocal(target, wip.splatVector(try self.simdType(destination_vector.?), scalar, "") catch return error.OutOfMemory);
            },
            .simd_get_lane_unchecked => {
                const value = try self.loadSimdLocal(GuardedList.at(args, 0));
                const index_arg = GuardedList.at(args, 1);
                const index = try self.coerceScalar(try self.loadScalar(self.slot(index_arg).ptr, self.localLayout(index_arg)), .i32, false);
                const lane = wip.extractElement(value, index, "") catch return error.OutOfMemory;
                try self.storeScalar(self.slot(target).ptr, target_layout, lane);
            },
            .simd_with_lane_unchecked => {
                const value = try self.loadSimdLocal(GuardedList.at(args, 0));
                const index_arg = GuardedList.at(args, 1);
                const lane_arg = GuardedList.at(args, 2);
                const index = try self.coerceScalar(try self.loadScalar(self.slot(index_arg).ptr, self.localLayout(index_arg)), .i32, false);
                const lane = try self.coerceScalar(try self.loadScalar(self.slot(lane_arg).ptr, self.localLayout(lane_arg)), lane_ty, self.localLayout(lane_arg).isSigned());
                try self.storeSimdLocal(target, wip.insertElement(value, lane, index, "") catch return error.OutOfMemory);
            },
            .simd_to_u128_bits => {
                const value = try self.loadSimdLocal(GuardedList.at(args, 0));
                try self.storeScalar(self.slot(target).ptr, target_layout, wip.cast(.bitcast, value, .i128, "") catch return error.OutOfMemory);
            },
            .simd_from_u128_bits => {
                const arg = GuardedList.at(args, 0);
                const bits = try self.loadScalar(self.slot(arg).ptr, self.localLayout(arg));
                try self.storeSimdLocal(target, wip.cast(.bitcast, bits, try self.simdType(destination_vector.?), "") catch return error.OutOfMemory);
            },
            .simd_add_wrap, .simd_sub_wrap, .simd_mul_wrap, .simd_and, .simd_or, .simd_xor => {
                const lhs = try self.loadSimdLocal(GuardedList.at(args, 0));
                const rhs = try self.loadSimdLocal(GuardedList.at(args, 1));
                const tag: LlvmBuilder.Function.Instruction.Tag = if (op == .simd_add_wrap)
                    .add
                else if (op == .simd_sub_wrap)
                    .sub
                else if (op == .simd_mul_wrap)
                    .mul
                else if (op == .simd_and)
                    .@"and"
                else if (op == .simd_or)
                    .@"or"
                else if (op == .simd_xor)
                    .xor
                else
                    unreachable;
                try self.storeSimdLocal(target, wip.bin(tag, lhs, rhs, "") catch return error.OutOfMemory);
            },
            .simd_add_sat, .simd_sub_sat => {
                const lhs = try self.loadSimdLocal(GuardedList.at(args, 0));
                const rhs = try self.loadSimdLocal(GuardedList.at(args, 1));
                const intrinsic: LlvmBuilder.Intrinsic = if (op == .simd_add_sat)
                    (if (vector.isSigned()) .@"sadd.sat" else .@"uadd.sat")
                else if (vector.isSigned())
                    .@"ssub.sat"
                else
                    .@"usub.sat";
                const result = wip.callIntrinsic(.normal, .none, intrinsic, &.{vector_ty}, &.{ lhs, rhs }, "") catch return error.OutOfMemory;
                try self.storeSimdLocal(target, result);
            },
            .simd_neg_wrap => {
                try self.storeSimdLocal(target, wip.neg(try self.loadSimdLocal(GuardedList.at(args, 0)), "") catch return error.OutOfMemory);
            },
            .simd_not => {
                try self.storeSimdLocal(target, wip.not(try self.loadSimdLocal(GuardedList.at(args, 0)), "") catch return error.OutOfMemory);
            },
            .simd_abs_wrap => {
                const value = try self.loadSimdLocal(GuardedList.at(args, 0));
                const zero = builder.zeroInitValue(vector_ty) catch return error.OutOfMemory;
                const negative = wip.icmp(.slt, value, zero, "") catch return error.OutOfMemory;
                const negated = wip.neg(value, "") catch return error.OutOfMemory;
                try self.storeSimdLocal(target, wip.select(.normal, negative, negated, value, "") catch return error.OutOfMemory);
            },
            .simd_min, .simd_max => {
                const lhs = try self.loadSimdLocal(GuardedList.at(args, 0));
                const rhs = try self.loadSimdLocal(GuardedList.at(args, 1));
                const condition: LlvmBuilder.IntegerCondition = if (op == .simd_min)
                    (if (vector.isSigned()) .slt else .ult)
                else if (vector.isSigned())
                    .sgt
                else
                    .ugt;
                const choose_lhs = wip.icmp(condition, lhs, rhs, "") catch return error.OutOfMemory;
                try self.storeSimdLocal(target, wip.select(.normal, choose_lhs, lhs, rhs, "") catch return error.OutOfMemory);
            },
            .simd_abs_diff => {
                const lhs = try self.loadSimdLocal(GuardedList.at(args, 0));
                const rhs = try self.loadSimdLocal(GuardedList.at(args, 1));
                const lhs_ge = wip.icmp(.uge, lhs, rhs, "") catch return error.OutOfMemory;
                const forward = wip.bin(.sub, lhs, rhs, "") catch return error.OutOfMemory;
                const reverse = wip.bin(.sub, rhs, lhs, "") catch return error.OutOfMemory;
                try self.storeSimdLocal(target, wip.select(.normal, lhs_ge, forward, reverse, "") catch return error.OutOfMemory);
            },
            .simd_avg_rounded => {
                // PAVGB/PAVGW are SSE2, available at every x86 CPU level.
                if (self.isX86Simd()) {
                    const lhs = try self.loadSimdLocal(GuardedList.at(args, 0));
                    const rhs = try self.loadSimdLocal(GuardedList.at(args, 1));
                    const name = if (vector.laneBits() == 8) "llvm.x86.sse2.pavg.b" else "llvm.x86.sse2.pavg.w";
                    try self.storeSimdLocal(target, try self.callBuiltin(name, vector_ty, &.{ vector_ty, vector_ty }, &.{ lhs, rhs }));
                    return;
                }
                const wide_bits: u16 = vector.laneBits() * 2;
                const lhs = try self.simdExtendVector(try self.loadSimdLocal(GuardedList.at(args, 0)), wide_bits, vector.laneCount(), false);
                const rhs = try self.simdExtendVector(try self.loadSimdLocal(GuardedList.at(args, 1)), wide_bits, vector.laneCount(), false);
                const sum = wip.bin(.add, lhs, rhs, "") catch return error.OutOfMemory;
                const biased = wip.bin(.add, sum, try self.simdRawSplat(wide_bits, vector.laneCount(), 1), "") catch return error.OutOfMemory;
                const average = wip.bin(.lshr, biased, try self.simdRawSplat(wide_bits, vector.laneCount(), 1), "") catch return error.OutOfMemory;
                try self.storeSimdLocal(target, wip.cast(.trunc, average, vector_ty, "") catch return error.OutOfMemory);
            },
            .simd_bit_select => {
                const mask = try self.loadSimdLocal(GuardedList.at(args, 0));
                const when_set = try self.loadSimdLocal(GuardedList.at(args, 1));
                const when_clear = try self.loadSimdLocal(GuardedList.at(args, 2));
                const set_bits = wip.bin(.@"and", mask, when_set, "") catch return error.OutOfMemory;
                const clear_bits = wip.bin(.@"and", wip.not(mask, "") catch return error.OutOfMemory, when_clear, "") catch return error.OutOfMemory;
                try self.storeSimdLocal(target, wip.bin(.@"or", set_bits, clear_bits, "") catch return error.OutOfMemory);
            },
            .simd_eq_lanes, .simd_gt_lanes, .simd_gte_lanes => {
                const lhs = try self.loadSimdLocal(GuardedList.at(args, 0));
                const rhs = try self.loadSimdLocal(GuardedList.at(args, 1));
                const condition: LlvmBuilder.IntegerCondition = if (op == .simd_eq_lanes)
                    .eq
                else if (op == .simd_gt_lanes)
                    if (vector.isSigned()) .sgt else .ugt
                else if (op == .simd_gte_lanes)
                    if (vector.isSigned()) .sge else .uge
                else
                    unreachable;
                const compared = wip.icmp(condition, lhs, rhs, "") catch return error.OutOfMemory;
                try self.storeSimdLocal(target, wip.cast(.sext, compared, vector_ty, "") catch return error.OutOfMemory);
            },
            .simd_mul_high,
            .simd_mul_q15_sat,
            .simd_mul_wide_lo,
            .simd_mul_wide_hi,
            .simd_dot_pairs,
            .simd_dot_pairs_sat,
            .simd_sad,
            .simd_bitmask,
            .simd_shl_wrap,
            .simd_shr_wrap,
            .simd_shr_zf_wrap,
            .simd_shr_rounded,
            .simd_interleave_lo,
            .simd_interleave_hi,
            .simd_even_lanes,
            .simd_odd_lanes,
            .simd_reverse_lanes,
            .simd_table_lookup,
            .simd_concat_shift_bytes,
            .simd_widen_lo,
            .simd_widen_hi,
            .simd_pairwise_add_widen,
            .simd_narrow_wrap,
            .simd_narrow_sat,
            .simd_sum_lanes,
            .simd_sum_lanes_wrap,
            .simd_clmul_lo,
            .simd_clmul_hi,
            => try self.emitSimdComplex(target, op, args, vector, destination_vector),
        }
    }

    fn simdShuffleHalf(self: *MonoLlvmCodeGen, value: LlvmBuilder.Value, vector: layout.Vector, high: bool) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        var indices: [8]u32 = undefined;
        const count: usize = vector.laneCount() / 2;
        const start: u32 = if (high) @intCast(count) else 0;
        for (0..count) |i| indices[i] = start + @as(u32, @intCast(i));
        return wip.shuffleVector(value, builder.poisonValue(try self.simdType(vector)) catch return error.OutOfMemory, try self.simdShuffleMask(indices[0..count]), "") catch return error.OutOfMemory;
    }

    fn simdPairShuffle(self: *MonoLlvmCodeGen, value: LlvmBuilder.Value, lane_count: u16, odd: bool) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        var indices: [8]u32 = undefined;
        const count: usize = lane_count / 2;
        for (0..count) |i| indices[i] = @intCast(2 * i + @intFromBool(odd));
        return wip.shuffleVector(value, builder.poisonValue(value.typeOfWip(wip)) catch return error.OutOfMemory, try self.simdShuffleMask(indices[0..count]), "") catch return error.OutOfMemory;
    }

    fn emitSimdComplex(
        self: *MonoLlvmCodeGen,
        target: LocalId,
        op: lir.LowLevel,
        args: anytype,
        vector: layout.Vector,
        destination_vector: ?layout.Vector,
    ) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const vector_ty = try self.simdType(vector);

        const simd_op = narrowLowLevel(SimdLowLevel, op) orelse return error.UnsupportedLowLevel;
        switch (simd_op) {
            .simd_mul_high => {
                const wide_bits: u16 = vector.laneBits() * 2;
                const lhs = try self.simdExtendVector(try self.loadSimdLocal(GuardedList.at(args, 0)), wide_bits, vector.laneCount(), vector.isSigned());
                const rhs = try self.simdExtendVector(try self.loadSimdLocal(GuardedList.at(args, 1)), wide_bits, vector.laneCount(), vector.isSigned());
                const product = wip.bin(.mul, lhs, rhs, "") catch return error.OutOfMemory;
                const shifted = wip.bin(if (vector.isSigned()) .ashr else .lshr, product, try self.simdRawSplat(wide_bits, vector.laneCount(), vector.laneBits()), "") catch return error.OutOfMemory;
                try self.storeSimdLocal(target, wip.cast(.trunc, shifted, vector_ty, "") catch return error.OutOfMemory);
            },
            .simd_mul_q15_sat => {
                if (self.isX86Simd() and self.hasX86Feature(.ssse3)) {
                    const lhs = try self.loadSimdLocal(GuardedList.at(args, 0));
                    const rhs = try self.loadSimdLocal(GuardedList.at(args, 1));
                    const multiplied = try self.callBuiltin("llvm.x86.ssse3.pmul.hr.sw.128", vector_ty, &.{ vector_ty, vector_ty }, &.{ lhs, rhs });
                    const minimum = try self.simdRawSplat(16, 8, 0x8000);
                    const lhs_min = wip.icmp(.eq, lhs, minimum, "") catch return error.OutOfMemory;
                    const rhs_min = wip.icmp(.eq, rhs, minimum, "") catch return error.OutOfMemory;
                    const both_min = wip.bin(.@"and", lhs_min, rhs_min, "") catch return error.OutOfMemory;
                    const maximum = try self.simdRawSplat(16, 8, 32767);
                    try self.storeSimdLocal(target, wip.select(.normal, both_min, maximum, multiplied, "") catch return error.OutOfMemory);
                    return;
                }
                if (self.isAarch64Simd()) {
                    const lhs = try self.loadSimdLocal(GuardedList.at(args, 0));
                    const rhs = try self.loadSimdLocal(GuardedList.at(args, 1));
                    try self.storeSimdLocal(target, try self.callBuiltin("llvm.aarch64.neon.sqrdmulh.v8i16", vector_ty, &.{ vector_ty, vector_ty }, &.{ lhs, rhs }));
                    return;
                }
                const lane_count = vector.laneCount();
                // Use i64 intermediates so the pinned -32768 * -32768 case
                // reaches +32768 before the final i16 saturation instead of
                // overflowing while doubling an i32 product.
                const lhs = try self.simdExtendVector(try self.loadSimdLocal(GuardedList.at(args, 0)), 64, lane_count, true);
                const rhs = try self.simdExtendVector(try self.loadSimdLocal(GuardedList.at(args, 1)), 64, lane_count, true);
                const product = wip.bin(.mul, lhs, rhs, "") catch return error.OutOfMemory;
                const doubled = wip.bin(.shl, product, try self.simdRawSplat(64, lane_count, 1), "") catch return error.OutOfMemory;
                const rounded = wip.bin(.add, doubled, try self.simdRawSplat(64, lane_count, 32768), "") catch return error.OutOfMemory;
                const scaled = wip.bin(.ashr, rounded, try self.simdRawSplat(64, lane_count, 16), "") catch return error.OutOfMemory;
                const clamped = try self.simdClampVector(scaled, 64, lane_count, -32768, 32767, true);
                try self.storeSimdLocal(target, wip.cast(.trunc, clamped, vector_ty, "") catch return error.OutOfMemory);
            },
            .simd_mul_wide_lo, .simd_mul_wide_hi => {
                const destination = destination_vector orelse return error.CompilationFailed;
                const high = op == .simd_mul_wide_hi;
                const lhs_half = try self.simdShuffleHalf(try self.loadSimdLocal(GuardedList.at(args, 0)), vector, high);
                const rhs_half = try self.simdShuffleHalf(try self.loadSimdLocal(GuardedList.at(args, 1)), vector, high);
                const lhs = try self.simdExtendVector(lhs_half, destination.laneBits(), destination.laneCount(), vector.isSigned());
                const rhs = try self.simdExtendVector(rhs_half, destination.laneBits(), destination.laneCount(), vector.isSigned());
                try self.storeSimdLocal(target, wip.bin(.mul, lhs, rhs, "") catch return error.OutOfMemory);
            },
            .simd_dot_pairs => {
                // PMADDWD is SSE2, so it is available at every x86 CPU level.
                if (self.isX86Simd()) {
                    const lhs = try self.loadSimdLocal(GuardedList.at(args, 0));
                    const rhs = try self.loadSimdLocal(GuardedList.at(args, 1));
                    try self.storeSimdLocal(target, try self.callBuiltin("llvm.x86.sse2.pmadd.wd", try self.simdRawType(32, 4), &.{ vector_ty, vector_ty }, &.{ lhs, rhs }));
                    return;
                }
                const lhs = try self.simdExtendVector(try self.loadSimdLocal(GuardedList.at(args, 0)), 32, 8, true);
                const rhs = try self.simdExtendVector(try self.loadSimdLocal(GuardedList.at(args, 1)), 32, 8, true);
                const products = wip.bin(.mul, lhs, rhs, "") catch return error.OutOfMemory;
                const even = try self.simdPairShuffle(products, 8, false);
                const odd = try self.simdPairShuffle(products, 8, true);
                try self.storeSimdLocal(target, wip.bin(.add, even, odd, "") catch return error.OutOfMemory);
            },
            .simd_dot_pairs_sat => {
                if (self.isX86Simd() and self.hasX86Feature(.ssse3)) {
                    const lhs = try self.loadSimdLocal(GuardedList.at(args, 0));
                    const rhs = try self.loadSimdLocal(GuardedList.at(args, 1));
                    try self.storeSimdLocal(target, try self.callBuiltin("llvm.x86.ssse3.pmadd.ub.sw.128", try self.simdRawType(16, 8), &.{ vector_ty, vector_ty }, &.{ lhs, rhs }));
                    return;
                }
                // A u8*i8 product fits in i16, but the sum of two products
                // does not. Accumulate in i32 lanes and saturate before
                // narrowing, matching PMADDUBSW rather than wrapping early.
                const lhs = try self.simdExtendVector(try self.loadSimdLocal(GuardedList.at(args, 0)), 32, 16, false);
                const rhs = try self.simdExtendVector(try self.loadSimdLocal(GuardedList.at(args, 1)), 32, 16, true);
                const products = wip.bin(.mul, lhs, rhs, "") catch return error.OutOfMemory;
                const even = try self.simdPairShuffle(products, 16, false);
                const odd = try self.simdPairShuffle(products, 16, true);
                const sums = wip.bin(.add, even, odd, "") catch return error.OutOfMemory;
                const clamped = try self.simdClampVector(sums, 32, 8, -32768, 32767, true);
                try self.storeSimdLocal(target, wip.cast(.trunc, clamped, try self.simdRawType(16, 8), "") catch return error.OutOfMemory);
            },
            .simd_sad => {
                // PSADBW is SSE2, so it is available at every x86 CPU level.
                if (self.isX86Simd()) {
                    const lhs = try self.loadSimdLocal(GuardedList.at(args, 0));
                    const rhs = try self.loadSimdLocal(GuardedList.at(args, 1));
                    try self.storeSimdLocal(target, try self.callBuiltin("llvm.x86.sse2.psad.bw", try self.simdRawType(64, 2), &.{ vector_ty, vector_ty }, &.{ lhs, rhs }));
                    return;
                }
                try self.emitSimdSad(target, args);
            },
            .simd_bitmask => try self.emitSimdBitmask(target, args, vector),
            .simd_shl_wrap, .simd_shr_wrap, .simd_shr_zf_wrap => {
                const value = try self.loadSimdLocal(GuardedList.at(args, 0));
                const count_arg = GuardedList.at(args, 1);
                const raw_count = try self.coerceScalar(try self.loadScalar(self.slot(count_arg).ptr, self.localLayout(count_arg)), builder.intType(vector.laneBits()) catch return error.OutOfMemory, false);
                const masked_count = wip.bin(.@"and", raw_count, builder.intValue(raw_count.typeOfWip(wip), vector.laneBits() - 1) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
                const counts = wip.splatVector(vector_ty, masked_count, "") catch return error.OutOfMemory;
                const tag: LlvmBuilder.Function.Instruction.Tag = if (op == .simd_shl_wrap)
                    .shl
                else if (op == .simd_shr_wrap)
                    if (vector.isSigned()) .ashr else .lshr
                else if (op == .simd_shr_zf_wrap)
                    .lshr
                else
                    unreachable;
                try self.storeSimdLocal(target, wip.bin(tag, value, counts, "") catch return error.OutOfMemory);
            },
            .simd_shr_rounded => try self.emitSimdRoundedShift(target, args, vector),
            .simd_interleave_lo, .simd_interleave_hi, .simd_even_lanes, .simd_odd_lanes, .simd_reverse_lanes => {
                const lhs = try self.loadSimdLocal(GuardedList.at(args, 0));
                const rhs = if (op == .simd_reverse_lanes)
                    builder.poisonValue(vector_ty) catch return error.OutOfMemory
                else
                    try self.loadSimdLocal(GuardedList.at(args, 1));
                var indices: [16]u32 = undefined;
                const count: usize = vector.laneCount();
                if (op == .simd_interleave_lo or op == .simd_interleave_hi) {
                    const start: usize = if (op == .simd_interleave_hi) count / 2 else 0;
                    for (0..count / 2) |i| {
                        indices[2 * i] = @intCast(start + i);
                        indices[2 * i + 1] = @intCast(count + start + i);
                    }
                } else if (op == .simd_even_lanes or op == .simd_odd_lanes) {
                    const parity: usize = @intFromBool(op == .simd_odd_lanes);
                    for (0..count / 2) |i| {
                        indices[i] = @intCast(2 * i + parity);
                        indices[count / 2 + i] = @intCast(count + 2 * i + parity);
                    }
                } else if (op == .simd_reverse_lanes) {
                    for (0..count) |i| {
                        indices[i] = @intCast(count - 1 - i);
                    }
                } else {
                    unreachable;
                }
                try self.storeSimdLocal(target, wip.shuffleVector(lhs, rhs, try self.simdShuffleMask(indices[0..count]), "") catch return error.OutOfMemory);
            },
            .simd_table_lookup => try self.emitSimdTableLookup(target, args),
            .simd_concat_shift_bytes => try self.emitSimdConcatShift(target, args),
            .simd_widen_lo, .simd_widen_hi => {
                const destination = destination_vector orelse return error.CompilationFailed;
                const half = try self.simdShuffleHalf(try self.loadSimdLocal(GuardedList.at(args, 0)), vector, op == .simd_widen_hi);
                try self.storeSimdLocal(target, try self.simdExtendVector(half, destination.laneBits(), destination.laneCount(), vector.isSigned()));
            },
            .simd_pairwise_add_widen => {
                const destination = destination_vector orelse return error.CompilationFailed;
                const value = try self.loadSimdLocal(GuardedList.at(args, 0));
                const even = try self.simdPairShuffle(value, vector.laneCount(), false);
                const odd = try self.simdPairShuffle(value, vector.laneCount(), true);
                const wide_even = try self.simdExtendVector(even, destination.laneBits(), destination.laneCount(), vector.isSigned());
                const wide_odd = try self.simdExtendVector(odd, destination.laneBits(), destination.laneCount(), vector.isSigned());
                try self.storeSimdLocal(target, wip.bin(.add, wide_even, wide_odd, "") catch return error.OutOfMemory);
            },
            .simd_narrow_wrap, .simd_narrow_sat => try self.emitSimdNarrow(target, args, vector, destination_vector orelse return error.CompilationFailed, op == .simd_narrow_sat),
            .simd_sum_lanes, .simd_sum_lanes_wrap => try self.emitSimdSum(target, args, vector),
            .simd_clmul_lo, .simd_clmul_hi => try self.emitSimdClmul(target, args, op == .simd_clmul_hi),
            .simd_splat,
            .simd_get_lane_unchecked,
            .simd_with_lane_unchecked,
            .simd_to_u128_bits,
            .simd_from_u128_bits,
            .simd_add_wrap,
            .simd_sub_wrap,
            .simd_add_sat,
            .simd_sub_sat,
            .simd_neg_wrap,
            .simd_abs_wrap,
            .simd_min,
            .simd_max,
            .simd_abs_diff,
            .simd_avg_rounded,
            .simd_mul_wrap,
            .simd_and,
            .simd_or,
            .simd_xor,
            .simd_not,
            .simd_bit_select,
            .simd_eq_lanes,
            .simd_gt_lanes,
            .simd_gte_lanes,
            => return error.UnsupportedLowLevel,
        }
    }

    fn emitSimdSad(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const lhs = try self.loadSimdLocal(GuardedList.at(args, 0));
        const rhs = try self.loadSimdLocal(GuardedList.at(args, 1));
        const result_ty = try self.simdRawType(64, 2);
        var result = builder.zeroInitValue(result_ty) catch return error.OutOfMemory;
        for (0..2) |half| {
            var sum = builder.intValue(.i64, 0) catch return error.OutOfMemory;
            for (0..8) |lane_in_half| {
                const lane = half * 8 + lane_in_half;
                const index = builder.intValue(.i32, lane) catch return error.OutOfMemory;
                const a8 = wip.extractElement(lhs, index, "") catch return error.OutOfMemory;
                const b8 = wip.extractElement(rhs, index, "") catch return error.OutOfMemory;
                const a = try self.coerceScalar(a8, .i64, false);
                const b = try self.coerceScalar(b8, .i64, false);
                const a_ge_b = wip.icmp(.uge, a, b, "") catch return error.OutOfMemory;
                const forward = wip.bin(.sub, a, b, "") catch return error.OutOfMemory;
                const reverse = wip.bin(.sub, b, a, "") catch return error.OutOfMemory;
                sum = wip.bin(.add, sum, wip.select(.normal, a_ge_b, forward, reverse, "") catch return error.OutOfMemory, "") catch return error.OutOfMemory;
            }
            result = wip.insertElement(result, sum, builder.intValue(.i32, half) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        }
        try self.storeSimdLocal(target, result);
    }

    fn emitSimdBitmask(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, vector: layout.Vector) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const value = try self.loadSimdLocal(GuardedList.at(args, 0));
        const result_ty = self.scalarType(self.localLayout(target));
        var result = builder.intValue(result_ty, 0) catch return error.OutOfMemory;
        for (0..vector.laneCount()) |i| {
            const lane = wip.extractElement(value, builder.intValue(.i32, i) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
            const sign = wip.bin(.lshr, lane, builder.intValue(lane.typeOfWip(wip), vector.laneBits() - 1) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
            const bit = try self.coerceScalar(sign, result_ty, false);
            const positioned = if (i == 0)
                bit
            else
                wip.bin(.shl, bit, builder.intValue(result_ty, i) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
            result = wip.bin(.@"or", result, positioned, "") catch return error.OutOfMemory;
        }
        try self.storeScalar(self.slot(target).ptr, self.localLayout(target), result);
    }

    fn emitSimdRoundedShift(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, vector: layout.Vector) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const value = try self.loadSimdLocal(GuardedList.at(args, 0));
        const count_arg = GuardedList.at(args, 1);
        const count = try self.coerceScalar(try self.loadScalar(self.slot(count_arg).ptr, self.localLayout(count_arg)), .i32, false);
        const zero32 = builder.intValue(.i32, 0) catch return error.OutOfMemory;
        const is_zero = wip.icmp(.eq, count, zero32, "") catch return error.OutOfMemory;
        const out_of_range = wip.icmp(.uge, count, builder.intValue(.i32, vector.laneBits()) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const safe_count = wip.select(.normal, is_zero, builder.intValue(.i32, 1) catch return error.OutOfMemory, count, "") catch return error.OutOfMemory;
        const wide_bits: u16 = vector.laneBits() * 2;
        const wide_ty = try self.simdRawType(wide_bits, vector.laneCount());
        const wide_value = try self.simdExtendVector(value, wide_bits, vector.laneCount(), true);
        const wide_count_scalar = try self.coerceScalar(safe_count, builder.intType(wide_bits) catch return error.OutOfMemory, false);
        const wide_counts = wip.splatVector(wide_ty, wide_count_scalar, "") catch return error.OutOfMemory;
        const bias_count = wip.bin(.sub, wide_count_scalar, builder.intValue(wide_count_scalar.typeOfWip(wip), 1) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const bias_scalar = wip.bin(.shl, builder.intValue(wide_count_scalar.typeOfWip(wip), 1) catch return error.OutOfMemory, bias_count, "") catch return error.OutOfMemory;
        const bias = wip.splatVector(wide_ty, bias_scalar, "") catch return error.OutOfMemory;
        const rounded = wip.bin(.ashr, wip.bin(.add, wide_value, bias, "") catch return error.OutOfMemory, wide_counts, "") catch return error.OutOfMemory;
        const narrowed = wip.cast(.trunc, rounded, try self.simdType(vector), "") catch return error.OutOfMemory;
        const zero_vector = builder.zeroInitValue(try self.simdType(vector)) catch return error.OutOfMemory;
        const after_range = wip.select(.normal, out_of_range, zero_vector, narrowed, "") catch return error.OutOfMemory;
        try self.storeSimdLocal(target, wip.select(.normal, is_zero, value, after_range, "") catch return error.OutOfMemory);
    }

    fn emitSimdTableLookup(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const table = try self.loadSimdLocal(GuardedList.at(args, 0));
        const indices = try self.loadSimdLocal(GuardedList.at(args, 1));
        const vector_ty = try self.simdRawType(8, 16);
        if (self.isX86Simd() and self.hasX86Feature(.ssse3)) {
            // pshufb zeroes a lane when bit 7 of its index is set, but wraps
            // indices 16-127 through `& 0x0F` instead of zeroing them, which is
            // not the semantics this op promises. Saturating-add 0x70 first:
            // 0-15 land in 0x70-0x7F, leaving bit 7 clear and the low nibble
            // intact, while everything >= 16 saturates to at least 0x80 and so
            // zeroes. That is one `paddusb`, and emitting it directly keeps the
            // guarantee independent of whether an optimizer is running.
            const biased = wip.callIntrinsic(.normal, .none, .@"uadd.sat", &.{vector_ty}, &.{ indices, try self.simdRawSplat(8, 16, 0x70) }, "") catch return error.OutOfMemory;
            try self.storeSimdLocal(target, try self.callBuiltin("llvm.x86.ssse3.pshuf.b.128", vector_ty, &.{ vector_ty, vector_ty }, &.{ table, biased }));
            return;
        }
        // TBL is part of NEON, which Armv8.0-A makes mandatory.
        if (self.isAarch64Simd()) {
            try self.storeSimdLocal(target, try self.callBuiltin("llvm.aarch64.neon.tbl1.v16i8", vector_ty, &.{ vector_ty, vector_ty }, &.{ table, indices }));
            return;
        }
        var result = builder.zeroInitValue(vector_ty) catch return error.OutOfMemory;
        for (0..16) |i| {
            const lane_index = builder.intValue(.i32, i) catch return error.OutOfMemory;
            const requested8 = wip.extractElement(indices, lane_index, "") catch return error.OutOfMemory;
            const requested = try self.coerceScalar(requested8, .i32, false);
            const in_range = wip.icmp(.ult, requested, builder.intValue(.i32, 16) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
            const safe_index = wip.select(.normal, in_range, requested, builder.intValue(.i32, 0) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
            const selected = wip.extractElement(table, safe_index, "") catch return error.OutOfMemory;
            const lane = wip.select(.normal, in_range, selected, builder.intValue(.i8, 0) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
            result = wip.insertElement(result, lane, lane_index, "") catch return error.OutOfMemory;
        }
        try self.storeSimdLocal(target, result);
    }

    fn emitSimdConcatShift(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const lhs_vector = try self.loadSimdLocal(GuardedList.at(args, 0));
        const rhs_vector = try self.loadSimdLocal(GuardedList.at(args, 1));
        const count_arg = GuardedList.at(args, 2);
        const count = try self.coerceScalar(try self.loadScalar(self.slot(count_arg).ptr, self.localLayout(count_arg)), .i128, false);
        const zero = builder.intValue(.i128, 0) catch return error.OutOfMemory;
        const sixteen = builder.intValue(.i128, 16) catch return error.OutOfMemory;
        const is_zero = wip.icmp(.eq, count, zero, "") catch return error.OutOfMemory;
        const is_sixteen = wip.icmp(.eq, count, sixteen, "") catch return error.OutOfMemory;
        const safe_nonzero = wip.select(.normal, is_zero, builder.intValue(.i128, 1) catch return error.OutOfMemory, count, "") catch return error.OutOfMemory;
        const safe_count = wip.select(.normal, is_sixteen, builder.intValue(.i128, 15) catch return error.OutOfMemory, safe_nonzero, "") catch return error.OutOfMemory;
        const bit_count = wip.bin(.mul, safe_count, builder.intValue(.i128, 8) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const inverse = wip.bin(.sub, builder.intValue(.i128, 128) catch return error.OutOfMemory, bit_count, "") catch return error.OutOfMemory;
        const lhs = wip.cast(.bitcast, lhs_vector, .i128, "") catch return error.OutOfMemory;
        const rhs = wip.cast(.bitcast, rhs_vector, .i128, "") catch return error.OutOfMemory;
        const shifted = wip.bin(.@"or", wip.bin(.lshr, lhs, bit_count, "") catch return error.OutOfMemory, wip.bin(.shl, rhs, inverse, "") catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const after_sixteen = wip.select(.normal, is_sixteen, rhs, shifted, "") catch return error.OutOfMemory;
        const result = wip.select(.normal, is_zero, lhs, after_sixteen, "") catch return error.OutOfMemory;
        try self.storeSimdLocal(target, wip.cast(.bitcast, result, lhs_vector.typeOfWip(wip), "") catch return error.OutOfMemory);
    }

    fn emitSimdNarrow(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, source: layout.Vector, destination: layout.Vector, saturated: bool) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        var lhs = try self.loadSimdLocal(GuardedList.at(args, 0));
        var rhs = try self.loadSimdLocal(GuardedList.at(args, 1));
        if (saturated) {
            const upper: i128 = if (destination.isSigned())
                (@as(i128, 1) << @intCast(destination.laneBits() - 1)) - 1
            else
                (@as(i128, 1) << @intCast(destination.laneBits())) - 1;
            const lower: i128 = if (destination.isSigned()) -(@as(i128, 1) << @intCast(destination.laneBits() - 1)) else 0;
            const signed_compare = destination.isSigned() or source.isSigned();
            lhs = try self.simdClampVector(lhs, source.laneBits(), source.laneCount(), lower, upper, signed_compare);
            rhs = try self.simdClampVector(rhs, source.laneBits(), source.laneCount(), lower, upper, signed_compare);
        }
        const narrow_half_ty = try self.simdRawType(destination.laneBits(), source.laneCount());
        const lhs_narrow = wip.cast(.trunc, lhs, narrow_half_ty, "") catch return error.OutOfMemory;
        const rhs_narrow = wip.cast(.trunc, rhs, narrow_half_ty, "") catch return error.OutOfMemory;
        var indices: [16]u32 = undefined;
        for (0..destination.laneCount()) |i| indices[i] = @intCast(i);
        const result = wip.shuffleVector(lhs_narrow, rhs_narrow, try self.simdShuffleMask(indices[0..destination.laneCount()]), "") catch return error.OutOfMemory;
        try self.storeSimdLocal(target, result);
    }

    fn emitSimdSum(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, vector: layout.Vector) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const target_layout = self.localLayout(target);
        const result_bits: u16 = @intCast(self.intBits(target_layout));
        const value = try self.loadSimdLocal(GuardedList.at(args, 0));
        const extended = if (result_bits == vector.laneBits())
            value
        else
            try self.simdExtendVector(value, result_bits, vector.laneCount(), vector.isSigned());
        const result = wip.callIntrinsic(.normal, .none, .@"vector.reduce.add", &.{extended.typeOfWip(wip)}, &.{extended}, "") catch return error.OutOfMemory;
        try self.storeScalar(self.slot(target).ptr, target_layout, result);
    }

    fn emitSimdClmul(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, high: bool) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const lhs_vector = try self.loadSimdLocal(GuardedList.at(args, 0));
        const rhs_vector = try self.loadSimdLocal(GuardedList.at(args, 1));
        const index = builder.intValue(.i32, @intFromBool(high)) catch return error.OutOfMemory;
        const lhs64 = wip.extractElement(lhs_vector, index, "") catch return error.OutOfMemory;
        const rhs64 = wip.extractElement(rhs_vector, index, "") catch return error.OutOfMemory;
        if (self.isX86Simd() and self.hasX86Feature(.pclmul)) {
            const vector_ty = try self.simdRawType(64, 2);
            const immediate: u8 = if (high) 0x11 else 0x00;
            try self.storeSimdLocal(target, try self.callBuiltin(
                "llvm.x86.pclmulqdq",
                vector_ty,
                &.{ vector_ty, vector_ty, .i8 },
                &.{ lhs_vector, rhs_vector, builder.intValue(.i8, immediate) catch return error.OutOfMemory },
            ));
            return;
        }
        // PMULL64 comes from the AES extension, not from base NEON, so an
        // Armv8.0-A target without it takes the bitwise path below.
        if (self.isAarch64Simd() and self.hasAarch64Feature(.aes)) {
            const byte_vector_ty = try self.simdRawType(8, 16);
            const product = try self.callBuiltin("llvm.aarch64.neon.pmull64", byte_vector_ty, &.{ .i64, .i64 }, &.{ lhs64, rhs64 });
            try self.storeSimdLocal(target, product);
            return;
        }
        const lhs = try self.coerceScalar(lhs64, .i128, false);
        var result = builder.intValue(.i128, 0) catch return error.OutOfMemory;
        for (0..64) |i| {
            const bit = wip.bin(.@"and", wip.bin(.lshr, rhs64, builder.intValue(.i64, i) catch return error.OutOfMemory, "") catch return error.OutOfMemory, builder.intValue(.i64, 1) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
            const bit128 = try self.coerceScalar(bit, .i128, false);
            const mask = wip.neg(bit128, "") catch return error.OutOfMemory;
            const shifted = if (i == 0) lhs else wip.bin(.shl, lhs, builder.intValue(.i128, i) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
            result = wip.bin(.xor, result, wip.bin(.@"and", shifted, mask, "") catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        }
        try self.storeSimdLocal(target, wip.cast(.bitcast, result, lhs_vector.typeOfWip(wip), "") catch return error.OutOfMemory);
    }
    fn emitNumericAbs(self: *MonoLlvmCodeGen, target: LocalId, op: lir.LowLevel, arg: LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const target_layout = self.localLayout(target);
        const checked_op: ?lir.LowLevel = if (CheckedArithmetic.uncheckedOp(op) != null) op else null;
        const value = try self.loadScalar(self.slot(arg).ptr, self.localLayout(arg));
        if (!target_layout.isSigned() and !isFloatLayout(target_layout)) {
            try self.storeScalar(self.slot(target).ptr, target_layout, value);
            return;
        }
        if (checked_op) |checked| {
            if (isFloatLayout(target_layout)) return error.UnsupportedLowLevel;
            const lowest = builder.intValue(value.typeOfWip(wip), CheckedArithmetic.signedLowestValue(target_layout) orelse unreachable) catch return error.OutOfMemory;
            const is_lowest = wip.icmp(.eq, value, lowest, "") catch return error.OutOfMemory;
            try self.emitCrashIf(is_lowest, CheckedArithmetic.overflowMessageForLayout(checked, target_layout) orelse unreachable);
        }
        const zero = builder.zeroInitValue(value.typeOfWip(wip)) catch return error.OutOfMemory;
        const is_neg = if (isFloatLayout(target_layout))
            wip.fcmp(.normal, .olt, value, zero, "") catch return error.OutOfMemory
        else
            wip.icmp(.slt, value, zero, "") catch return error.OutOfMemory;
        const neg = if (isFloatLayout(target_layout))
            wip.un(.fneg, value, "") catch return error.OutOfMemory
        else
            wip.neg(value, "") catch return error.OutOfMemory;
        const result = wip.select(.normal, is_neg, neg, value, "") catch return error.OutOfMemory;
        try self.storeScalar(self.slot(target).ptr, target_layout, result);
    }

    fn emitNumericAbsDiff(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const lhs_layout = self.localLayout(GuardedList.at(args, 0));
        const rhs_layout = self.localLayout(GuardedList.at(args, 1));
        const lhs = try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 0)).ptr, lhs_layout), .i128, lhs_layout.isSigned());
        const rhs = try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, rhs_layout), .i128, rhs_layout.isSigned());
        const zero = builder.intValue(.i128, 0) catch return error.OutOfMemory;

        const result = if (lhs_layout.isSigned() or rhs_layout.isSigned()) blk: {
            const diff = wip.bin(.sub, lhs, rhs, "") catch return error.OutOfMemory;
            const is_neg = wip.icmp(.slt, diff, zero, "") catch return error.OutOfMemory;
            const neg = wip.bin(.sub, zero, diff, "") catch return error.OutOfMemory;
            break :blk wip.select(.normal, is_neg, neg, diff, "") catch return error.OutOfMemory;
        } else blk: {
            const lhs_ge_rhs = wip.icmp(.uge, lhs, rhs, "") catch return error.OutOfMemory;
            const lhs_minus_rhs = wip.bin(.sub, lhs, rhs, "") catch return error.OutOfMemory;
            const rhs_minus_lhs = wip.bin(.sub, rhs, lhs, "") catch return error.OutOfMemory;
            break :blk wip.select(.normal, lhs_ge_rhs, lhs_minus_rhs, rhs_minus_lhs, "") catch return error.OutOfMemory;
        };

        const target_layout = self.localLayout(target);
        const coerced = try self.coerceScalar(result, self.scalarType(target_layout), false);
        try self.storeScalar(self.slot(target).ptr, target_layout, coerced);
    }

    /// Narrow `op` to a smaller enum whose variants carry `LowLevel`'s values.
    /// Returns null when `op` is not one of them.
    fn narrowLowLevel(comptime Narrow: type, op: lir.LowLevel) ?Narrow {
        const raw = @intFromEnum(op);
        inline for (@typeInfo(Narrow).@"enum".fields) |field| {
            if (raw == field.value) return @enumFromInt(field.value);
        }
        return null;
    }

    /// Lower a scalar numeric conversion. Delegates to a helper for each
    /// (source class, destination class) pair. Each helper switches on the mode.
    fn emitNumericConversion(self: *MonoLlvmCodeGen, target: LocalId, op: lir.LowLevel, args: anytype) Error!void {
        if (args.len < 1) return error.UnsupportedLowLevel;
        const arg = GuardedList.at(args, 0);
        const spec = numeric_conversion.getConversionSpec(op) orelse return error.UnsupportedLowLevel;
        switch (spec.src.class()) {
            .int => switch (spec.dst.class()) {
                .int => try self.lowerIntToInt(spec, target, arg),
                .float => try self.lowerIntToFloat(spec, target, arg),
                .dec => try self.lowerIntToDec(spec, target, arg),
            },
            .float => switch (spec.dst.class()) {
                .int => try self.lowerFloatToInt(spec, target, arg),
                .float => try self.lowerFloatToFloat(spec, target, arg),
                .dec => return error.UnsupportedLowLevel,
            },
            .dec => switch (spec.dst.class()) {
                .int => try self.lowerDecToInt(spec, target, arg),
                .float => try self.lowerDecToFloat(spec, target, arg),
                .dec => return error.UnsupportedLowLevel,
            },
        }
    }

    fn lowerIntToInt(self: *MonoLlvmCodeGen, spec: numeric_conversion.Conversion, target: LocalId, arg: LocalId) Error!void {
        switch (spec.mode) {
            .exact, .wrap => try self.emitScalarCoercion(target, arg),
            .@"try" => try self.emitIntTryConversion(target, arg),
            .trunc, .try_unsafe => return error.UnsupportedLowLevel,
        }
    }

    fn lowerIntToFloat(self: *MonoLlvmCodeGen, spec: numeric_conversion.Conversion, target: LocalId, arg: LocalId) Error!void {
        switch (spec.mode) {
            .exact => try self.emitScalarCoercion(target, arg),
            .wrap, .trunc, .@"try", .try_unsafe => return error.UnsupportedLowLevel,
        }
    }

    fn lowerIntToDec(self: *MonoLlvmCodeGen, spec: numeric_conversion.Conversion, target: LocalId, arg: LocalId) Error!void {
        switch (spec.mode) {
            .exact => try self.emitIntToDec(target, arg),
            // The builtin takes the source as two 64-bit halves, so a narrower
            // source has no lowering here.
            .try_unsafe => if (spec.src.bits() == 128)
                try self.emitInt128ToDecTryUnsafeConversion(target, arg, spec.src.isSigned())
            else
                return error.UnsupportedLowLevel,
            .wrap, .trunc, .@"try" => return error.UnsupportedLowLevel,
        }
    }

    fn lowerFloatToInt(self: *MonoLlvmCodeGen, spec: numeric_conversion.Conversion, target: LocalId, arg: LocalId) Error!void {
        switch (spec.mode) {
            .trunc => try self.emitFloatToIntTruncConversion(spec, target, arg),
            .try_unsafe => try self.emitFloatToIntTryUnsafeConversion(target, arg),
            .exact, .wrap, .@"try" => return error.UnsupportedLowLevel,
        }
    }

    fn lowerFloatToFloat(self: *MonoLlvmCodeGen, spec: numeric_conversion.Conversion, target: LocalId, arg: LocalId) Error!void {
        switch (spec.mode) {
            .exact, .wrap => try self.emitScalarCoercion(target, arg),
            // The builtin is the f64 to f32 one, so the other direction has no
            // lowering here.
            .try_unsafe => if (spec.src == .f64 and spec.dst == .f32)
                try self.emitF64ToF32TryUnsafeConversion(target, arg)
            else
                return error.UnsupportedLowLevel,
            .trunc, .@"try" => return error.UnsupportedLowLevel,
        }
    }

    fn lowerDecToInt(self: *MonoLlvmCodeGen, spec: numeric_conversion.Conversion, target: LocalId, arg: LocalId) Error!void {
        switch (spec.mode) {
            .trunc => try self.emitDecToIntTruncConversion(target, arg),
            .try_unsafe => try self.emitDecToIntTryUnsafeConversion(target, arg),
            .exact, .wrap, .@"try" => return error.UnsupportedLowLevel,
        }
    }

    fn lowerDecToFloat(self: *MonoLlvmCodeGen, spec: numeric_conversion.Conversion, target: LocalId, arg: LocalId) Error!void {
        switch (spec.mode) {
            .exact, .wrap => try self.emitDecToFloatConversion(target, arg, spec.dst == .f32),
            // The builtin is the Dec to f32 one, so a wider destination has no
            // lowering here.
            .try_unsafe => if (spec.dst == .f32)
                try self.emitDecToF32TryUnsafeConversion(target, arg)
            else
                return error.UnsupportedLowLevel,
            .trunc, .@"try" => return error.UnsupportedLowLevel,
        }
    }

    /// Convert a scalar to the target's type with a single LLVM instruction. The
    /// layouts of `arg` and `target` decide which: sext, zext, trunc, sitofp,
    /// uitofp, fpext, or fptrunc.
    fn emitScalarCoercion(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const src_layout = self.localLayout(arg);
        const target_layout = self.localLayout(target);
        const value = try self.loadScalar(self.slot(arg).ptr, src_layout);
        const coerced = try self.coerceScalar(value, self.scalarType(target_layout), src_layout.isSigned());
        try self.storeScalar(self.slot(target).ptr, target_layout, coerced);
    }

    fn emitDecToFloatConversion(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId, is_f32: bool) Error!void {
        const dec_value = try self.loadScalar(self.slot(arg).ptr, .dec);
        const parts = try self.splitI128Value(dec_value);
        const float_ty: LlvmBuilder.Type = if (is_f32) .float else .double;
        const result = try self.callBuiltin(
            LowLevelBuiltins.decToFloat(is_f32).symbolName(),
            float_ty,
            &.{ .i64, .i64 },
            &.{ parts.low, parts.high },
        );
        try self.storeScalar(self.slot(target).ptr, self.localLayout(target), result);
    }

    /// A Dec's payload is its value scaled by 10^18, so recovering the whole
    /// part means dividing the payload by that scale before wrapping it into
    /// the destination width. The division runs at the full i128 width so whole
    /// parts beyond i64 wrap rather than trap, and it goes through the same
    /// i128 div-trunc builtin the dev and wasm backends call: see
    /// `emitI128DivRem` for why the module must not contain a 128-bit divide.
    fn emitDecToIntTruncConversion(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const payload = try self.loadScalar(self.slot(arg).ptr, .dec);
        const scale = builder.intValue(.i128, builtins.dec.RocDec.one_point_zero_i128) catch return error.OutOfMemory;
        const whole = try self.callI128BinaryBuiltin(
            builtinSymbol(LowLevelBuiltins.i128DivRem(false, false)),
            payload,
            scale,
            true,
        );
        const target_layout = self.localLayout(target);
        const wrapped = try self.coerceScalar(whole, self.scalarType(target_layout), true);
        try self.storeScalar(self.slot(target).ptr, target_layout, wrapped);
    }

    /// Wrapping float→int conversion: the builtin wrapper implements Roc's
    /// wrap semantics (NaN and the infinities produce 0; finite values
    /// truncate toward zero and wrap modulo 2^bits), writing the result bytes
    /// directly into the target slot.
    fn emitFloatToIntTruncConversion(self: *MonoLlvmCodeGen, spec: numeric_conversion.Conversion, target: LocalId, arg: LocalId) Error!void {
        const src_is_f32 = spec.src == .f32;
        const target_bits: u32 = spec.dst.bits();
        const builder = self.builder orelse return error.CompilationFailed;
        const value = try self.loadScalar(self.slot(arg).ptr, self.localLayout(arg));
        const name = if (src_is_f32) builtinSymbol(.f32_to_int_wrap) else builtinSymbol(.f64_to_int_wrap);
        const float_ty: LlvmBuilder.Type = if (src_is_f32) .float else .double;
        try self.callBuiltinVoid(
            name,
            &.{ try self.ptrType(), float_ty, .i32, .i32 },
            &.{
                self.slot(target).ptr,
                value,
                builder.intValue(.i32, target_bits) catch return error.OutOfMemory,
                builder.intValue(.i32, target_bits / 8) catch return error.OutOfMemory,
            },
        );
    }

    fn emitIntToDec(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const arg_layout = self.localLayout(arg);
        const value = try self.loadScalar(self.slot(arg).ptr, arg_layout);
        const value64 = try self.coerceScalar(value, .i64, arg_layout.isSigned());
        const low_ptr = try self.allocEntryBlockSlot(.i64, 1, LlvmBuilder.Alignment.fromByteUnits(8), "dec_low");
        const high_ptr = try self.allocEntryBlockSlot(.i64, 1, LlvmBuilder.Alignment.fromByteUnits(8), "dec_high");
        const fn_name = if (arg_layout.isSigned()) builtinSymbol(.i64_to_dec) else builtinSymbol(.u64_to_dec);
        try self.callBuiltinVoid(fn_name, &.{ try self.ptrType(), try self.ptrType(), .i64 }, &.{ low_ptr, high_ptr, value64 });
        const low = wip.load(.normal, .i64, low_ptr, LlvmBuilder.Alignment.fromByteUnits(8), "") catch return error.OutOfMemory;
        const high = wip.load(.normal, .i64, high_ptr, LlvmBuilder.Alignment.fromByteUnits(8), "") catch return error.OutOfMemory;
        try self.storeScalar(self.slot(target).ptr, .dec, try self.combineI128Parts(low, high));
    }

    fn emitIntTryConversion(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const allocated = try self.allocAggregateTarget(target);
        const target_payload_layout = self.tagPayloadLayout(allocated.layout_idx, 1);
        if (!isIntegerLayout(target_payload_layout)) return error.CompilationFailed;
        const source_layout = self.localLayout(arg);
        const source_signed = source_layout.isSigned();
        const disc_offset = try self.tagDiscriminantOffset(allocated.layout_idx);
        const payload_size = self.layoutByteSize(target_payload_layout);
        const target_bits = self.intBits(target_payload_layout);
        const target_signed = target_payload_layout.isSigned();

        // Sources up to 64 bits use the bounds-checked scalar wrappers, with the
        // target bounds computed exactly as the dev/wasm backends do. Only
        // 128-bit sources need the range-checked i128 wrappers.
        if (self.intBits(source_layout) <= 64) {
            const value = try self.loadScalar(self.slot(arg).ptr, source_layout);
            const value64 = try self.coerceScalar(value, .i64, source_signed);
            if (source_signed) {
                const min_val: i64 = if (target_signed) blk: {
                    if (target_bits >= 64) break :blk std.math.minInt(i64);
                    const shift: u6 = @intCast(target_bits - 1);
                    break :blk -(@as(i64, 1) << shift);
                } else 0;
                const max_val: i64 = if (target_signed) blk: {
                    if (target_bits >= 64) break :blk std.math.maxInt(i64);
                    const shift: u6 = @intCast(target_bits - 1);
                    break :blk (@as(i64, 1) << shift) - 1;
                } else blk: {
                    if (target_bits >= 64) break :blk std.math.maxInt(i64);
                    const shift: u6 = @intCast(target_bits);
                    break :blk (@as(i64, 1) << shift) - 1;
                };
                try self.callBuiltinVoid(
                    builtinSymbol(LowLevelBuiltins.intTryConvert(false, true)),
                    &.{ try self.ptrType(), .i64, .i64, .i64, .i32, .i32 },
                    &.{
                        allocated.ptr,
                        value64,
                        builder.intValue(.i64, min_val) catch return error.OutOfMemory,
                        builder.intValue(.i64, max_val) catch return error.OutOfMemory,
                        builder.intValue(.i32, payload_size) catch return error.OutOfMemory,
                        builder.intValue(.i32, disc_offset) catch return error.OutOfMemory,
                    },
                );
            } else {
                const max_val: u64 = if (target_signed) blk: {
                    if (target_bits >= 64) break :blk @as(u64, @bitCast(@as(i64, std.math.maxInt(i64))));
                    const shift: u6 = @intCast(target_bits - 1);
                    break :blk @as(u64, @intCast((@as(i64, 1) << shift) - 1));
                } else blk: {
                    if (target_bits >= 64) break :blk std.math.maxInt(u64);
                    const shift: u6 = @intCast(target_bits);
                    break :blk (@as(u64, 1) << shift) - 1;
                };
                try self.callBuiltinVoid(
                    builtinSymbol(LowLevelBuiltins.intTryConvert(false, false)),
                    &.{ try self.ptrType(), .i64, .i64, .i32, .i32 },
                    &.{
                        allocated.ptr,
                        value64,
                        builder.intValue(.i64, @as(i64, @bitCast(max_val))) catch return error.OutOfMemory,
                        builder.intValue(.i32, payload_size) catch return error.OutOfMemory,
                        builder.intValue(.i32, disc_offset) catch return error.OutOfMemory,
                    },
                );
            }
            return;
        }

        const value = try self.loadScalar(self.slot(arg).ptr, source_layout);
        const value128 = try self.coerceScalar(value, .i128, source_signed);
        const parts = try self.splitI128Value(value128);
        try self.callBuiltinVoid(
            LowLevelBuiltins.intTryConvert(true, source_signed).symbolName(),
            &.{ try self.ptrType(), .i64, .i64, .i32, .i32, .i32, .i32 },
            &.{
                allocated.ptr,
                parts.low,
                parts.high,
                builder.intValue(.i32, target_bits) catch return error.OutOfMemory,
                builder.intValue(.i32, @intFromBool(target_signed)) catch return error.OutOfMemory,
                builder.intValue(.i32, payload_size) catch return error.OutOfMemory,
                builder.intValue(.i32, disc_offset) catch return error.OutOfMemory,
            },
        );
    }

    const TryUnsafeRecordInfo = struct {
        success_offset: u32,
        value_offset: u32,
        value_layout: layout.Idx,
        value_size: u32,
    };

    fn tryUnsafeRecordInfo(self: *MonoLlvmCodeGen, ret_layout: layout.Idx) Error!TryUnsafeRecordInfo {
        const ret_layout_val = self.layoutValue(ret_layout);
        if (ret_layout_val.tag != .struct_) return error.CompilationFailed;
        const struct_idx = ret_layout_val.getStruct().idx;
        const value_layout = self.layouts().getStructFieldLayoutByOriginalIndex(struct_idx, 1);
        if (!isIntegerLayout(value_layout)) return error.CompilationFailed;
        return .{
            .success_offset = self.layouts().getStructFieldOffsetByOriginalIndex(struct_idx, 0),
            .value_offset = self.layouts().getStructFieldOffsetByOriginalIndex(struct_idx, 1),
            .value_layout = value_layout,
            .value_size = self.layoutByteSize(value_layout),
        };
    }

    const TryUnsafeTarget = struct {
        ptr: LlvmBuilder.Value,
        success_offset: u32,
        value_offset: u32,
    };

    /// Allocate a `{ success, value }` result and read its field offsets. Unlike
    /// `tryUnsafeRecordInfo` this places no constraint on the value field's
    /// layout, so it also serves conversions whose value is a float or a Dec.
    fn allocTryUnsafeTarget(self: *MonoLlvmCodeGen, target: LocalId) Error!TryUnsafeTarget {
        const allocated = try self.allocAggregateTarget(target);
        const ret_layout_val = self.layoutValue(allocated.layout_idx);
        if (ret_layout_val.tag != .struct_) return error.CompilationFailed;
        const struct_idx = ret_layout_val.getStruct().idx;
        return .{
            .ptr = allocated.ptr,
            .success_offset = self.layouts().getStructFieldOffsetByOriginalIndex(struct_idx, 0),
            .value_offset = self.layouts().getStructFieldOffsetByOriginalIndex(struct_idx, 1),
        };
    }

    fn emitFloatToIntTryUnsafeConversion(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const info = try self.tryUnsafeRecordInfo(self.localLayout(target));
        const arg_layout = self.localLayout(arg);
        const value = try self.loadScalar(self.slot(arg).ptr, arg_layout);
        const is_f32 = arg_layout == .f32;
        const float_ty: LlvmBuilder.Type = if (is_f32) .float else .double;

        try self.callBuiltinVoid(
            if (is_f32) builtinSymbol(.f32_to_int_try_unsafe) else builtinSymbol(.f64_to_int_try_unsafe),
            &.{ try self.ptrType(), float_ty, .i32, .i32, .i32, .i32, .i32 },
            &.{
                self.slot(target).ptr,
                value,
                builder.intValue(.i32, self.intBits(info.value_layout)) catch return error.OutOfMemory,
                builder.intValue(.i32, @intFromBool(info.value_layout.isSigned())) catch return error.OutOfMemory,
                builder.intValue(.i32, info.value_size) catch return error.OutOfMemory,
                builder.intValue(.i32, info.success_offset) catch return error.OutOfMemory,
                builder.intValue(.i32, info.value_offset) catch return error.OutOfMemory,
            },
        );
    }

    fn emitDecAttosMove(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const value = try self.loadScalar(self.slot(arg).ptr, self.localLayout(arg));
        try self.storeScalar(self.slot(target).ptr, self.localLayout(target), value);
    }

    fn emitF64ToF32TryUnsafeConversion(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const allocated = try self.allocTryUnsafeTarget(target);
        const value = try self.loadScalar(self.slot(arg).ptr, .f64);

        try self.callBuiltinVoid(
            builtinSymbol(.f64_to_f32_try_unsafe),
            &.{ try self.ptrType(), .double, .i32, .i32 },
            &.{
                allocated.ptr,
                value,
                builder.intValue(.i32, allocated.success_offset) catch return error.OutOfMemory,
                builder.intValue(.i32, allocated.value_offset) catch return error.OutOfMemory,
            },
        );
    }

    fn emitDecToF32TryUnsafeConversion(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const allocated = try self.allocTryUnsafeTarget(target);
        const value = try self.loadScalar(self.slot(arg).ptr, .dec);
        const parts = try self.splitI128Value(value);

        try self.callBuiltinVoid(
            builtinSymbol(.dec_to_f32_try_unsafe),
            &.{ try self.ptrType(), .i64, .i64, .i32, .i32 },
            &.{
                allocated.ptr,
                parts.low,
                parts.high,
                builder.intValue(.i32, allocated.success_offset) catch return error.OutOfMemory,
                builder.intValue(.i32, allocated.value_offset) catch return error.OutOfMemory,
            },
        );
    }

    fn emitInt128ToDecTryUnsafeConversion(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId, is_signed: bool) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const allocated = try self.allocTryUnsafeTarget(target);
        const value = try self.loadScalar(self.slot(arg).ptr, self.localLayout(arg));
        const parts = try self.splitI128Value(value);

        try self.callBuiltinVoid(
            LowLevelBuiltins.int128ToDec(is_signed).symbolName(),
            &.{ try self.ptrType(), .i64, .i64, .i32, .i32 },
            &.{
                allocated.ptr,
                parts.low,
                parts.high,
                builder.intValue(.i32, allocated.success_offset) catch return error.OutOfMemory,
                builder.intValue(.i32, allocated.value_offset) catch return error.OutOfMemory,
            },
        );
    }

    fn emitDecToIntTryUnsafeConversion(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const info = try self.tryUnsafeRecordInfo(self.localLayout(target));
        const value = try self.loadScalar(self.slot(arg).ptr, .dec);
        const parts = try self.splitI128Value(value);

        try self.callBuiltinVoid(
            builtinSymbol(.dec_to_int_try_unsafe),
            &.{ try self.ptrType(), .i64, .i64, .i32, .i32, .i32, .i32, .i32 },
            &.{
                self.slot(target).ptr,
                parts.low,
                parts.high,
                builder.intValue(.i32, self.intBits(info.value_layout)) catch return error.OutOfMemory,
                builder.intValue(.i32, @intFromBool(info.value_layout.isSigned())) catch return error.OutOfMemory,
                builder.intValue(.i32, info.value_size) catch return error.OutOfMemory,
                builder.intValue(.i32, info.success_offset) catch return error.OutOfMemory,
                builder.intValue(.i32, info.value_offset) catch return error.OutOfMemory,
            },
        );
    }

    /// Emits the LLVM switch instruction and queues each branch body (and the
    /// default body) as work items. The case blocks and branch slice are carried
    /// to the continuations via a heap `SwitchState` freed by `.switch_free`.
    fn emitSwitch(self: *MonoLlvmCodeGen, sw: anytype, wa: Allocator, work: *std.ArrayList(StmtWork)) Error!void {
        try self.materializeLocalIfDeferred(sw.cond);
        try self.materializeAllDeferredStrCaptures();
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const branches = self.store.getCFSwitchBranches(sw.branches);
        const default_block = wip.block(0, "switch_default") catch return error.OutOfMemory;
        const branch_blocks = try self.allocator.alloc(LlvmBuilder.Function.Block.Index, branches.len);
        for (branch_blocks) |*block| block.* = wip.block(0, "switch_case") catch return error.OutOfMemory;
        const cond = try self.readSwitchValue(self.slot(sw.cond).ptr, self.localLayout(sw.cond));
        if (sw.default_is_cold and branches.len == 1) {
            const branch = GuardedList.at(branches, 0);
            const expected = builder.intValue(cond.typeOfWip(wip), branch.value) catch return error.OutOfMemory;
            const is_branch = wip.icmp(.eq, cond, expected, "") catch return error.OutOfMemory;
            _ = wip.brCond(is_branch, branch_blocks[0], default_block, .then_likely) catch return error.OutOfMemory;
        } else {
            var switch_inst = wip.@"switch"(cond, default_block, @intCast(branches.len), .none) catch return error.OutOfMemory;
            for (0..branches.len) |i| {
                const branch = GuardedList.at(branches, i);
                const block = branch_blocks[i];
                switch_inst.addCase(builder.intConst(cond.typeOfWip(wip), branch.value) catch return error.OutOfMemory, block, wip) catch return error.OutOfMemory;
            }
            switch_inst.finish(wip);
        }

        const state = try self.allocator.create(SwitchState);
        state.* = .{
            .branches = branches,
            .branch_blocks = branch_blocks,
            .default_block = default_block,
            .default_branch = sw.default_branch,
            .default_is_cold = sw.default_is_cold,
        };
        if (branches.len == 0) {
            try work.append(wa, .{ .switch_default = state });
        } else {
            try work.append(wa, .{ .switch_branch = .{ .state = state, .index = 0 } });
        }
    }

    fn emitInitializedPayloadSwitch(self: *MonoLlvmCodeGen, sw: anytype, wa: Allocator, work: *std.ArrayList(StmtWork)) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const initialized_block = wip.block(0, "payload_initialized") catch return error.OutOfMemory;
        const uninitialized_block = wip.block(0, "payload_uninitialized") catch return error.OutOfMemory;
        const cond_value = try self.readSwitchValue(self.slot(sw.cond).ptr, self.localLayout(sw.cond));
        const mask = builder.intValue(cond_value.typeOfWip(wip), sw.cond_mask) catch return error.OutOfMemory;
        const masked = wip.bin(.@"and", cond_value, mask, "") catch return error.OutOfMemory;
        const is_initialized = wip.icmp(.eq, masked, mask, "") catch return error.OutOfMemory;
        _ = wip.brCond(is_initialized, initialized_block, uninitialized_block, if (sw.uninitialized_is_cold) .then_likely else .none) catch return error.OutOfMemory;

        const state = try self.allocator.create(InitializedPayloadSwitchState);
        state.* = .{
            .initialized_block = initialized_block,
            .uninitialized_block = uninitialized_block,
            .initialized_branch = sw.initialized_branch,
            .uninitialized_branch = sw.uninitialized_branch,
        };
        try work.append(wa, .{ .initialized_payload_branch = .{ .state = state, .initialized = true } });
    }

    fn emitDecrefIfInitialized(
        self: *MonoLlvmCodeGen,
        cond: LocalId,
        cond_mask: u64,
        value: LocalId,
        helper: lir.LIR.RcHelper,
        atomicity: lir.LIR.RcAtomicity,
    ) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const release_block = wip.block(0, "payload_cond_decref") catch return error.OutOfMemory;
        const next_block = wip.block(0, "payload_cond_next") catch return error.OutOfMemory;
        const cond_value = try self.readSwitchValue(self.slot(cond).ptr, self.localLayout(cond));
        const mask = builder.intValue(cond_value.typeOfWip(wip), cond_mask) catch return error.OutOfMemory;
        const masked = wip.bin(.@"and", cond_value, mask, "") catch return error.OutOfMemory;
        const is_initialized = wip.icmp(.eq, masked, mask, "") catch return error.OutOfMemory;
        _ = wip.brCond(is_initialized, release_block, next_block, .then_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = release_block };
        try self.emitExplicitRcStmt(helper, .decref, value, 1, atomicity);
        if (!self.currentBlockHasTerminator()) {
            _ = wip.br(next_block) catch return error.OutOfMemory;
        }
        wip.cursor = .{ .block = next_block };
    }

    const StrMatchSource = struct {
        bytes: LlvmBuilder.Value,
        len: LlvmBuilder.Value,
        is_small: LlvmBuilder.Value,
        alloc: LlvmBuilder.Value,
    };

    const StrByteSlice = struct {
        bytes: LlvmBuilder.Value,
        len: LlvmBuilder.Value,
    };

    const StrDropEdge = enum {
        prefix,
        suffix,
    };

    const DeferredStrCapture = struct {
        source_local: LocalId,
        source: StrMatchSource,
        start_ptr: LlvmBuilder.Value,
        end_ptr: LlvmBuilder.Value,
        pending_rc_count: u16,
        pending_rc_atomicity: RcAtomicity,
    };

    const DeferredStrCaptureBinding = struct {
        local: LocalId,
        capture: DeferredStrCapture,
    };

    const StrMatchCaptureSlots = struct {
        start_ptr: LlvmBuilder.Value,
        end_ptr: LlvmBuilder.Value,
    };

    fn emitStrMatch(self: *MonoLlvmCodeGen, str_match: anytype, wa: Allocator, work: *std.ArrayList(StmtWork)) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;

        try self.materializeAllDeferredStrCaptures();
        try self.materializeLocalIfDeferred(str_match.source);

        const on_miss_block = wip.block(0, "str_match_miss") catch return error.OutOfMemory;
        const source = try self.emitStrMatchSourceShape(self.slot(str_match.source).ptr);
        const match_body = try self.emitStrMatchArmTest(str_match.source, source, str_match, on_miss_block);
        try work.append(wa, .{ .str_match_body = .{ .block = on_miss_block, .stmt = str_match.on_miss } });
        try work.append(wa, .{ .str_match_body = match_body });
    }

    fn emitStrMatchSet(self: *MonoLlvmCodeGen, str_match_set: anytype, wa: Allocator, work: *std.ArrayList(StmtWork)) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;

        try self.materializeAllDeferredStrCaptures();
        try self.materializeLocalIfDeferred(str_match_set.source);

        const arms = self.store.getStrMatchArms(str_match_set.arms);
        const final_miss_block = wip.block(0, "str_match_set_miss") catch return error.OutOfMemory;
        const source = try self.emitStrMatchSourceShape(self.slot(str_match_set.source).ptr);

        var match_bodies = std.ArrayList(StrMatchBody).empty;
        errdefer {
            for (match_bodies.items) |body| {
                if (body.captures.len > 0) self.allocator.free(body.captures);
            }
            match_bodies.deinit(self.allocator);
        }

        var index: usize = 0;
        while (index < arms.len) {
            const arm = GuardedList.at(arms, index);
            const prefix = self.store.getStringLiteral(arm.prefix);
            if (prefix.len == 0) {
                const next_miss_block = if (index + 1 == arms.len)
                    final_miss_block
                else
                    wip.block(0, "str_match_set_next") catch return error.OutOfMemory;
                try match_bodies.append(self.allocator, try self.emitStrMatchArmTest(str_match_set.source, source, arm, next_miss_block));
                if (index + 1 < arms.len) {
                    wip.cursor = .{ .block = next_miss_block };
                }
                index += 1;
                continue;
            }

            var segment_end = index + 1;
            while (segment_end < arms.len and self.store.getStringLiteral(GuardedList.at(arms, segment_end).prefix).len > 0) {
                segment_end += 1;
            }

            const segment_miss_block = if (segment_end == arms.len)
                final_miss_block
            else
                wip.block(0, "str_match_set_next") catch return error.OutOfMemory;

            if (self.strMatchSetDistinctFirstByteCount(arms, index, segment_end) > 1) {
                try self.emitStrMatchSetFirstByteDispatch(str_match_set.source, source, arms, index, segment_end, segment_miss_block, &match_bodies);
            } else {
                try self.emitStrMatchSetLinearRange(str_match_set.source, source, arms, index, segment_end, segment_miss_block, &match_bodies);
            }

            if (segment_end < arms.len) {
                wip.cursor = .{ .block = segment_miss_block };
            }
            index = segment_end;
        }

        try work.append(wa, .{ .str_match_body = .{ .block = final_miss_block, .stmt = str_match_set.on_miss } });
        const owned_bodies = try match_bodies.toOwnedSlice(self.allocator);
        var handed_off = false;
        errdefer if (!handed_off) {
            for (owned_bodies) |body| {
                if (body.captures.len > 0) self.allocator.free(body.captures);
            }
            self.allocator.free(owned_bodies);
        };
        for (owned_bodies) |body| {
            try work.append(wa, .{ .str_match_body = body });
        }
        handed_off = true;
        self.allocator.free(owned_bodies);
    }

    fn strMatchSetDistinctFirstByteCount(self: *MonoLlvmCodeGen, arms: anytype, start: usize, end: usize) usize {
        var seen = [_]bool{false} ** 256;
        var count: usize = 0;
        for (start..end) |i| {
            const arm = GuardedList.at(arms, i);
            const prefix = self.store.getStringLiteral(arm.prefix);
            if (prefix.len == 0) continue;
            const byte = prefix[0];
            if (!seen[byte]) {
                seen[byte] = true;
                count += 1;
            }
        }
        return count;
    }

    fn emitStrMatchSetLinearRange(
        self: *MonoLlvmCodeGen,
        source_local: LocalId,
        source: StrMatchSource,
        arms: anytype,
        start: usize,
        end: usize,
        miss_block: LlvmBuilder.Function.Block.Index,
        match_bodies: *std.ArrayList(StrMatchBody),
    ) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;

        for (start..end) |index| {
            const arm = GuardedList.at(arms, index);
            const next_miss_block = if (index + 1 == end)
                miss_block
            else
                wip.block(0, "str_match_set_next") catch return error.OutOfMemory;
            try match_bodies.append(self.allocator, try self.emitStrMatchArmTest(source_local, source, arm, next_miss_block));
            if (index + 1 < end) {
                wip.cursor = .{ .block = next_miss_block };
            }
        }
    }

    fn emitStrMatchSetFirstByteDispatch(
        self: *MonoLlvmCodeGen,
        source_local: LocalId,
        source: StrMatchSource,
        arms: anytype,
        start: usize,
        end: usize,
        miss_block: LlvmBuilder.Function.Block.Index,
        match_bodies: *std.ArrayList(StrMatchBody),
    ) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const usize_ty = self.ptrSizedIntType();

        var bucket_blocks = [_]?LlvmBuilder.Function.Block.Index{null} ** 256;
        var bucket_bytes = std.ArrayList(u8).empty;
        defer bucket_bytes.deinit(self.allocator);

        for (start..end) |i| {
            const arm = GuardedList.at(arms, i);
            const prefix = self.store.getStringLiteral(arm.prefix);
            const byte = prefix[0];
            if (bucket_blocks[byte] == null) {
                bucket_blocks[byte] = wip.block(0, "str_match_set_first_byte") catch return error.OutOfMemory;
                try bucket_bytes.append(self.allocator, byte);
            }
        }

        const dispatch_block = wip.block(0, "str_match_set_dispatch") catch return error.OutOfMemory;
        const zero = builder.intValue(usize_ty, 0) catch return error.OutOfMemory;
        const has_first_byte = wip.icmp(.ugt, source.len, zero, "") catch return error.OutOfMemory;
        _ = wip.brCond(has_first_byte, dispatch_block, miss_block, .then_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = dispatch_block };
        const first_byte = wip.load(.normal, .i8, source.bytes, LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
        var switch_inst = wip.@"switch"(first_byte, miss_block, @intCast(bucket_bytes.items.len), .none) catch return error.OutOfMemory;
        for (bucket_bytes.items) |byte| {
            const block = bucket_blocks[byte] orelse return error.CompilationFailed;
            switch_inst.addCase(builder.intConst(.i8, byte) catch return error.OutOfMemory, block, wip) catch return error.OutOfMemory;
        }
        switch_inst.finish(wip);

        for (bucket_bytes.items) |byte| {
            const bucket_block = bucket_blocks[byte] orelse return error.CompilationFailed;
            wip.cursor = .{ .block = bucket_block };
            var remaining_in_bucket: usize = 0;
            for (start..end) |i| {
                const arm = GuardedList.at(arms, i);
                if (self.store.getStringLiteral(arm.prefix)[0] == byte) remaining_in_bucket += 1;
            }

            var emitted_in_bucket: usize = 0;
            for (start..end) |i| {
                const arm = GuardedList.at(arms, i);
                if (self.store.getStringLiteral(arm.prefix)[0] != byte) continue;
                emitted_in_bucket += 1;
                const next_miss_block = if (emitted_in_bucket == remaining_in_bucket)
                    miss_block
                else
                    wip.block(0, "str_match_set_next") catch return error.OutOfMemory;
                try match_bodies.append(self.allocator, try self.emitStrMatchArmTest(source_local, source, arm, next_miss_block));
                if (emitted_in_bucket < remaining_in_bucket) {
                    wip.cursor = .{ .block = next_miss_block };
                }
            }
        }
    }

    fn emitStrMatchArmTest(
        self: *MonoLlvmCodeGen,
        source_local: LocalId,
        source: StrMatchSource,
        arm: anytype,
        miss_block: LlvmBuilder.Function.Block.Index,
    ) Error!StrMatchBody {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const usize_ty = self.ptrSizedIntType();
        const usize_alignment = self.targetPointerAlignment();
        const on_match_block = wip.block(0, "str_match_success") catch return error.OutOfMemory;
        const steps = self.store.getStrMatchSteps(arm.steps);

        const capture_slots = try self.allocator.alloc(?StrMatchCaptureSlots, steps.len);
        defer self.allocator.free(capture_slots);
        for (0..steps.len) |i| {
            const step = GuardedList.at(steps, i);
            capture_slots[i] = switch (step.capture) {
                .discard => null,
                .view => .{
                    .start_ptr = try self.allocEntryBlockSlot(usize_ty, 1, usize_alignment, "str_match_capture_start"),
                    .end_ptr = try self.allocEntryBlockSlot(usize_ty, 1, usize_alignment, "str_match_capture_end"),
                },
            };
        }

        const cursor_ptr = try self.allocEntryBlockSlot(usize_ty, 1, usize_alignment, "str_match_cursor");
        const zero = builder.intValue(usize_ty, 0) catch return error.OutOfMemory;
        try self.storeUsize(cursor_ptr, zero);

        var captures = std.ArrayList(DeferredStrCaptureBinding).empty;
        errdefer captures.deinit(self.allocator);

        const prefix = self.store.getStringLiteral(arm.prefix);
        if (prefix.len > 0) {
            try self.emitStrMatchCheckAvailable(cursor_ptr, source.len, prefix.len, miss_block);
            try self.emitStrMatchCompareLiteral(source.bytes, prefix, miss_block);
            try self.storeUsize(cursor_ptr, builder.intValue(usize_ty, prefix.len) catch return error.OutOfMemory);
        }

        for (0..steps.len) |step_i| {
            const step = GuardedList.at(steps, step_i);
            const capture_start = try self.loadUsize(cursor_ptr);
            const delimiter = self.store.getStringLiteral(step.delimiter);
            const is_final_tail_capture = arm.end == .tail and step_i + 1 == steps.len and delimiter.len == 0;

            const capture_end = if (is_final_tail_capture) blk: {
                try self.storeUsize(cursor_ptr, source.len);
                break :blk source.len;
            } else blk: {
                try self.emitStrMatchFindDelimiter(source.bytes, source.len, cursor_ptr, delimiter, miss_block);
                break :blk try self.loadUsize(cursor_ptr);
            };

            switch (step.capture) {
                .discard => {},
                .view => |local| {
                    const slots = capture_slots[step_i] orelse return error.CompilationFailed;
                    try self.storeUsize(slots.start_ptr, capture_start);
                    try self.storeUsize(slots.end_ptr, capture_end);
                    try captures.append(self.allocator, .{
                        .local = local,
                        .capture = .{
                            .source_local = source_local,
                            .source = source,
                            .start_ptr = slots.start_ptr,
                            .end_ptr = slots.end_ptr,
                            .pending_rc_count = 0,
                            .pending_rc_atomicity = .atomic,
                        },
                    });
                },
            }

            if (!is_final_tail_capture and delimiter.len > 0) {
                const advanced = wip.bin(.add, try self.loadUsize(cursor_ptr), builder.intValue(usize_ty, delimiter.len) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
                try self.storeUsize(cursor_ptr, advanced);
            }
        }

        switch (arm.end) {
            .exact => {
                const at_end = wip.icmp(.eq, try self.loadUsize(cursor_ptr), source.len, "") catch return error.OutOfMemory;
                _ = wip.brCond(at_end, on_match_block, miss_block, .then_likely) catch return error.OutOfMemory;
            },
            .tail => _ = wip.br(on_match_block) catch return error.OutOfMemory,
        }

        const match_captures = try captures.toOwnedSlice(self.allocator);
        errdefer self.allocator.free(match_captures);
        return .{ .block = on_match_block, .stmt = arm.on_match, .captures = match_captures };
    }

    fn emitStrMatchSourceShape(self: *MonoLlvmCodeGen, source_ptr: LlvmBuilder.Value) Error!StrMatchSource {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const usize_ty = self.ptrSizedIntType();
        const ptr_ty = try self.ptrType();
        const word_size = self.targetWordSize();

        const raw_len = try self.loadUsize(try self.offsetPtr(source_ptr, self.rocStrLenOffset()));
        const small_bit = @as(u64, 1) << @intCast(self.target.ptrBitWidth() - 1);
        const small_mask = wip.bin(.@"and", raw_len, builder.intValue(usize_ty, small_bit) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const is_small = wip.icmp(.ne, small_mask, builder.intValue(usize_ty, 0) catch return error.OutOfMemory, "") catch return error.OutOfMemory;

        const small_block = wip.block(0, "str_match_source_small") catch return error.OutOfMemory;
        const heap_block = wip.block(0, "str_match_source_heap") catch return error.OutOfMemory;
        const after_block = wip.block(2, "str_match_source_after") catch return error.OutOfMemory;
        _ = wip.brCond(is_small, small_block, heap_block, .then_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = small_block };
        const small_len_byte = wip.load(.normal, .i8, try self.offsetPtr(source_ptr, 3 * word_size - 1), LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
        const small_len_masked = wip.bin(.@"and", small_len_byte, builder.intValue(.i8, 0x7F) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const small_len = try self.coerceScalar(small_len_masked, usize_ty, false);
        const zero = builder.intValue(usize_ty, 0) catch return error.OutOfMemory;
        _ = wip.br(after_block) catch return error.OutOfMemory;

        wip.cursor = .{ .block = heap_block };
        const heap_bytes = try self.loadPointer(source_ptr);
        const cap_or_alloc = try self.loadUsize(try self.offsetPtr(source_ptr, self.rocStrCapacityOffset()));
        const slice_tag = wip.bin(.@"and", cap_or_alloc, builder.intValue(usize_ty, 1) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const is_slice = wip.icmp(.ne, slice_tag, builder.intValue(usize_ty, 0) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const heap_bytes_int = wip.cast(.ptrtoint, heap_bytes, usize_ty, "") catch return error.OutOfMemory;
        const encoded_heap_ptr = wip.bin(.@"or", heap_bytes_int, builder.intValue(usize_ty, 1) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const heap_alloc = wip.select(.normal, is_slice, cap_or_alloc, encoded_heap_ptr, "") catch return error.OutOfMemory;
        _ = wip.br(after_block) catch return error.OutOfMemory;

        wip.cursor = .{ .block = after_block };
        const bytes_phi = wip.phi(ptr_ty, "str_match_bytes") catch return error.OutOfMemory;
        bytes_phi.finish(&.{ source_ptr, heap_bytes }, &.{ small_block, heap_block }, wip);
        const len_phi = wip.phi(usize_ty, "str_match_len") catch return error.OutOfMemory;
        len_phi.finish(&.{ small_len, raw_len }, &.{ small_block, heap_block }, wip);
        const alloc_phi = wip.phi(usize_ty, "str_match_alloc") catch return error.OutOfMemory;
        alloc_phi.finish(&.{ zero, heap_alloc }, &.{ small_block, heap_block }, wip);

        return .{
            .bytes = bytes_phi.toValue(),
            .len = len_phi.toValue(),
            .is_small = is_small,
            .alloc = alloc_phi.toValue(),
        };
    }

    fn emitRocStrLen(self: *MonoLlvmCodeGen, str_ptr: LlvmBuilder.Value) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const usize_ty = self.ptrSizedIntType();
        const word_size = self.targetWordSize();

        const raw_len = try self.loadUsize(try self.offsetPtr(str_ptr, self.rocStrLenOffset()));
        const small_bit = @as(u64, 1) << @intCast(self.target.ptrBitWidth() - 1);
        const small_mask = wip.bin(.@"and", raw_len, builder.intValue(usize_ty, small_bit) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const is_small = wip.icmp(.ne, small_mask, builder.intValue(usize_ty, 0) catch return error.OutOfMemory, "") catch return error.OutOfMemory;

        const small_len_byte = wip.load(.normal, .i8, try self.offsetPtr(str_ptr, 3 * word_size - 1), LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
        const small_len_masked = wip.bin(.@"and", small_len_byte, builder.intValue(.i8, 0x7F) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const small_len = try self.coerceScalar(small_len_masked, usize_ty, false);

        return wip.select(.normal, is_small, small_len, raw_len, "") catch return error.OutOfMemory;
    }

    fn emitStrMatchFailIf(self: *MonoLlvmCodeGen, cond: LlvmBuilder.Value, fail_block: LlvmBuilder.Function.Block.Index) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const cont = wip.block(0, "str_match_continue") catch return error.OutOfMemory;
        _ = wip.brCond(cond, fail_block, cont, .none) catch return error.OutOfMemory;
        wip.cursor = .{ .block = cont };
    }

    fn emitStrMatchCheckAvailable(
        self: *MonoLlvmCodeGen,
        cursor_ptr: LlvmBuilder.Value,
        len: LlvmBuilder.Value,
        needed: usize,
        fail_block: LlvmBuilder.Function.Block.Index,
    ) Error!void {
        if (needed == 0) return;
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const usize_ty = self.ptrSizedIntType();
        const needed_value = builder.intValue(usize_ty, needed) catch return error.OutOfMemory;
        try self.emitStrMatchFailIf(wip.icmp(.ult, len, needed_value, "") catch return error.OutOfMemory, fail_block);
        const limit = wip.bin(.sub, len, needed_value, "") catch return error.OutOfMemory;
        try self.emitStrMatchFailIf(wip.icmp(.ugt, try self.loadUsize(cursor_ptr), limit, "") catch return error.OutOfMemory, fail_block);
    }

    fn emitStrMatchCompareLiteral(
        self: *MonoLlvmCodeGen,
        ptr: LlvmBuilder.Value,
        literal: []const u8,
        fail_block: LlvmBuilder.Function.Block.Index,
    ) Error!void {
        if (literal.len == 0) return;

        var mismatch: ?LlvmBuilder.Value = null;
        var offset: usize = 0;
        while (literal.len - offset >= 16) : (offset += 16) {
            mismatch = try self.emitStrMatchAccumulateLiteralMismatch(ptr, literal, offset, 16, mismatch);
        }

        if (offset < literal.len) {
            const remaining = literal.len - offset;
            switch (remaining) {
                1 => mismatch = try self.emitStrMatchAccumulateLiteralMismatch(ptr, literal, offset, 1, mismatch),
                2 => mismatch = try self.emitStrMatchAccumulateLiteralMismatch(ptr, literal, offset, 2, mismatch),
                3 => {
                    mismatch = try self.emitStrMatchAccumulateLiteralMismatch(ptr, literal, offset, 2, mismatch);
                    mismatch = try self.emitStrMatchAccumulateLiteralMismatch(ptr, literal, offset + 1, 2, mismatch);
                },
                4 => mismatch = try self.emitStrMatchAccumulateLiteralMismatch(ptr, literal, offset, 4, mismatch),
                5...7 => {
                    mismatch = try self.emitStrMatchAccumulateLiteralMismatch(ptr, literal, offset, 4, mismatch);
                    mismatch = try self.emitStrMatchAccumulateLiteralMismatch(ptr, literal, literal.len - 4, 4, mismatch);
                },
                8 => mismatch = try self.emitStrMatchAccumulateLiteralMismatch(ptr, literal, offset, 8, mismatch),
                9...15 => {
                    mismatch = try self.emitStrMatchAccumulateLiteralMismatch(ptr, literal, offset, 8, mismatch);
                    mismatch = try self.emitStrMatchAccumulateLiteralMismatch(ptr, literal, literal.len - 8, 8, mismatch);
                },
                else => return error.CompilationFailed,
            }
        }

        if (mismatch) |value| {
            try self.emitStrMatchFailIf(value, fail_block);
        }
    }

    fn emitStrMatchAccumulateLiteralMismatch(
        self: *MonoLlvmCodeGen,
        ptr: LlvmBuilder.Value,
        literal: []const u8,
        offset: usize,
        chunk_len: usize,
        previous: ?LlvmBuilder.Value,
    ) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;

        const mismatch = try self.emitStrMatchLiteralChunkMismatch(ptr, literal, offset, chunk_len);
        if (previous) |prev| {
            return wip.bin(.@"or", prev, mismatch, "") catch return error.OutOfMemory;
        }
        return mismatch;
    }

    fn emitStrMatchLiteralChunkMismatch(
        self: *MonoLlvmCodeGen,
        ptr: LlvmBuilder.Value,
        literal: []const u8,
        offset: usize,
        chunk_len: usize,
    ) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;

        if (chunk_len == 0 or offset + chunk_len > literal.len) return error.CompilationFailed;
        const chunk_ty = intTypeForBytes(@intCast(chunk_len));
        const chunk_ptr = try self.offsetPtrValue(ptr, builder.intValue(self.ptrSizedIntType(), offset) catch return error.OutOfMemory);
        const actual = wip.load(.normal, chunk_ty, chunk_ptr, LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
        const expected = builder.intValue(chunk_ty, littleEndianInt(literal[offset..][0..chunk_len])) catch return error.OutOfMemory;
        return wip.icmp(.ne, actual, expected, "") catch return error.OutOfMemory;
    }

    fn emitStrMatchProbeDelimiterByte(
        self: *MonoLlvmCodeGen,
        bytes: LlvmBuilder.Value,
        cursor: LlvmBuilder.Value,
        cursor_ptr: LlvmBuilder.Value,
        delimiter_byte: u8,
        found_block: LlvmBuilder.Function.Block.Index,
        fail_block: LlvmBuilder.Function.Block.Index,
    ) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        try self.emitStrMatchProbeDelimiterByteValue(
            bytes,
            cursor,
            cursor_ptr,
            builder.intValue(.i8, delimiter_byte) catch return error.OutOfMemory,
            found_block,
            fail_block,
        );
    }

    fn emitStrMatchProbeDelimiterByteValue(
        self: *MonoLlvmCodeGen,
        bytes: LlvmBuilder.Value,
        cursor: LlvmBuilder.Value,
        cursor_ptr: LlvmBuilder.Value,
        delimiter_byte: LlvmBuilder.Value,
        found_block: LlvmBuilder.Function.Block.Index,
        fail_block: LlvmBuilder.Function.Block.Index,
    ) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;

        const candidate = try self.offsetPtrValue(bytes, cursor);
        const actual = wip.load(.normal, .i8, candidate, LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
        const first_matches = wip.icmp(.eq, actual, delimiter_byte, "") catch return error.OutOfMemory;
        const matched_block = wip.block(0, "str_match_scan_tail_match") catch return error.OutOfMemory;
        _ = wip.brCond(first_matches, matched_block, fail_block, .none) catch return error.OutOfMemory;

        wip.cursor = .{ .block = matched_block };
        try self.storeUsize(cursor_ptr, cursor);
        _ = wip.br(found_block) catch return error.OutOfMemory;
    }

    fn emitStrMatchWordByteMaskValue(
        self: *MonoLlvmCodeGen,
        word: LlvmBuilder.Value,
        byte: LlvmBuilder.Value,
        width: u8,
    ) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const word_ty = intTypeForBytes(width);
        const repeated = wip.bin(
            .mul,
            try self.coerceScalar(byte, word_ty, false),
            builder.intValue(word_ty, repeatedByte(0x01, width)) catch return error.OutOfMemory,
            "",
        ) catch return error.OutOfMemory;
        const ones = builder.intValue(word_ty, repeatedByte(0x01, width)) catch return error.OutOfMemory;
        const high_bits = builder.intValue(word_ty, repeatedByte(0x80, width)) catch return error.OutOfMemory;

        const x = wip.bin(.xor, word, repeated, "") catch return error.OutOfMemory;
        const minus_ones = wip.bin(.sub, x, ones, "") catch return error.OutOfMemory;
        const not_x = wip.not(x, "") catch return error.OutOfMemory;
        const candidate = wip.bin(.@"and", minus_ones, not_x, "") catch return error.OutOfMemory;
        return wip.bin(.@"and", candidate, high_bits, "") catch return error.OutOfMemory;
    }

    fn emitStrMatchFirstSetByteOffset(
        self: *MonoLlvmCodeGen,
        mask: LlvmBuilder.Value,
        width: u8,
    ) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const word_ty = intTypeForBytes(width);
        const trailing = wip.callIntrinsic(
            .normal,
            .none,
            .cttz,
            &.{word_ty},
            &.{ mask, builder.intValue(.i1, 1) catch return error.OutOfMemory },
            "",
        ) catch return error.OutOfMemory;
        const byte_bits = wip.bin(.lshr, trailing, builder.intValue(word_ty, 3) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        return self.coerceScalar(byte_bits, self.ptrSizedIntType(), false);
    }

    fn emitStrMatchWordProbeDelimiter(
        self: *MonoLlvmCodeGen,
        bytes: LlvmBuilder.Value,
        cursor: LlvmBuilder.Value,
        cursor_ptr: LlvmBuilder.Value,
        delimiter_byte: u8,
        width: u8,
        found_block: LlvmBuilder.Function.Block.Index,
        no_match_block: LlvmBuilder.Function.Block.Index,
    ) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        try self.emitStrMatchWordProbeDelimiterValue(
            bytes,
            cursor,
            cursor_ptr,
            builder.intValue(.i8, delimiter_byte) catch return error.OutOfMemory,
            width,
            found_block,
            no_match_block,
        );
    }

    fn emitStrMatchWordProbeDelimiterValue(
        self: *MonoLlvmCodeGen,
        bytes: LlvmBuilder.Value,
        cursor: LlvmBuilder.Value,
        cursor_ptr: LlvmBuilder.Value,
        delimiter_byte: LlvmBuilder.Value,
        width: u8,
        found_block: LlvmBuilder.Function.Block.Index,
        no_match_block: LlvmBuilder.Function.Block.Index,
    ) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const word_ty = intTypeForBytes(width);
        const word_ptr = try self.offsetPtrValue(bytes, cursor);
        const word = wip.load(.normal, word_ty, word_ptr, LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
        const mask = try self.emitStrMatchWordByteMaskValue(word, delimiter_byte, width);
        const has_match = wip.icmp(.ne, mask, builder.intValue(word_ty, 0) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const matched_block = wip.block(0, "str_match_scan_word_match") catch return error.OutOfMemory;
        _ = wip.brCond(has_match, matched_block, no_match_block, .none) catch return error.OutOfMemory;

        wip.cursor = .{ .block = matched_block };
        const byte_offset = try self.emitStrMatchFirstSetByteOffset(mask, width);
        const found_cursor = wip.bin(.add, cursor, byte_offset, "") catch return error.OutOfMemory;
        try self.storeUsize(cursor_ptr, found_cursor);
        _ = wip.br(found_block) catch return error.OutOfMemory;
    }

    fn emitStrMatchCandidateCount(
        self: *MonoLlvmCodeGen,
        cursor: LlvmBuilder.Value,
        limit: LlvmBuilder.Value,
    ) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const usize_ty = self.ptrSizedIntType();
        const cursor_after_limit = wip.icmp(.ugt, cursor, limit, "") catch return error.OutOfMemory;
        const remaining = wip.bin(.sub, limit, cursor, "") catch return error.OutOfMemory;
        const count = wip.bin(.add, remaining, builder.intValue(usize_ty, 1) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        return wip.select(.normal, cursor_after_limit, builder.intValue(usize_ty, 0) catch return error.OutOfMemory, count, "") catch return error.OutOfMemory;
    }

    fn emitStrMatchBranchForCandidateCount(
        self: *MonoLlvmCodeGen,
        count: LlvmBuilder.Value,
        width: u8,
        word_block: LlvmBuilder.Function.Block.Index,
        tail_block: LlvmBuilder.Function.Block.Index,
        miss_block: LlvmBuilder.Function.Block.Index,
    ) Error!LlvmBuilder.Function.Block.Index {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const usize_ty = self.ptrSizedIntType();
        const has_word = wip.icmp(.uge, count, builder.intValue(usize_ty, width) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const maybe_tail_block = wip.block(0, "str_match_scan_maybe_tail") catch return error.OutOfMemory;
        _ = wip.brCond(has_word, word_block, maybe_tail_block, .then_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = maybe_tail_block };
        const has_tail = wip.icmp(.ne, count, builder.intValue(usize_ty, 0) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        _ = wip.brCond(has_tail, tail_block, miss_block, .none) catch return error.OutOfMemory;
        return maybe_tail_block;
    }

    fn strMatchScanWidth(self: *const MonoLlvmCodeGen) u8 {
        if (self.target.cpu.arch == .x86_64 or self.target.cpu.arch == .aarch64) return 16;
        return switch (self.target.ptrBitWidth()) {
            64 => 8,
            32 => 4,
            else => unreachable,
        };
    }

    fn emitStrMatchFindDelimiter(
        self: *MonoLlvmCodeGen,
        bytes: LlvmBuilder.Value,
        len: LlvmBuilder.Value,
        cursor_ptr: LlvmBuilder.Value,
        delimiter: []const u8,
        miss_block: LlvmBuilder.Function.Block.Index,
    ) Error!void {
        if (delimiter.len == 0) return;
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const usize_ty = self.ptrSizedIntType();
        const width = self.strMatchScanWidth();
        const delimiter_len = builder.intValue(usize_ty, delimiter.len) catch return error.OutOfMemory;
        try self.emitStrMatchFailIf(wip.icmp(.ult, len, delimiter_len, "") catch return error.OutOfMemory, miss_block);
        const limit = wip.bin(.sub, len, delimiter_len, "") catch return error.OutOfMemory;

        const found_block = wip.block(0, "str_match_scan_found") catch return error.OutOfMemory;
        const decide_block = wip.block(0, "str_match_scan_decide") catch return error.OutOfMemory;
        const word_loop_block = wip.block(2, "str_match_scan_word") catch return error.OutOfMemory;
        const word_after_block = wip.block(0, "str_match_scan_word_after") catch return error.OutOfMemory;
        const tail_block = wip.block(3, "str_match_scan_tail") catch return error.OutOfMemory;

        _ = wip.br(decide_block) catch return error.OutOfMemory;

        wip.cursor = .{ .block = decide_block };
        const initial_cursor = try self.loadUsize(cursor_ptr);
        const initial_count = try self.emitStrMatchCandidateCount(initial_cursor, limit);
        const initial_tail_pred = try self.emitStrMatchBranchForCandidateCount(initial_count, width, word_loop_block, tail_block, miss_block);

        wip.cursor = .{ .block = word_loop_block };
        const word_cursor_phi = wip.phi(usize_ty, "str_match_scan_word_cursor") catch return error.OutOfMemory;
        const word_cursor = word_cursor_phi.toValue();
        try self.emitStrMatchWordProbeDelimiter(bytes, word_cursor, cursor_ptr, delimiter[0], width, found_block, word_after_block);

        wip.cursor = .{ .block = word_after_block };
        const next_word_cursor = wip.bin(.add, word_cursor, builder.intValue(usize_ty, width) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const remaining_count = try self.emitStrMatchCandidateCount(next_word_cursor, limit);
        const word_tail_pred = try self.emitStrMatchBranchForCandidateCount(remaining_count, width, word_loop_block, tail_block, miss_block);

        word_cursor_phi.finish(&.{ initial_cursor, next_word_cursor }, &.{ decide_block, word_after_block }, wip);

        wip.cursor = .{ .block = tail_block };
        const tail_cursor_phi = wip.phi(usize_ty, "str_match_scan_tail_cursor") catch return error.OutOfMemory;
        const tail_cursor = tail_cursor_phi.toValue();
        const tail_after_probe_block = wip.block(0, "str_match_scan_tail_after_probe") catch return error.OutOfMemory;
        try self.emitStrMatchProbeDelimiterByte(bytes, tail_cursor, cursor_ptr, delimiter[0], found_block, tail_after_probe_block);

        wip.cursor = .{ .block = tail_after_probe_block };
        const next_tail_cursor = wip.bin(.add, tail_cursor, builder.intValue(usize_ty, 1) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const next_tail_count = try self.emitStrMatchCandidateCount(next_tail_cursor, limit);
        const has_next_tail = wip.icmp(.ne, next_tail_count, builder.intValue(usize_ty, 0) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        _ = wip.brCond(has_next_tail, tail_block, miss_block, .none) catch return error.OutOfMemory;

        tail_cursor_phi.finish(&.{ initial_cursor, next_word_cursor, next_tail_cursor }, &.{ initial_tail_pred, word_tail_pred, tail_after_probe_block }, wip);

        wip.cursor = .{ .block = found_block };
        if (delimiter.len > 1) {
            const found_cursor = try self.loadUsize(cursor_ptr);
            const found_candidate = try self.offsetPtrValue(bytes, found_cursor);
            const tail_ptr = try self.offsetPtrValue(found_candidate, builder.intValue(usize_ty, 1) catch return error.OutOfMemory);
            try self.emitStrMatchCompareLiteral(tail_ptr, delimiter[1..], miss_block);
        }
    }

    fn emitFindFirstByte(
        self: *MonoLlvmCodeGen,
        bytes: LlvmBuilder.Value,
        cursor_ptr: LlvmBuilder.Value,
        limit: LlvmBuilder.Value,
        byte: LlvmBuilder.Value,
        found_block: LlvmBuilder.Function.Block.Index,
        miss_block: LlvmBuilder.Function.Block.Index,
    ) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const usize_ty = self.ptrSizedIntType();
        const width = self.strMatchScanWidth();

        const decide_block = wip.block(0, "str_find_byte_decide") catch return error.OutOfMemory;
        const word_loop_block = wip.block(2, "str_find_byte_word") catch return error.OutOfMemory;
        const word_after_block = wip.block(0, "str_find_byte_word_after") catch return error.OutOfMemory;
        const tail_block = wip.block(3, "str_find_byte_tail") catch return error.OutOfMemory;

        _ = wip.br(decide_block) catch return error.OutOfMemory;

        wip.cursor = .{ .block = decide_block };
        const initial_cursor = try self.loadUsize(cursor_ptr);
        const initial_count = try self.emitStrMatchCandidateCount(initial_cursor, limit);
        const initial_tail_pred = try self.emitStrMatchBranchForCandidateCount(initial_count, width, word_loop_block, tail_block, miss_block);

        wip.cursor = .{ .block = word_loop_block };
        const word_cursor_phi = wip.phi(usize_ty, "str_find_byte_word_cursor") catch return error.OutOfMemory;
        const word_cursor = word_cursor_phi.toValue();
        try self.emitStrMatchWordProbeDelimiterValue(bytes, word_cursor, cursor_ptr, byte, width, found_block, word_after_block);

        wip.cursor = .{ .block = word_after_block };
        const next_word_cursor = wip.bin(.add, word_cursor, builder.intValue(usize_ty, width) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const remaining_count = try self.emitStrMatchCandidateCount(next_word_cursor, limit);
        const word_tail_pred = try self.emitStrMatchBranchForCandidateCount(remaining_count, width, word_loop_block, tail_block, miss_block);

        word_cursor_phi.finish(&.{ initial_cursor, next_word_cursor }, &.{ decide_block, word_after_block }, wip);

        wip.cursor = .{ .block = tail_block };
        const tail_cursor_phi = wip.phi(usize_ty, "str_find_byte_tail_cursor") catch return error.OutOfMemory;
        const tail_cursor = tail_cursor_phi.toValue();
        const tail_after_probe_block = wip.block(0, "str_find_byte_tail_after_probe") catch return error.OutOfMemory;
        try self.emitStrMatchProbeDelimiterByteValue(bytes, tail_cursor, cursor_ptr, byte, found_block, tail_after_probe_block);

        wip.cursor = .{ .block = tail_after_probe_block };
        const next_tail_cursor = wip.bin(.add, tail_cursor, builder.intValue(usize_ty, 1) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const next_tail_count = try self.emitStrMatchCandidateCount(next_tail_cursor, limit);
        const has_next_tail = wip.icmp(.ne, next_tail_count, builder.intValue(usize_ty, 0) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        _ = wip.brCond(has_next_tail, tail_block, miss_block, .none) catch return error.OutOfMemory;

        tail_cursor_phi.finish(&.{ initial_cursor, next_word_cursor, next_tail_cursor }, &.{ initial_tail_pred, word_tail_pred, tail_after_probe_block }, wip);
    }

    fn emitStoreStrMatchCapture(
        self: *MonoLlvmCodeGen,
        target_ptr: LlvmBuilder.Value,
        source: StrMatchSource,
        start: LlvmBuilder.Value,
        end: LlvmBuilder.Value,
    ) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const capture_len = wip.bin(.sub, end, start, "") catch return error.OutOfMemory;

        const small_block = wip.block(0, "str_match_capture_small") catch return error.OutOfMemory;
        const heap_block = wip.block(0, "str_match_capture_heap") catch return error.OutOfMemory;
        const after_block = wip.block(0, "str_match_capture_after") catch return error.OutOfMemory;
        _ = wip.brCond(source.is_small, small_block, heap_block, .then_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = small_block };
        try self.zeroBytes(target_ptr, 3 * self.targetWordSize());
        const small_src = try self.offsetPtrValue(source.bytes, start);
        _ = wip.callMemCpy(
            target_ptr,
            LlvmBuilder.Alignment.fromByteUnits(1),
            small_src,
            LlvmBuilder.Alignment.fromByteUnits(1),
            capture_len,
            .normal,
            false,
        ) catch return error.OutOfMemory;
        const len_byte = try self.coerceScalar(capture_len, .i8, false);
        const tagged_len = wip.bin(.@"or", len_byte, builder.intValue(.i8, 0x80) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const small_len_ptr = try self.offsetPtr(target_ptr, 3 * self.targetWordSize() - 1);
        _ = wip.store(.normal, tagged_len, small_len_ptr, LlvmBuilder.Alignment.fromByteUnits(1)) catch return error.OutOfMemory;
        _ = wip.br(after_block) catch return error.OutOfMemory;

        wip.cursor = .{ .block = heap_block };
        try self.storePointer(target_ptr, try self.offsetPtrValue(source.bytes, start));
        try self.storeUsize(try self.offsetPtr(target_ptr, self.rocStrCapacityOffset()), source.alloc);
        try self.storeUsize(try self.offsetPtr(target_ptr, self.rocStrLenOffset()), capture_len);
        _ = wip.br(after_block) catch return error.OutOfMemory;

        wip.cursor = .{ .block = after_block };
    }

    fn clearDeferredStrCaptures(self: *MonoLlvmCodeGen) void {
        for (self.deferred_str_capture_actives.items) |index| {
            self.deferred_str_captures[index] = null;
        }
        self.deferred_str_capture_count = 0;
        self.deferred_str_capture_actives.clearRetainingCapacity();
    }

    fn installDeferredStrCapture(self: *MonoLlvmCodeGen, local: LocalId, capture: DeferredStrCapture) Error!void {
        if (!self.isStrLocal(local)) return error.CompilationFailed;
        try self.prepareLocalWrite(local);
        const capture_slot = &self.deferred_str_captures[@intFromEnum(local)];
        if (capture_slot.* == null) {
            self.deferred_str_capture_actives.append(self.allocator, @intFromEnum(local)) catch return error.OutOfMemory;
        }
        capture_slot.* = capture;
    }

    fn deferredStrCapture(self: *MonoLlvmCodeGen, local: LocalId) ?DeferredStrCapture {
        if (self.deferred_str_captures.len == 0) return null;
        return self.deferred_str_captures[@intFromEnum(local)];
    }

    fn clearDeferredStrCapture(self: *MonoLlvmCodeGen, local: LocalId) void {
        if (self.deferred_str_captures.len == 0) return;
        if (self.deferred_str_captures[@intFromEnum(local)] != null) {
            self.deferred_str_captures[@intFromEnum(local)] = null;
            self.deferred_str_capture_count -= 1;
        }
    }

    fn materializeLocalIfDeferred(self: *MonoLlvmCodeGen, local: LocalId) Error!void {
        const capture = self.deferredStrCapture(local) orelse return;
        try self.emitStoreStrMatchCapture(
            self.slot(local).ptr,
            capture.source,
            try self.loadUsize(capture.start_ptr),
            try self.loadUsize(capture.end_ptr),
        );
        self.clearDeferredStrCapture(local);
        if (capture.pending_rc_count != 0) {
            try self.emitRcForLocal(.incref, capture.source_local, capture.pending_rc_count, capture.pending_rc_atomicity);
        }
    }

    fn materializeLocalSpanIfDeferred(self: *MonoLlvmCodeGen, locals: anytype) Error!void {
        for (0..locals.len) |i| {
            const local = GuardedList.at(locals, i);
            try self.materializeLocalIfDeferred(local);
        }
    }

    fn materializeAllDeferredStrCaptures(self: *MonoLlvmCodeGen) Error!void {
        if (self.deferred_str_capture_count == 0) return;
        var index: usize = 0;
        while (index < self.deferred_str_capture_actives.items.len) : (index += 1) {
            const local_index = self.deferred_str_capture_actives.items[index];
            if (self.deferred_str_captures[local_index] != null) {
                try self.materializeLocalIfDeferred(@enumFromInt(local_index));
            }
        }
    }

    fn materializeDeferredStrCaptureParams(self: *MonoLlvmCodeGen, params: LocalSpan) Error!void {
        const param_locals = self.store.getLocalSpan(params);
        for (0..param_locals.len) |i| {
            const param = GuardedList.at(param_locals, i);
            try self.materializeLocalIfDeferred(param);
        }
        self.clearDeferredStrCaptures();
    }

    fn materializeDeferredCapturesUsingSource(self: *MonoLlvmCodeGen, source: LocalId) Error!void {
        if (self.deferred_str_capture_count == 0) return;
        var index: usize = 0;
        while (index < self.deferred_str_capture_actives.items.len) : (index += 1) {
            const local_index = self.deferred_str_capture_actives.items[index];
            const capture = self.deferred_str_captures[local_index] orelse continue;
            if (capture.source_local == source and local_index != @intFromEnum(source)) {
                try self.materializeLocalIfDeferred(@enumFromInt(local_index));
            }
        }
    }

    fn prepareLocalWrite(self: *MonoLlvmCodeGen, local: LocalId) Error!void {
        try self.materializeDeferredCapturesUsingSource(local);
        self.clearDeferredStrCapture(local);
    }

    fn propagateDeferredStrCapture(self: *MonoLlvmCodeGen, target: LocalId, source: LocalId) Error!bool {
        const capture = self.deferredStrCapture(source) orelse return false;
        if (!self.isStrLocal(target)) return error.CompilationFailed;
        if (target != source) {
            try self.prepareLocalWrite(target);
            self.deferred_str_captures[@intFromEnum(target)] = capture;
            self.deferred_str_capture_count += 1;
        }
        return true;
    }

    fn deferredStrCaptureLen(self: *MonoLlvmCodeGen, capture: DeferredStrCapture) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        return wip.bin(.sub, try self.loadUsize(capture.end_ptr), try self.loadUsize(capture.start_ptr), "") catch return error.OutOfMemory;
    }

    fn noteDeferredStrCaptureIncref(self: *MonoLlvmCodeGen, local: LocalId, count: u16, atomicity: RcAtomicity) Error!void {
        var capture = self.deferredStrCapture(local) orelse return error.CompilationFailed;
        if (count == 0) return;
        if (capture.pending_rc_count != 0 and capture.pending_rc_atomicity != atomicity) return error.CompilationFailed;
        const total: u32 = @as(u32, capture.pending_rc_count) + count;
        if (total > std.math.maxInt(u16)) return error.CompilationFailed;
        capture.pending_rc_count = @intCast(total);
        capture.pending_rc_atomicity = atomicity;
        self.deferred_str_captures[@intFromEnum(local)] = capture;
    }

    fn offsetPtrValue(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, offset: LlvmBuilder.Value) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        return wip.gep(.inbounds, .i8, ptr, &.{offset}, "") catch return error.OutOfMemory;
    }

    /// Registers the join point, queues the remainder subtree, and lets
    /// `.join_after_remainder`/`.join_after_body` emit the branch-back glue and
    /// the join body. Heap `JoinState` is freed by the final continuation.
    fn emitJoin(self: *MonoLlvmCodeGen, join_stmt: anytype, wa: Allocator, work: *std.ArrayList(StmtWork)) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const key = @intFromEnum(join_stmt.id);
        const join_block = wip.block(0, "join_body") catch return error.OutOfMemory;
        const after_block = wip.block(0, "join_after") catch return error.OutOfMemory;
        try self.join_points.put(key, .{ .block = join_block, .params = join_stmt.params, .body = join_stmt.body });

        const state = try self.allocator.create(JoinState);
        state.* = .{
            .key = key,
            .join_block = join_block,
            .after_block = after_block,
            .body = join_stmt.body,
        };
        try work.append(wa, .{ .join_after_remainder = state });
        try work.append(wa, .{ .node = join_stmt.remainder });
    }

    fn emitJump(self: *MonoLlvmCodeGen, jump_stmt: anytype) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const info = self.join_points.get(@intFromEnum(jump_stmt.target)) orelse return error.CompilationFailed;
        try self.materializeDeferredStrCaptureParams(info.params);
        _ = wip.br(info.block) catch return error.OutOfMemory;
    }

    fn emitLoopContinue(self: *MonoLlvmCodeGen) Error!void {
        try self.materializeAllDeferredStrCaptures();
        const wip = self.wip orelse return error.CompilationFailed;
        const dest = self.loop_continue_blocks.items[self.loop_continue_blocks.items.len - 1];
        _ = wip.br(dest) catch return error.OutOfMemory;
    }

    fn emitLoopBreak(self: *MonoLlvmCodeGen) Error!void {
        try self.materializeAllDeferredStrCaptures();
        const wip = self.wip orelse return error.CompilationFailed;
        const dest = self.loop_break_blocks.items[self.loop_break_blocks.items.len - 1];
        _ = wip.br(dest) catch return error.OutOfMemory;
    }

    fn emitReturn(self: *MonoLlvmCodeGen, value: LocalId) Error!void {
        try self.materializeLocalIfDeferred(value);
        const ret_ptr = self.ret_ptr_arg orelse return error.CompilationFailed;
        const size = self.layoutByteSize(self.current_ret_layout);
        if (size > 0) {
            try self.copyBytes(ret_ptr, self.slot(value).ptr, size, self.alignmentForLayout(self.current_ret_layout));
        }
        if (self.ret_desc_ptr_arg) |out_desc| {
            const runtime_desc: ?lir.LIR.BoxyDescRef = if (self.current_runtime_ret_desc) |desc_local|
                .{ .local = desc_local }
            else
                self.store.getLocal(value).boxy_desc;
            if (runtime_desc) |desc_ref| {
                try self.storePointer(out_desc, try self.resolveBoxyDesc(desc_ref));
            }
        }
        const wip = self.wip orelse return error.CompilationFailed;
        _ = wip.retVoid() catch return error.OutOfMemory;
    }

    fn emitExpect(self: *MonoLlvmCodeGen, condition: LocalId) Error!void {
        try self.materializeLocalIfDeferred(condition);
        const wip = self.wip orelse return error.CompilationFailed;
        const ok_block = wip.block(0, "expect_ok") catch return error.OutOfMemory;
        const fail_block = wip.block(0, "expect_fail") catch return error.OutOfMemory;
        const cond = try self.loadBool(self.slot(condition).ptr);
        _ = wip.brCond(cond, ok_block, fail_block, .then_likely) catch return error.OutOfMemory;
        wip.cursor = .{ .block = fail_block };
        try self.emitStaticRocOpsMessageCall(.expect_failed, "expect failed");
        _ = wip.br(ok_block) catch return error.OutOfMemory;
        wip.cursor = .{ .block = ok_block };
    }

    fn emitCrashIf(self: *MonoLlvmCodeGen, condition: LlvmBuilder.Value, msg: []const u8) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const ok_block = wip.block(0, "checked_ok") catch return error.OutOfMemory;
        const crash_block = wip.block(0, "checked_crash") catch return error.OutOfMemory;
        _ = wip.brCond(condition, crash_block, ok_block, .else_likely) catch return error.OutOfMemory;
        wip.cursor = .{ .block = crash_block };
        try self.emitCrashBytes(msg);
        wip.cursor = .{ .block = ok_block };
    }

    fn emitCrashBytes(self: *MonoLlvmCodeGen, msg: []const u8) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        if (!try self.emitDefaultPlatformCrashWithFrames(
            try self.staticBytes(msg),
            builder.intValue(self.ptrSizedIntType(), msg.len) catch return error.OutOfMemory,
        )) {
            try self.emitStaticRocOpsMessageCall(.crashed, msg);
        }
        // Linux AArch64 eval tests handle crashes by returning to the Zig host.
        // Longjmping through LLVM-generated frames is not reliable on that target.
        if (self.target.cpu.arch == .aarch64 and self.target.os.tag == .linux) {
            _ = wip.retVoid() catch return error.OutOfMemory;
        } else {
            _ = wip.@"unreachable"() catch return error.OutOfMemory;
        }
    }

    fn emitCrashLocal(self: *MonoLlvmCodeGen, message: LocalId) Error!void {
        try self.materializeLocalIfDeferred(message);
        const msg = try self.emitStrMatchSourceShape(self.slot(message).ptr);
        if (!try self.emitDefaultPlatformCrashWithFrames(msg.bytes, msg.len)) {
            try self.callBuiltinVoid(
                builtinSymbol(.crash_str),
                &.{ try self.ptrType(), try self.ptrType() },
                &.{ self.slot(message).ptr, self.rocOps() },
            );
        }
        const wip = self.wip orelse return error.CompilationFailed;
        if (self.target.cpu.arch == .aarch64 and self.target.os.tag == .linux) {
            _ = wip.retVoid() catch return error.OutOfMemory;
        } else {
            _ = wip.@"unreachable"() catch return error.OutOfMemory;
        }
    }

    /// Call the synthetic default platform's diagnostic-only crash entrypoint
    /// with the exact virtual source-frame chain attached to the current LIR
    /// statement. This is a lossless backend encoding of LIR inline scopes, not
    /// a reconstruction from machine procedures or symbol names.
    fn emitDefaultPlatformCrashWithFrames(
        self: *MonoLlvmCodeGen,
        msg_bytes: LlvmBuilder.Value,
        msg_len: LlvmBuilder.Value,
    ) Error!bool {
        if (!self.enable_default_platform_diagnostics) return false;
        const stmt_id = self.current_source_stmt orelse return false;
        var scope_id = self.store.stmtInlineScope(stmt_id);
        if (scope_id == lir.LIR.InlineScopeId.none) return false;

        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const ptr_ty = try self.ptrType();
        const usize_ty = self.ptrSizedIntType();
        const frame_ty = builder.structType(.normal, &.{ ptr_ty, usize_ty, ptr_ty, usize_ty, .i32, .i32 }) catch return error.OutOfMemory;

        var frames = std.ArrayList(LlvmBuilder.Constant).empty;
        defer frames.deinit(self.allocator);
        var frame_loc = self.store.stmtLoc(stmt_id);
        while (scope_id != lir.LIR.InlineScopeId.none) {
            const scope = self.store.inlineScope(scope_id);
            try frames.append(self.allocator, try self.defaultPlatformSourceFrameConst(frame_ty, scope, frame_loc));
            frame_loc = scope.call_site;
            scope_id = scope.parent;
        }

        const frames_ty = builder.arrayType(frames.items.len, frame_ty) catch return error.OutOfMemory;
        const frames_name = builder.strtabStringFmt(".roc.crash_frames.{d}", .{self.string_counter}) catch return error.OutOfMemory;
        self.string_counter += 1;
        const frames_var = builder.addVariable(frames_name, frames_ty, .default) catch return error.OutOfMemory;
        frames_var.ptrConst(builder).global.setLinkage(.internal, builder);
        frames_var.setMutability(.constant, builder);
        frames_var.setInitializer(builder.arrayConst(frames_ty, frames.items) catch return error.OutOfMemory, builder) catch return error.OutOfMemory;

        const fn_ty = builder.fnType(.void, &.{ ptr_ty, usize_ty, ptr_ty, usize_ty }, .normal) catch return error.OutOfMemory;
        const callback = try self.declareExternSymbol(shim_symbols.roc_default_crashed_with_frames, fn_ty);
        _ = wip.call(.normal, .ccc, .none, fn_ty, callback.toValue(builder), &.{
            msg_bytes,
            msg_len,
            frames_var.toValue(builder),
            builder.intValue(usize_ty, frames.items.len) catch return error.OutOfMemory,
        }, "") catch return error.OutOfMemory;
        return true;
    }

    fn defaultPlatformSourceFrameConst(
        self: *MonoLlvmCodeGen,
        frame_ty: LlvmBuilder.Type,
        scope: lir.LIR.InlineScope,
        loc: SourceLoc,
    ) Error!LlvmBuilder.Constant {
        const builder = self.builder orelse return error.CompilationFailed;
        var allocated_name: ?[]u8 = null;
        defer if (allocated_name) |name| self.allocator.free(name);
        const name = if (scope.source_name.isNone()) blk: {
            const generated = try std.fmt.allocPrint(self.allocator, "roc__proc_{x}", .{scope.source_symbol.raw()});
            allocated_name = generated;
            break :blk generated;
        } else self.store.getString(scope.source_name);
        const file = if (loc.file == SourceLoc.no_file or loc.file >= self.store.sourceFileCount())
            ""
        else
            self.store.sourceFileName(loc.file);

        return builder.structConst(frame_ty, &.{
            (try self.staticBytes(name)).toConst().?,
            builder.intConst(self.ptrSizedIntType(), name.len) catch return error.OutOfMemory,
            (try self.staticBytes(file)).toConst().?,
            builder.intConst(self.ptrSizedIntType(), file.len) catch return error.OutOfMemory,
            builder.intConst(.i32, if (loc.hasLocation()) loc.line else 0) catch return error.OutOfMemory,
            builder.intConst(.i32, if (loc.hasLocation()) loc.column else 0) catch return error.OutOfMemory,
        }) catch return error.OutOfMemory;
    }

    fn emitRuntimeError(self: *MonoLlvmCodeGen) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const func = self.runtime_error_func orelse return error.CompilationFailed;
        _ = wip.call(.normal, .ccc, .none, func.typeOf(builder), func.toValue(builder), &.{self.rocOps()}, "") catch return error.OutOfMemory;
        // Keep the terminal behavior identical to the previous inline
        // `emitCrashBytes("hit a runtime error")` lowering at the call site.
        if (self.target.cpu.arch == .aarch64 and self.target.os.tag == .linux) {
            _ = wip.retVoid() catch return error.OutOfMemory;
        } else {
            _ = wip.@"unreachable"() catch return error.OutOfMemory;
        }
    }

    fn emitStaticRocOpsMessageCall(self: *MonoLlvmCodeGen, callback: RocOpsCallback, msg: []const u8) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const ptr_ty = try self.ptrType();

        if (callback == .expect_failed or callback == .crashed) {
            const wrapper_name = if (callback == .crashed)
                builtinSymbol(.roc_crashed)
            else
                builtinSymbol(.roc_expect_failed);

            try self.callBuiltinVoid(
                wrapper_name,
                &.{ ptr_ty, self.ptrSizedIntType(), ptr_ty },
                &.{
                    try self.staticBytes(msg),
                    builder.intValue(self.ptrSizedIntType(), msg.len) catch return error.OutOfMemory,
                    self.rocOps(),
                },
            );
            return;
        }

        const wip = self.wip orelse return error.CompilationFailed;
        if (self.host_call_mode == .extern_symbols) {
            // Symbol ABI: call the host's runtime symbol directly:
            // roc_dbg(bytes: [*]const u8, len: usize).
            const fn_ty = builder.fnType(.void, &.{ ptr_ty, self.ptrSizedIntType() }, .normal) catch return error.OutOfMemory;
            const func = try self.declareExternSymbol("roc_dbg", fn_ty);
            _ = wip.call(.normal, .ccc, .none, fn_ty, func.toValue(builder), &.{
                try self.staticBytes(msg),
                builder.intValue(self.ptrSizedIntType(), msg.len) catch return error.OutOfMemory,
            }, "") catch return error.OutOfMemory;
            return;
        }

        // RocOps callback ABI: roc_dbg(ops: *RocOps, bytes: [*]const u8, len: usize).
        const callback_ptr_ptr = try self.offsetPtr(self.rocOps(), self.rocOpsCallbackOffset(callback));
        const callback_ptr = try self.loadPointer(callback_ptr_ptr);
        const fn_ty = builder.fnType(.void, &.{ ptr_ty, ptr_ty, self.ptrSizedIntType() }, .normal) catch return error.OutOfMemory;
        _ = wip.call(.normal, .ccc, .none, fn_ty, callback_ptr, &.{
            self.rocOps(),
            try self.staticBytes(msg),
            builder.intValue(self.ptrSizedIntType(), msg.len) catch return error.OutOfMemory,
        }, "") catch return error.OutOfMemory;
    }

    fn copyLocal(self: *MonoLlvmCodeGen, target: LocalId, source: LocalId) Error!void {
        const target_slot = self.slot(target);
        if (target_slot.size == 0) return;
        if (try self.propagateDeferredStrCapture(target, source)) return;
        try self.materializeLocalIfDeferred(source);
        try self.prepareLocalWrite(target);
        try self.copyBytes(target_slot.ptr, self.slot(source).ptr, target_slot.size, target_slot.alignment);
    }

    fn emitStrLiteral(self: *MonoLlvmCodeGen, out: LlvmBuilder.Value, literal: StrLiteral) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const bytes = self.store.getStringLiteral(literal);
        try self.callBuiltinVoid(
            builtinSymbol(.str_from_literal),
            &.{ try self.ptrType(), try self.ptrType(), self.ptrSizedIntType(), try self.ptrType() },
            &.{
                out,
                try self.staticBytes(bytes),
                builder.intValue(self.ptrSizedIntType(), bytes.len) catch return error.OutOfMemory,
                self.rocOps(),
            },
        );
    }

    fn emitStaticDataLiteral(self: *MonoLlvmCodeGen, out: LocalSlot, id: lir.LIR.StaticDataId) Error!void {
        if (out.size == 0) return;
        try self.copyBytes(out.ptr, try self.staticDataGlobal(id, out.size), out.size, out.alignment);
    }

    fn staticDataGlobal(self: *MonoLlvmCodeGen, id: lir.LIR.StaticDataId, size: u32) Error!LlvmBuilder.Value {
        const raw_id: u32 = @intFromEnum(id);
        if (self.static_data_globals.get(raw_id)) |value| return value;

        const builder = self.builder orelse return error.CompilationFailed;
        const symbol_name = try lir.Program.staticDataSymbolName(self.allocator, id);
        defer self.allocator.free(symbol_name);

        const arr_ty = builder.arrayType(@max(size, 1), .i8) catch return error.OutOfMemory;
        const variable = builder.addVariable(builder.strtabString(symbol_name) catch return error.OutOfMemory, arr_ty, .default) catch return error.OutOfMemory;
        variable.ptrConst(builder).global.setLinkage(.external, builder);

        const value = variable.toValue(builder);
        try self.static_data_globals.put(raw_id, value);
        return value;
    }

    fn emitBytesLiteral(self: *MonoLlvmCodeGen, out: LlvmBuilder.Value, literal: lir.LIR.ListLiteral) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const bytes = self.store.getStringLiteral(literal.bytes);
        if (bytes.len == 0) {
            try self.storeListFields(out, builder.nullValue(try self.ptrType()) catch return error.OutOfMemory, 0, 0);
            return;
        }

        const backing_bytes = self.store.getStringLiteralBacking(literal.bytes);
        const whole_backing = literal.bytes.offset == 0 and @as(usize, literal.bytes.len) == backing_bytes.len;
        const backing_ptr = try self.staticRefcountedBytes(literal.bytes.backing);
        const bytes_ptr = try self.offsetPtrValue(
            backing_ptr,
            builder.intValue(self.ptrSizedIntType(), literal.bytes.offset) catch return error.OutOfMemory,
        );

        try self.storePointer(out, bytes_ptr);
        try self.storeListLen(out, builder.intValue(self.ptrSizedIntType(), literal.len) catch return error.OutOfMemory);
        if (whole_backing) {
            try self.storeListCapacity(out, builder.intValue(self.ptrSizedIntType(), @as(u64, literal.len) << 1) catch return error.OutOfMemory);
        } else {
            const backing_int = wip.cast(.ptrtoint, backing_ptr, self.ptrSizedIntType(), "") catch return error.OutOfMemory;
            const alloc_ptr = wip.bin(.add, backing_int, builder.intValue(self.ptrSizedIntType(), 1) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
            try self.storeListCapacity(out, alloc_ptr);
        }
    }

    fn staticBytes(self: *MonoLlvmCodeGen, bytes: []const u8) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const actual = if (bytes.len == 0) "\x00" else bytes;
        if (self.static_bytes.get(actual)) |existing| return existing;

        const key = self.allocator.dupe(u8, actual) catch return error.OutOfMemory;
        errdefer self.allocator.free(key);

        const arr_ty = builder.arrayType(actual.len, .i8) catch return error.OutOfMemory;
        const name = builder.strtabStringFmt(".roc.bytes.{d}", .{self.string_counter}) catch return error.OutOfMemory;
        self.string_counter += 1;
        const variable = builder.addVariable(name, arr_ty, .default) catch return error.OutOfMemory;
        variable.ptrConst(builder).global.setLinkage(.internal, builder);
        variable.setMutability(.constant, builder);
        variable.setInitializer(builder.stringConst(builder.string(actual) catch return error.OutOfMemory) catch return error.OutOfMemory, builder) catch return error.OutOfMemory;
        const value = variable.toValue(builder);
        try self.static_bytes.put(key, value);
        return value;
    }

    fn staticRefcountedBytes(self: *MonoLlvmCodeGen, backing: Base.StringLiteral.Idx) Error!LlvmBuilder.Value {
        const key: u32 = @intFromEnum(backing);
        const builder = self.builder orelse return error.CompilationFailed;
        const word_size: usize = self.targetWordSize();
        const backing_alignment = @max(word_size, @as(usize, self.store.strings.alignment(backing)));
        const data_offset = std.mem.alignForward(usize, word_size, backing_alignment);
        // Only the backing global may be cached across proc bodies: the GEP to
        // its data offset is an instruction in whichever WipFunction is being
        // compiled, so it must be emitted fresh per function.
        if (self.static_refcounted_backings.get(key)) |existing| {
            return try self.offsetPtrValue(existing, builder.intValue(self.ptrSizedIntType(), data_offset) catch return error.OutOfMemory);
        }

        const bytes = self.store.getString(backing);
        const storage = self.allocator.alloc(u8, data_offset + bytes.len) catch return error.OutOfMemory;
        defer self.allocator.free(storage);

        @memset(storage[0..data_offset], 0);
        @memcpy(storage[data_offset..][0..bytes.len], bytes);

        const arr_ty = builder.arrayType(storage.len, .i8) catch return error.OutOfMemory;
        const name = builder.strtabStringFmt(".roc.refcounted_bytes.{d}", .{self.string_counter}) catch return error.OutOfMemory;
        self.string_counter += 1;
        const variable = builder.addVariable(name, arr_ty, .default) catch return error.OutOfMemory;
        variable.ptrConst(builder).global.setLinkage(.internal, builder);
        variable.setMutability(.constant, builder);
        variable.setAlignment(LlvmBuilder.Alignment.fromByteUnits(backing_alignment), builder);
        variable.setInitializer(builder.stringConst(builder.string(storage) catch return error.OutOfMemory) catch return error.OutOfMemory, builder) catch return error.OutOfMemory;

        const base = variable.toValue(builder);
        try self.static_refcounted_backings.put(key, base);
        return try self.offsetPtrValue(base, builder.intValue(self.ptrSizedIntType(), data_offset) catch return error.OutOfMemory);
    }

    fn emitStrByteSliceForLocal(self: *MonoLlvmCodeGen, local: LocalId) Error!StrByteSlice {
        if (self.deferredStrCapture(local)) |capture| {
            const start = try self.loadUsize(capture.start_ptr);
            const bytes = try self.offsetPtrValue(capture.source.bytes, start);
            return .{
                .bytes = bytes,
                .len = try self.deferredStrCaptureLen(capture),
            };
        }
        const source = try self.emitStrMatchSourceShape(self.slot(local).ptr);
        return .{ .bytes = source.bytes, .len = source.len };
    }

    fn emitStrViewShapeForLocal(self: *MonoLlvmCodeGen, local: LocalId) Error!StrMatchSource {
        if (self.deferredStrCapture(local)) |capture| {
            const start = try self.loadUsize(capture.start_ptr);
            return .{
                .bytes = try self.offsetPtrValue(capture.source.bytes, start),
                .len = try self.deferredStrCaptureLen(capture),
                .is_small = capture.source.is_small,
                .alloc = capture.source.alloc,
            };
        }
        return self.emitStrMatchSourceShape(self.slot(local).ptr);
    }

    fn emitStrBoolJoin(
        self: *MonoLlvmCodeGen,
        target: LocalId,
        true_block: LlvmBuilder.Function.Block.Index,
        false_block: LlvmBuilder.Function.Block.Index,
        after_block: LlvmBuilder.Function.Block.Index,
    ) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;

        wip.cursor = .{ .block = true_block };
        try self.storeBool(self.slot(target).ptr, (self.builder orelse return error.CompilationFailed).intValue(.i1, 1) catch return error.OutOfMemory);
        _ = wip.br(after_block) catch return error.OutOfMemory;

        wip.cursor = .{ .block = false_block };
        try self.storeBool(self.slot(target).ptr, (self.builder orelse return error.CompilationFailed).intValue(.i1, 0) catch return error.OutOfMemory);
        _ = wip.br(after_block) catch return error.OutOfMemory;

        wip.cursor = .{ .block = after_block };
    }

    fn emitByteSlicesEqualBranch(
        self: *MonoLlvmCodeGen,
        lhs_bytes: LlvmBuilder.Value,
        rhs_bytes: LlvmBuilder.Value,
        len: LlvmBuilder.Value,
        equal_block: LlvmBuilder.Function.Block.Index,
        not_equal_block: LlvmBuilder.Function.Block.Index,
    ) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const usize_ty = self.ptrSizedIntType();
        const width = self.strMatchScanWidth();
        const zero = builder.intValue(usize_ty, 0) catch return error.OutOfMemory;

        const decide_block = wip.block(0, "str_eq_decide") catch return error.OutOfMemory;
        const nonempty_block = wip.block(0, "str_eq_nonempty") catch return error.OutOfMemory;
        const word_loop_block = wip.block(2, "str_eq_word") catch return error.OutOfMemory;
        const word_after_block = wip.block(0, "str_eq_word_after") catch return error.OutOfMemory;
        const word_maybe_tail_block = wip.block(0, "str_eq_word_maybe_tail") catch return error.OutOfMemory;
        const tail_loop_block = wip.block(3, "str_eq_tail") catch return error.OutOfMemory;
        const tail_after_block = wip.block(0, "str_eq_tail_after") catch return error.OutOfMemory;

        _ = wip.br(decide_block) catch return error.OutOfMemory;

        wip.cursor = .{ .block = decide_block };
        const len_is_zero = wip.icmp(.eq, len, zero, "") catch return error.OutOfMemory;
        _ = wip.brCond(len_is_zero, equal_block, nonempty_block, .none) catch return error.OutOfMemory;

        wip.cursor = .{ .block = nonempty_block };
        const has_word = wip.icmp(.uge, len, builder.intValue(usize_ty, width) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        _ = wip.brCond(has_word, word_loop_block, tail_loop_block, .then_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = word_loop_block };
        const word_cursor_phi = wip.phi(usize_ty, "str_eq_word_cursor") catch return error.OutOfMemory;
        const word_cursor = word_cursor_phi.toValue();
        const lhs_word_ptr = try self.offsetPtrValue(lhs_bytes, word_cursor);
        const rhs_word_ptr = try self.offsetPtrValue(rhs_bytes, word_cursor);
        const word_ty = intTypeForBytes(width);
        const lhs_word = wip.load(.normal, word_ty, lhs_word_ptr, LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
        const rhs_word = wip.load(.normal, word_ty, rhs_word_ptr, LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
        const words_equal = wip.icmp(.eq, lhs_word, rhs_word, "") catch return error.OutOfMemory;
        _ = wip.brCond(words_equal, word_after_block, not_equal_block, .then_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = word_after_block };
        const next_word_cursor = wip.bin(.add, word_cursor, builder.intValue(usize_ty, width) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const word_remaining = wip.bin(.sub, len, next_word_cursor, "") catch return error.OutOfMemory;
        const has_next_word = wip.icmp(.uge, word_remaining, builder.intValue(usize_ty, width) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        _ = wip.brCond(has_next_word, word_loop_block, word_maybe_tail_block, .then_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = word_maybe_tail_block };
        const has_word_tail = wip.icmp(.ne, word_remaining, zero, "") catch return error.OutOfMemory;
        _ = wip.brCond(has_word_tail, tail_loop_block, equal_block, .none) catch return error.OutOfMemory;

        word_cursor_phi.finish(&.{ zero, next_word_cursor }, &.{ nonempty_block, word_after_block }, wip);

        wip.cursor = .{ .block = tail_loop_block };
        const tail_cursor_phi = wip.phi(usize_ty, "str_eq_tail_cursor") catch return error.OutOfMemory;
        const tail_cursor = tail_cursor_phi.toValue();
        const lhs_byte = wip.load(.normal, .i8, try self.offsetPtrValue(lhs_bytes, tail_cursor), LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
        const rhs_byte = wip.load(.normal, .i8, try self.offsetPtrValue(rhs_bytes, tail_cursor), LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
        const bytes_equal = wip.icmp(.eq, lhs_byte, rhs_byte, "") catch return error.OutOfMemory;
        _ = wip.brCond(bytes_equal, tail_after_block, not_equal_block, .then_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = tail_after_block };
        const next_tail_cursor = wip.bin(.add, tail_cursor, builder.intValue(usize_ty, 1) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const tail_done = wip.icmp(.eq, next_tail_cursor, len, "") catch return error.OutOfMemory;
        _ = wip.brCond(tail_done, equal_block, tail_loop_block, .none) catch return error.OutOfMemory;

        tail_cursor_phi.finish(&.{ zero, next_word_cursor, next_tail_cursor }, &.{ nonempty_block, word_maybe_tail_block, tail_after_block }, wip);
    }

    fn emitStrIsEq(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const lhs = try self.emitStrByteSliceForLocal(GuardedList.at(args, 0));
        const rhs = try self.emitStrByteSliceForLocal(GuardedList.at(args, 1));
        const true_block = wip.block(0, "str_eq_true") catch return error.OutOfMemory;
        const false_block = wip.block(0, "str_eq_false") catch return error.OutOfMemory;
        const same_len_block = wip.block(0, "str_eq_same_len") catch return error.OutOfMemory;
        const compare_block = wip.block(0, "str_eq_compare") catch return error.OutOfMemory;
        const after_block = wip.block(0, "str_eq_after") catch return error.OutOfMemory;
        const same_len = wip.icmp(.eq, lhs.len, rhs.len, "") catch return error.OutOfMemory;
        _ = wip.brCond(same_len, same_len_block, false_block, .then_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = same_len_block };
        // `bytes` is the actual content address for every string representation.
        const same_bytes = wip.icmp(.eq, lhs.bytes, rhs.bytes, "") catch return error.OutOfMemory;
        _ = wip.brCond(same_bytes, true_block, compare_block, .none) catch return error.OutOfMemory;

        wip.cursor = .{ .block = compare_block };
        try self.emitByteSlicesEqualBranch(lhs.bytes, rhs.bytes, lhs.len, true_block, false_block);
        try self.emitStrBoolJoin(target, true_block, false_block, after_block);
    }

    fn emitStrContains(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const usize_ty = self.ptrSizedIntType();
        const haystack = try self.emitStrByteSliceForLocal(GuardedList.at(args, 0));
        const needle = try self.emitStrByteSliceForLocal(GuardedList.at(args, 1));

        const true_block = wip.block(0, "str_contains_true") catch return error.OutOfMemory;
        const false_block = wip.block(0, "str_contains_false") catch return error.OutOfMemory;
        const nonempty_needle_block = wip.block(0, "str_contains_nonempty_needle") catch return error.OutOfMemory;
        const search_init_block = wip.block(0, "str_contains_search_init") catch return error.OutOfMemory;
        const scan_block = wip.block(0, "str_contains_scan") catch return error.OutOfMemory;
        const found_block = wip.block(0, "str_contains_candidate") catch return error.OutOfMemory;
        const candidate_miss_block = wip.block(0, "str_contains_candidate_miss") catch return error.OutOfMemory;
        const after_block = wip.block(0, "str_contains_after") catch return error.OutOfMemory;

        const zero = builder.intValue(usize_ty, 0) catch return error.OutOfMemory;
        const needle_is_empty = wip.icmp(.eq, needle.len, zero, "") catch return error.OutOfMemory;
        _ = wip.brCond(needle_is_empty, true_block, nonempty_needle_block, .none) catch return error.OutOfMemory;

        wip.cursor = .{ .block = nonempty_needle_block };
        const len_ok = wip.icmp(.uge, haystack.len, needle.len, "") catch return error.OutOfMemory;
        _ = wip.brCond(len_ok, search_init_block, false_block, .then_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = search_init_block };
        const cursor_ptr = try self.allocEntryBlockSlot(usize_ty, 1, self.targetPointerAlignment(), "str_contains_cursor");
        try self.storeUsize(cursor_ptr, zero);
        const limit = wip.bin(.sub, haystack.len, needle.len, "") catch return error.OutOfMemory;
        const first_byte = wip.load(.normal, .i8, needle.bytes, LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
        _ = wip.br(scan_block) catch return error.OutOfMemory;

        wip.cursor = .{ .block = scan_block };
        try self.emitFindFirstByte(haystack.bytes, cursor_ptr, limit, first_byte, found_block, false_block);

        wip.cursor = .{ .block = found_block };
        const found_cursor = try self.loadUsize(cursor_ptr);
        const candidate = try self.offsetPtrValue(haystack.bytes, found_cursor);
        try self.emitByteSlicesEqualBranch(candidate, needle.bytes, needle.len, true_block, candidate_miss_block);

        wip.cursor = .{ .block = candidate_miss_block };
        const next_cursor = wip.bin(.add, try self.loadUsize(cursor_ptr), builder.intValue(usize_ty, 1) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        try self.storeUsize(cursor_ptr, next_cursor);
        const has_next_candidate = wip.icmp(.ule, next_cursor, limit, "") catch return error.OutOfMemory;
        _ = wip.brCond(has_next_candidate, scan_block, false_block, .then_likely) catch return error.OutOfMemory;

        try self.emitStrBoolJoin(target, true_block, false_block, after_block);
    }

    fn emitStrCaselessAsciiEquals(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const usize_ty = self.ptrSizedIntType();
        const lhs = try self.emitStrByteSliceForLocal(GuardedList.at(args, 0));
        const rhs = try self.emitStrByteSliceForLocal(GuardedList.at(args, 1));

        const true_block = wip.block(0, "str_caseless_true") catch return error.OutOfMemory;
        const false_block = wip.block(0, "str_caseless_false") catch return error.OutOfMemory;
        const same_len_block = wip.block(0, "str_caseless_same_len") catch return error.OutOfMemory;
        const loop_block = wip.block(2, "str_caseless_loop") catch return error.OutOfMemory;
        const maybe_case_block = wip.block(0, "str_caseless_maybe_case") catch return error.OutOfMemory;
        const after_byte_block = wip.block(0, "str_caseless_after_byte") catch return error.OutOfMemory;
        const after_block = wip.block(0, "str_caseless_after") catch return error.OutOfMemory;

        const same_len = wip.icmp(.eq, lhs.len, rhs.len, "") catch return error.OutOfMemory;
        _ = wip.brCond(same_len, same_len_block, false_block, .then_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = same_len_block };
        const zero = builder.intValue(usize_ty, 0) catch return error.OutOfMemory;
        const len_is_zero = wip.icmp(.eq, lhs.len, zero, "") catch return error.OutOfMemory;
        _ = wip.brCond(len_is_zero, true_block, loop_block, .none) catch return error.OutOfMemory;

        wip.cursor = .{ .block = loop_block };
        const cursor_phi = wip.phi(usize_ty, "str_caseless_cursor") catch return error.OutOfMemory;
        const cursor = cursor_phi.toValue();
        const lhs_byte = wip.load(.normal, .i8, try self.offsetPtrValue(lhs.bytes, cursor), LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
        const rhs_byte = wip.load(.normal, .i8, try self.offsetPtrValue(rhs.bytes, cursor), LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
        const bytes_equal = wip.icmp(.eq, lhs_byte, rhs_byte, "") catch return error.OutOfMemory;
        _ = wip.brCond(bytes_equal, after_byte_block, maybe_case_block, .then_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = maybe_case_block };
        const ascii_case_bit = builder.intValue(.i8, 0x20) catch return error.OutOfMemory;
        const lhs_folded = wip.bin(.@"or", lhs_byte, ascii_case_bit, "") catch return error.OutOfMemory;
        const rhs_folded = wip.bin(.@"or", rhs_byte, ascii_case_bit, "") catch return error.OutOfMemory;
        const folded_equal = wip.icmp(.eq, lhs_folded, rhs_folded, "") catch return error.OutOfMemory;
        const ge_a = wip.icmp(.uge, lhs_folded, builder.intValue(.i8, 'a') catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const le_z = wip.icmp(.ule, lhs_folded, builder.intValue(.i8, 'z') catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const is_ascii_letter = wip.bin(.@"and", ge_a, le_z, "") catch return error.OutOfMemory;
        const caseless_match = wip.bin(.@"and", folded_equal, is_ascii_letter, "") catch return error.OutOfMemory;
        _ = wip.brCond(caseless_match, after_byte_block, false_block, .then_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = after_byte_block };
        const next_cursor = wip.bin(.add, cursor, builder.intValue(usize_ty, 1) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const done = wip.icmp(.eq, next_cursor, lhs.len, "") catch return error.OutOfMemory;
        _ = wip.brCond(done, true_block, loop_block, .none) catch return error.OutOfMemory;

        cursor_phi.finish(&.{ zero, next_cursor }, &.{ same_len_block, after_byte_block }, wip);

        try self.emitStrBoolJoin(target, true_block, false_block, after_block);
    }

    fn emitStrDropEdge(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, edge: StrDropEdge) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const usize_ty = self.ptrSizedIntType();
        const source = try self.emitStrViewShapeForLocal(GuardedList.at(args, 0));
        const affix = try self.emitStrByteSliceForLocal(GuardedList.at(args, 1));
        const target_slot = self.slot(target);
        const result_ptr = if (target == GuardedList.at(args, 0))
            try self.allocEntryBlockSlot(.i8, @max(target_slot.size, 1), target_slot.alignment, "str_drop_result")
        else
            target_slot.ptr;

        const no_match_block = wip.block(0, "str_drop_no_match") catch return error.OutOfMemory;
        const match_block = wip.block(0, "str_drop_match") catch return error.OutOfMemory;
        const compare_block = wip.block(0, "str_drop_compare") catch return error.OutOfMemory;
        const done_block = wip.block(0, "str_drop_done") catch return error.OutOfMemory;

        const len_ok = wip.icmp(.uge, source.len, affix.len, "") catch return error.OutOfMemory;
        _ = wip.brCond(len_ok, compare_block, no_match_block, .then_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = compare_block };
        const compare_bytes = switch (edge) {
            .prefix => source.bytes,
            .suffix => blk: {
                const suffix_start = wip.bin(.sub, source.len, affix.len, "") catch return error.OutOfMemory;
                break :blk try self.offsetPtrValue(source.bytes, suffix_start);
            },
        };
        try self.emitByteSlicesEqualBranch(compare_bytes, affix.bytes, affix.len, match_block, no_match_block);

        wip.cursor = .{ .block = no_match_block };
        const zero = builder.intValue(usize_ty, 0) catch return error.OutOfMemory;
        try self.emitStoreStrMatchCapture(result_ptr, source, zero, source.len);
        _ = wip.br(done_block) catch return error.OutOfMemory;

        wip.cursor = .{ .block = match_block };
        const result_start = switch (edge) {
            .prefix => affix.len,
            .suffix => zero,
        };
        const result_end = switch (edge) {
            .prefix => source.len,
            .suffix => wip.bin(.sub, source.len, affix.len, "") catch return error.OutOfMemory,
        };
        try self.emitStoreStrMatchCapture(result_ptr, source, result_start, result_end);
        _ = wip.br(done_block) catch return error.OutOfMemory;

        wip.cursor = .{ .block = done_block };
        if (target == GuardedList.at(args, 0)) {
            try self.copyBytes(target_slot.ptr, result_ptr, target_slot.size, target_slot.alignment);
        }
    }

    fn emitStrStartsWith(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const haystack = try self.emitStrByteSliceForLocal(GuardedList.at(args, 0));
        const needle = try self.emitStrByteSliceForLocal(GuardedList.at(args, 1));
        const true_block = wip.block(0, "str_starts_true") catch return error.OutOfMemory;
        const false_block = wip.block(0, "str_starts_false") catch return error.OutOfMemory;
        const len_ok_block = wip.block(0, "str_starts_len_ok") catch return error.OutOfMemory;
        const after_block = wip.block(0, "str_starts_after") catch return error.OutOfMemory;
        const len_ok = wip.icmp(.uge, haystack.len, needle.len, "") catch return error.OutOfMemory;
        _ = wip.brCond(len_ok, len_ok_block, false_block, .then_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = len_ok_block };
        try self.emitByteSlicesEqualBranch(haystack.bytes, needle.bytes, needle.len, true_block, false_block);
        try self.emitStrBoolJoin(target, true_block, false_block, after_block);
    }

    fn emitStrEndsWith(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const haystack = try self.emitStrByteSliceForLocal(GuardedList.at(args, 0));
        const needle = try self.emitStrByteSliceForLocal(GuardedList.at(args, 1));
        const true_block = wip.block(0, "str_ends_true") catch return error.OutOfMemory;
        const false_block = wip.block(0, "str_ends_false") catch return error.OutOfMemory;
        const len_ok_block = wip.block(0, "str_ends_len_ok") catch return error.OutOfMemory;
        const after_block = wip.block(0, "str_ends_after") catch return error.OutOfMemory;
        const len_ok = wip.icmp(.uge, haystack.len, needle.len, "") catch return error.OutOfMemory;
        _ = wip.brCond(len_ok, len_ok_block, false_block, .then_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = len_ok_block };
        const start = wip.bin(.sub, haystack.len, needle.len, "") catch return error.OutOfMemory;
        const suffix_bytes = try self.offsetPtrValue(haystack.bytes, start);
        try self.emitByteSlicesEqualBranch(suffix_bytes, needle.bytes, needle.len, true_block, false_block);
        try self.emitStrBoolJoin(target, true_block, false_block, after_block);
    }

    fn emitStrEqStaticSmall(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        if (args.len != 5) return error.CompilationFailed;

        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const usize_ty = self.ptrSizedIntType();
        const str_ptr = self.slot(GuardedList.at(args, 0)).ptr;
        const target_ptr = self.slot(target).ptr;

        const raw_len = try self.loadUsize(try self.offsetPtr(str_ptr, self.rocStrLenOffset()));
        const is_small = wip.icmp(.slt, raw_len, builder.intValue(usize_ty, 0) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const last_byte = wip.load(
            .normal,
            .i8,
            try self.offsetPtr(str_ptr, self.targetWordSize() * 3 - 1),
            LlvmBuilder.Alignment.fromByteUnits(1),
            "",
        ) catch return error.OutOfMemory;
        const small_len_byte = wip.bin(.@"and", last_byte, builder.intValue(.i8, 0x7f) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const small_len = try self.coerceScalar(small_len_byte, usize_ty, false);
        const runtime_len = wip.select(.normal, is_small, small_len, raw_len, "") catch return error.OutOfMemory;

        const heap_ptr = try self.loadPointer(str_ptr);
        const data_ptr = wip.select(.normal, is_small, str_ptr, heap_ptr, "") catch return error.OutOfMemory;

        const static_len = try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1))), usize_ty, false);
        const words = [3]LlvmBuilder.Value{
            try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 2)).ptr, self.localLayout(GuardedList.at(args, 2))), .i64, false),
            try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 3)).ptr, self.localLayout(GuardedList.at(args, 3))), .i64, false),
            try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 4)).ptr, self.localLayout(GuardedList.at(args, 4))), .i64, false),
        };

        try self.storeBool(target_ptr, builder.intValue(.i1, 0) catch return error.OutOfMemory);
        const len_matches = wip.icmp(.eq, runtime_len, static_len, "") catch return error.OutOfMemory;
        const len_in_range = wip.icmp(.ule, static_len, builder.intValue(usize_ty, 24) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const can_compare = wip.bin(.@"and", len_matches, len_in_range, "") catch return error.OutOfMemory;

        const compare_block = wip.block(0, "str_static_compare") catch return error.OutOfMemory;
        const fail_block = wip.block(0, "str_static_fail") catch return error.OutOfMemory;
        const done_block = wip.block(0, "str_static_done") catch return error.OutOfMemory;
        _ = wip.brCond(can_compare, compare_block, done_block, .then_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = compare_block };
        try self.storeBool(target_ptr, builder.intValue(.i1, 1) catch return error.OutOfMemory);

        inline for (0..24) |index| {
            const byte_needed = wip.icmp(.ugt, static_len, builder.intValue(usize_ty, @as(i64, @intCast(index))) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
            const byte_block = wip.block(0, "str_static_byte") catch return error.OutOfMemory;
            const next_block = wip.block(0, "str_static_next") catch return error.OutOfMemory;
            _ = wip.brCond(byte_needed, byte_block, done_block, .then_likely) catch return error.OutOfMemory;

            wip.cursor = .{ .block = byte_block };
            const runtime_byte = wip.load(
                .normal,
                .i8,
                try self.offsetPtr(data_ptr, @intCast(index)),
                LlvmBuilder.Alignment.fromByteUnits(1),
                "",
            ) catch return error.OutOfMemory;
            const word = words[index / @sizeOf(u64)];
            const shifted = if ((index % @sizeOf(u64)) == 0)
                word
            else
                wip.bin(.lshr, word, builder.intValue(.i64, @as(i64, @intCast((index % @sizeOf(u64)) * 8))) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
            const expected_byte = try self.coerceScalar(shifted, .i8, false);
            const byte_matches = wip.icmp(.eq, runtime_byte, expected_byte, "") catch return error.OutOfMemory;
            _ = wip.brCond(byte_matches, next_block, fail_block, .then_likely) catch return error.OutOfMemory;

            wip.cursor = .{ .block = next_block };
        }

        _ = wip.br(done_block) catch return error.OutOfMemory;

        wip.cursor = .{ .block = fail_block };
        try self.storeBool(target_ptr, builder.intValue(.i1, 0) catch return error.OutOfMemory);
        _ = wip.br(done_block) catch return error.OutOfMemory;

        wip.cursor = .{ .block = done_block };
    }

    const StaticSmallWordCompareMode = enum {
        exact,
        caseless,
    };

    fn emitStrStaticSmallWordEq(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        try self.emitStrStaticSmallWordCompare(target, args, .exact);
    }

    fn emitStrStaticSmallWordCaselessEq(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        try self.emitStrStaticSmallWordCompare(target, args, .caseless);
    }

    fn emitStrStaticSmallWordCompare(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, comptime mode: StaticSmallWordCompareMode) Error!void {
        if (args.len != 4) return error.CompilationFailed;

        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const usize_ty = self.ptrSizedIntType();
        const str_ptr = self.slot(GuardedList.at(args, 0)).ptr;
        const target_ptr = self.slot(target).ptr;

        const raw_len = try self.loadUsize(try self.offsetPtr(str_ptr, self.rocStrLenOffset()));
        const is_small = wip.icmp(.slt, raw_len, builder.intValue(usize_ty, 0) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const last_byte = wip.load(
            .normal,
            .i8,
            try self.offsetPtr(str_ptr, self.targetWordSize() * 3 - 1),
            LlvmBuilder.Alignment.fromByteUnits(1),
            "",
        ) catch return error.OutOfMemory;
        const small_len_byte = wip.bin(.@"and", last_byte, builder.intValue(.i8, 0x7f) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const small_len = try self.coerceScalar(small_len_byte, usize_ty, false);
        const runtime_len = wip.select(.normal, is_small, small_len, raw_len, "") catch return error.OutOfMemory;

        const heap_ptr = try self.loadPointer(str_ptr);
        const data_ptr = wip.select(.normal, is_small, str_ptr, heap_ptr, "") catch return error.OutOfMemory;

        const offset = try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1))), usize_ty, false);
        const active_len = try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 2)).ptr, self.localLayout(GuardedList.at(args, 2))), usize_ty, false);
        const static_word = try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 3)).ptr, self.localLayout(GuardedList.at(args, 3))), .i64, false);

        try self.storeBool(target_ptr, builder.intValue(.i1, 0) catch return error.OutOfMemory);

        const active_len_in_range = wip.icmp(.ule, active_len, builder.intValue(usize_ty, 8) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const offset_in_range = wip.icmp(.ule, offset, runtime_len, "") catch return error.OutOfMemory;
        const remaining_len = wip.bin(.sub, runtime_len, offset, "") catch return error.OutOfMemory;
        const active_in_remaining = wip.icmp(.ule, active_len, remaining_len, "") catch return error.OutOfMemory;
        const in_range = wip.bin(.@"and", active_len_in_range, wip.bin(.@"and", offset_in_range, active_in_remaining, "") catch return error.OutOfMemory, "") catch return error.OutOfMemory;

        const compare_block = wip.block(1, "str_static_lane_compare") catch return error.OutOfMemory;
        const full_load_block = wip.block(1, "str_static_lane_full_load") catch return error.OutOfMemory;
        const tail_block = wip.block(1, "str_static_lane_tail") catch return error.OutOfMemory;
        const mask_block = wip.block(2, "str_static_lane_mask") catch return error.OutOfMemory;
        const mask_zero_block = wip.block(1, "str_static_lane_mask_zero") catch return error.OutOfMemory;
        const mask_full_block = wip.block(1, "str_static_lane_mask_full") catch return error.OutOfMemory;
        const mask_eight_block = wip.block(1, "str_static_lane_mask_eight") catch return error.OutOfMemory;
        const mask_partial_block = wip.block(1, "str_static_lane_mask_partial") catch return error.OutOfMemory;
        const finish_block = wip.block(3, "str_static_lane_finish") catch return error.OutOfMemory;
        const done_block = wip.block(0, "str_static_lane_done") catch return error.OutOfMemory;

        _ = wip.brCond(in_range, compare_block, done_block, .then_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = compare_block };
        const inline_bytes = self.targetWordSize() * 3;
        const small_last_full_offset = inline_bytes - @sizeOf(u64);
        const small_full_load_available = wip.icmp(.ule, offset, builder.intValue(usize_ty, small_last_full_offset) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const runtime_len_ge_word = wip.icmp(.uge, runtime_len, builder.intValue(usize_ty, 8) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const heap_last_full_offset = wip.bin(.sub, runtime_len, builder.intValue(usize_ty, 8) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const heap_offset_in_full_range = wip.icmp(.ule, offset, heap_last_full_offset, "") catch return error.OutOfMemory;
        const heap_full_load_available = wip.bin(.@"and", runtime_len_ge_word, heap_offset_in_full_range, "") catch return error.OutOfMemory;
        const full_load_available = wip.select(.normal, is_small, small_full_load_available, heap_full_load_available, "") catch return error.OutOfMemory;
        _ = wip.brCond(full_load_available, full_load_block, tail_block, .then_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = full_load_block };
        const full_load_ptr = wip.gep(.inbounds, .i8, data_ptr, &.{offset}, "") catch return error.OutOfMemory;
        const full_word = wip.load(.normal, .i64, full_load_ptr, LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
        _ = wip.br(mask_block) catch return error.OutOfMemory;

        wip.cursor = .{ .block = tail_block };
        var tail_word = builder.intValue(.i64, 0) catch return error.OutOfMemory;
        inline for (0..8) |index| {
            const byte_needed = wip.icmp(.ugt, active_len, builder.intValue(usize_ty, @as(i64, @intCast(index))) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
            const branch_block = wip.cursor.block;
            const load_byte_block = wip.block(1, "str_static_lane_tail_byte") catch return error.OutOfMemory;
            const next_byte_block = wip.block(2, "str_static_lane_tail_next") catch return error.OutOfMemory;
            _ = wip.brCond(byte_needed, load_byte_block, next_byte_block, .then_likely) catch return error.OutOfMemory;

            wip.cursor = .{ .block = load_byte_block };
            const byte_offset = if (index == 0)
                offset
            else
                wip.bin(.add, offset, builder.intValue(usize_ty, @as(i64, @intCast(index))) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
            const byte_ptr = wip.gep(.inbounds, .i8, data_ptr, &.{byte_offset}, "") catch return error.OutOfMemory;
            const byte = wip.load(.normal, .i8, byte_ptr, LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
            var byte_word = try self.coerceScalar(byte, .i64, false);
            if (index != 0) {
                byte_word = wip.bin(.shl, byte_word, builder.intValue(.i64, @as(i64, @intCast(index * 8))) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
            }
            const updated_word = wip.bin(.@"or", tail_word, byte_word, "") catch return error.OutOfMemory;
            _ = wip.br(next_byte_block) catch return error.OutOfMemory;

            wip.cursor = .{ .block = next_byte_block };
            const next_tail_word = wip.phi(.i64, "str_static_lane_tail_word") catch return error.OutOfMemory;
            next_tail_word.finish(&.{ tail_word, updated_word }, &.{ branch_block, load_byte_block }, wip);
            tail_word = next_tail_word.toValue();
        }
        const tail_exit_block = wip.cursor.block;
        _ = wip.br(mask_block) catch return error.OutOfMemory;

        wip.cursor = .{ .block = mask_block };
        const runtime_word_phi = wip.phi(.i64, "str_static_lane_word") catch return error.OutOfMemory;
        runtime_word_phi.finish(&.{ full_word, tail_word }, &.{ full_load_block, tail_exit_block }, wip);
        const runtime_word = runtime_word_phi.toValue();
        const active_len_is_zero = wip.icmp(.eq, active_len, builder.intValue(usize_ty, 0) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        _ = wip.brCond(active_len_is_zero, mask_zero_block, mask_full_block, .else_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = mask_zero_block };
        const zero_mask = builder.intValue(.i64, 0) catch return error.OutOfMemory;
        _ = wip.br(finish_block) catch return error.OutOfMemory;

        wip.cursor = .{ .block = mask_full_block };
        const active_len_is_full = wip.icmp(.eq, active_len, builder.intValue(usize_ty, 8) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        _ = wip.brCond(active_len_is_full, mask_eight_block, mask_partial_block, .then_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = mask_eight_block };
        const full_mask = builder.intValue(.i64, -1) catch return error.OutOfMemory;
        _ = wip.br(finish_block) catch return error.OutOfMemory;

        wip.cursor = .{ .block = mask_partial_block };
        const active_bits = wip.bin(.shl, active_len, builder.intValue(usize_ty, 3) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const one = builder.intValue(.i64, 1) catch return error.OutOfMemory;
        const active_bits_i64 = try self.coerceScalar(active_bits, .i64, false);
        const shifted_one = wip.bin(.shl, one, active_bits_i64, "") catch return error.OutOfMemory;
        const partial_mask = wip.bin(.sub, shifted_one, one, "") catch return error.OutOfMemory;
        _ = wip.br(finish_block) catch return error.OutOfMemory;

        wip.cursor = .{ .block = finish_block };
        const mask_phi = wip.phi(.i64, "str_static_lane_mask_value") catch return error.OutOfMemory;
        mask_phi.finish(&.{ zero_mask, full_mask, partial_mask }, &.{ mask_zero_block, mask_eight_block, mask_partial_block }, wip);
        const mask = mask_phi.toValue();
        const is_equal = switch (mode) {
            .exact => blk: {
                const runtime_masked = wip.bin(.@"and", runtime_word, mask, "") catch return error.OutOfMemory;
                const static_masked = wip.bin(.@"and", static_word, mask, "") catch return error.OutOfMemory;
                break :blk wip.icmp(.eq, runtime_masked, static_masked, "") catch return error.OutOfMemory;
            },
            .caseless => try self.emitSwarCaselessAsciiEqualMasked(runtime_word, static_word, mask),
        };
        try self.storeBool(target_ptr, is_equal);
        _ = wip.br(done_block) catch return error.OutOfMemory;

        wip.cursor = .{ .block = done_block };
    }

    fn llvmI64Value(self: *MonoLlvmCodeGen, value: u64) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        return builder.intValue(.i64, @as(i64, @bitCast(value))) catch return error.OutOfMemory;
    }

    fn emitSwarSplat(self: *MonoLlvmCodeGen, byte: u8) Error!LlvmBuilder.Value {
        return try self.llvmI64Value(@as(u64, byte) * 0x0101010101010101);
    }

    fn emitSwarNot(self: *MonoLlvmCodeGen, value: LlvmBuilder.Value) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        return wip.bin(.xor, value, try self.llvmI64Value(std.math.maxInt(u64)), "") catch return error.OutOfMemory;
    }

    fn emitSwarZeroHigh(self: *MonoLlvmCodeGen, word: LlvmBuilder.Value) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const low7 = try self.llvmI64Value(0x7f7f7f7f7f7f7f7f);
        const highs = try self.llvmI64Value(0x8080808080808080);
        const masked = wip.bin(.@"and", word, low7, "") catch return error.OutOfMemory;
        const added = wip.bin(.add, masked, low7, "") catch return error.OutOfMemory;
        const carries_or_original = wip.bin(.@"or", added, word, "") catch return error.OutOfMemory;
        const inverted = try self.emitSwarNot(carries_or_original);
        return wip.bin(.@"and", inverted, highs, "") catch return error.OutOfMemory;
    }

    fn emitSwarByteEq(self: *MonoLlvmCodeGen, word: LlvmBuilder.Value, byte: u8) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const diff = wip.bin(.xor, word, try self.emitSwarSplat(byte), "") catch return error.OutOfMemory;
        return try self.emitSwarZeroHigh(diff);
    }

    fn emitSwarByteGeAscii(self: *MonoLlvmCodeGen, word: LlvmBuilder.Value, lo: u8) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const low7 = try self.llvmI64Value(0x7f7f7f7f7f7f7f7f);
        const highs = try self.llvmI64Value(0x8080808080808080);
        const masked = wip.bin(.@"and", word, low7, "") catch return error.OutOfMemory;
        const biased = wip.bin(.add, masked, try self.emitSwarSplat(0x80 - lo), "") catch return error.OutOfMemory;
        const not_word = try self.emitSwarNot(word);
        const ascii_ge = wip.bin(.@"and", biased, not_word, "") catch return error.OutOfMemory;
        return wip.bin(.@"and", ascii_ge, highs, "") catch return error.OutOfMemory;
    }

    fn emitSwarByteLeAscii(self: *MonoLlvmCodeGen, word: LlvmBuilder.Value, hi: u8) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const low7 = try self.llvmI64Value(0x7f7f7f7f7f7f7f7f);
        const highs = try self.llvmI64Value(0x8080808080808080);
        const masked = wip.bin(.@"and", word, low7, "") catch return error.OutOfMemory;
        const biased = wip.bin(.add, masked, try self.emitSwarSplat(0x7f - hi), "") catch return error.OutOfMemory;
        const carries_or_original = wip.bin(.@"or", biased, word, "") catch return error.OutOfMemory;
        const ascii_le = try self.emitSwarNot(carries_or_original);
        return wip.bin(.@"and", ascii_le, highs, "") catch return error.OutOfMemory;
    }

    fn emitSwarByteInAsciiRange(self: *MonoLlvmCodeGen, word: LlvmBuilder.Value, lo: u8, hi: u8) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const ge = try self.emitSwarByteGeAscii(word, lo);
        const le = try self.emitSwarByteLeAscii(word, hi);
        return wip.bin(.@"and", ge, le, "") catch return error.OutOfMemory;
    }

    fn emitSwarCaselessAsciiEqualMasked(
        self: *MonoLlvmCodeGen,
        left: LlvmBuilder.Value,
        right: LlvmBuilder.Value,
        active: LlvmBuilder.Value,
    ) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;

        // This mirrors builtins.str.wordCaselessAsciiEqualMasked. The fixed
        // integer dataflow is intentional: optimized native field dispatch
        // should become a handful of word-size mask operations, not a helper
        // call and not a byte loop. Exact-equal lanes are accepted for any byte;
        // lanes that differ must differ by ASCII's case bit and must be letters.
        //
        // The two implementations must agree byte-for-byte. The builtin side is
        // pinned exhaustively by `builtins.str.swar_caseless_word_vectors`; this
        // emitted routine is pinned end-to-end through caseless record-field
        // dispatch by `src/cli/test/http_header_decoder_platform_test.zig`,
        // which builds an LLVM executable and dispatches on case-varying header
        // names. There is no isolated JIT harness for a single emitted helper,
        // so full-pipeline execution is the only way to run this routine.
        const zero = builder.intValue(.i64, 0) catch return error.OutOfMemory;
        const highs = try self.llvmI64Value(0x8080808080808080);
        const ascii_case = try self.llvmI64Value(0x2020202020202020);

        const raw_diff = wip.bin(.xor, left, right, "") catch return error.OutOfMemory;
        const diff = wip.bin(.@"and", raw_diff, active, "") catch return error.OutOfMemory;
        const diff_zero = wip.icmp(.eq, diff, zero, "") catch return error.OutOfMemory;

        const active_highs = wip.bin(.@"and", active, highs, "") catch return error.OutOfMemory;
        const exact_bytes = wip.bin(.@"and", try self.emitSwarZeroHigh(diff), active_highs, "") catch return error.OutOfMemory;
        const case_diff_bytes = wip.bin(.@"and", try self.emitSwarByteEq(diff, 0x20), active_highs, "") catch return error.OutOfMemory;
        const accepted_diff_bytes = wip.bin(.@"or", exact_bytes, case_diff_bytes, "") catch return error.OutOfMemory;
        const valid_diff = wip.icmp(.eq, accepted_diff_bytes, active_highs, "") catch return error.OutOfMemory;

        const left_lower = wip.bin(.@"or", left, ascii_case, "") catch return error.OutOfMemory;
        const left_alpha = wip.bin(.@"and", try self.emitSwarByteInAsciiRange(left_lower, 'a', 'z'), active_highs, "") catch return error.OutOfMemory;
        const invalid_alpha = wip.bin(.@"and", case_diff_bytes, try self.emitSwarNot(left_alpha), "") catch return error.OutOfMemory;
        const valid_alpha = wip.icmp(.eq, invalid_alpha, zero, "") catch return error.OutOfMemory;

        const folded_ok = wip.bin(.@"and", valid_diff, valid_alpha, "") catch return error.OutOfMemory;
        return wip.bin(.@"or", diff_zero, folded_ok, "") catch return error.OutOfMemory;
    }

    /// `unique_args` is non-null for wrappers whose first argument carries the
    /// op's runtime uniqueness check; the update-mode argument is appended just
    /// before roc_ops.
    fn emitStrRetBuiltin(self: *MonoLlvmCodeGen, target: LocalId, name: []const u8, args: anytype, unique_args: ?u64) Error!void {
        var call_args = try self.rocStrArgs2(GuardedList.at(args, 0), GuardedList.at(args, 1), true);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        if (unique_args) |mask| try self.appendUpdateModeArg(&call_args, mask);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinOut(name, call_args.types.items, call_args.values.items);
    }

    /// `unique_args` is non-null for wrappers whose first argument carries the
    /// op's runtime uniqueness check; the update-mode argument is appended just
    /// before roc_ops.
    fn emitStrUnaryRetBuiltin(self: *MonoLlvmCodeGen, target: LocalId, name: []const u8, arg: LocalId, unique_args: ?u64) Error!void {
        var call_args = try self.rocStrArgs1(arg);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        if (unique_args) |mask| try self.appendUpdateModeArg(&call_args, mask);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinOut(name, call_args.types.items, call_args.values.items);
    }

    fn emitStrCountUtf8Bytes(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const result = if (self.deferredStrCapture(arg)) |capture|
            try self.deferredStrCaptureLen(capture)
        else
            try self.emitRocStrLen(self.slot(arg).ptr);
        try self.storeIntToLayout(self.slot(target).ptr, result, self.localLayout(target));
    }

    fn emitStrGetUtf8ByteUnsafe(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        var call_args = try self.rocStrArgs1(GuardedList.at(args, 0));
        defer call_args.deinit(self.allocator);
        const index = try self.coerceScalar(
            try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1))),
            .i64,
            false,
        );
        try call_args.append(self.allocator, .i64, index);
        const result = try self.callBuiltin(
            builtinSymbol(LowLevelBuiltins.strOp(.str_get_utf8_byte_unsafe)),
            .i8,
            call_args.types.items,
            call_args.values.items,
        );
        try self.storeScalar(self.slot(target).ptr, self.localLayout(target), result);
    }

    fn emitStrSubstringUnsafe(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        var call_args = try self.rocStrArgs1(GuardedList.at(args, 0));
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        const start = try self.coerceScalar(
            try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1))),
            .i64,
            false,
        );
        const length = try self.coerceScalar(
            try self.loadScalar(self.slot(GuardedList.at(args, 2)).ptr, self.localLayout(GuardedList.at(args, 2))),
            .i64,
            false,
        );
        try call_args.append(self.allocator, .i64, start);
        try call_args.append(self.allocator, .i64, length);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid(
            builtinSymbol(LowLevelBuiltins.strOp(.str_substring_unsafe)),
            call_args.types.items,
            call_args.values.items,
        );
    }

    fn emitStrSplitFirst(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        const target_slot = self.slot(target);
        const info = try self.resolveStrSplitFirstLayout(target_slot.layout_idx);
        if (target_slot.size > 0) try self.zeroBytes(target_slot.ptr, target_slot.size);

        const layout_ptr = try self.allocEntryBlockSlot(
            .i8,
            @sizeOf(builtins.dev_wrappers.StrSplitFirstLayout),
            LlvmBuilder.Alignment.fromByteUnits(@alignOf(builtins.dev_wrappers.StrSplitFirstLayout)),
            "str_split_first_layout",
        );

        try self.storeRawInt(layout_ptr, 0, .i32, info.after_offset, 4);
        try self.storeRawInt(layout_ptr, 4, .i32, info.before_offset, 4);
        try self.storeRawInt(layout_ptr, 8, .i32, info.found_offset, 4);

        var call_args = try self.rocStrArgs2(GuardedList.at(args, 0), GuardedList.at(args, 1), false);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), target_slot.ptr);
        try call_args.append(self.allocator, try self.ptrType(), layout_ptr);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid(builtinSymbol(LowLevelBuiltins.strOp(.str_split_first)), call_args.types.items, call_args.values.items);
    }

    fn emitStrSplitLast(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        const target_slot = self.slot(target);
        const info = try self.resolveStrSplitLastLayout(target_slot.layout_idx);
        if (target_slot.size > 0) try self.zeroBytes(target_slot.ptr, target_slot.size);

        const layout_ptr = try self.allocEntryBlockSlot(
            .i8,
            @sizeOf(builtins.dev_wrappers.StrSplitLastLayout),
            LlvmBuilder.Alignment.fromByteUnits(@alignOf(builtins.dev_wrappers.StrSplitLastLayout)),
            "str_split_last_layout",
        );

        try self.storeRawInt(layout_ptr, 0, .i32, info.after_offset, 4);
        try self.storeRawInt(layout_ptr, 4, .i32, info.before_offset, 4);
        try self.storeRawInt(layout_ptr, 8, .i32, info.found_offset, 4);

        var call_args = try self.rocStrArgs2(GuardedList.at(args, 0), GuardedList.at(args, 1), false);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), target_slot.ptr);
        try call_args.append(self.allocator, try self.ptrType(), layout_ptr);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid(builtinSymbol(LowLevelBuiltins.strOp(.str_split_last)), call_args.types.items, call_args.values.items);
    }

    fn emitStrDropPrefixCaselessAscii(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        const target_slot = self.slot(target);
        const info = try self.resolveStrDropPrefixCaselessAsciiLayout(target_slot.layout_idx);
        if (target_slot.size > 0) try self.zeroBytes(target_slot.ptr, target_slot.size);

        const layout_ptr = try self.allocEntryBlockSlot(
            .i8,
            @sizeOf(builtins.dev_wrappers.StrDropPrefixCaselessAsciiLayout),
            LlvmBuilder.Alignment.fromByteUnits(@alignOf(builtins.dev_wrappers.StrDropPrefixCaselessAsciiLayout)),
            "str_drop_prefix_caseless_ascii_layout",
        );

        try self.storeRawInt(layout_ptr, 0, .i32, info.after_offset, 4);
        try self.storeRawInt(layout_ptr, 4, .i32, info.found_offset, 4);

        var call_args = try self.rocStrArgs2(GuardedList.at(args, 0), GuardedList.at(args, 1), false);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), target_slot.ptr);
        try call_args.append(self.allocator, try self.ptrType(), layout_ptr);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid(builtinSymbol(LowLevelBuiltins.strOp(.str_drop_prefix_caseless_ascii)), call_args.types.items, call_args.values.items);
    }

    fn emitStrJoinWith(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        var call_args = try self.rocListArgs1(GuardedList.at(args, 0));
        defer call_args.deinit(self.allocator);
        const sep_args = try self.rocStrArgs1(GuardedList.at(args, 1));
        defer {
            var owned = sep_args;
            owned.deinit(self.allocator);
        }
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.types.appendSlice(self.allocator, sep_args.types.items);
        try call_args.values.appendSlice(self.allocator, sep_args.values.items);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid(builtinSymbol(LowLevelBuiltins.strOp(.str_join_with)), call_args.types.items, call_args.values.items);
    }

    fn emitStrRepeat(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        var call_args = try self.rocStrArgs1(GuardedList.at(args, 0));
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, .i64, try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1))), .i64, false));
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid(builtinSymbol(LowLevelBuiltins.strOp(.str_repeat)), call_args.types.items, call_args.values.items);
    }

    fn emitStrWithCapacity(self: *MonoLlvmCodeGen, target: LocalId, capacity: LocalId) Error!void {
        const cap = try self.coerceScalar(try self.loadScalar(self.slot(capacity).ptr, self.localLayout(capacity)), .i64, false);
        try self.callBuiltinVoid(builtinSymbol(LowLevelBuiltins.strOp(.str_with_capacity)), &.{ try self.ptrType(), .i64, try self.ptrType() }, &.{ self.slot(target).ptr, cap, self.rocOps() });
    }

    fn emitStrReserve(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, unique_args: u64) Error!void {
        var call_args = try self.rocStrArgs1(GuardedList.at(args, 0));
        defer call_args.deinit(self.allocator);
        const spare = try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1))), .i64, false);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, .i64, spare);
        try self.appendUpdateModeArg(&call_args, unique_args);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid(builtinSymbol(LowLevelBuiltins.strOp(.str_reserve)), call_args.types.items, call_args.values.items);
    }

    fn emitStrToUtf8(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        var call_args = try self.rocStrArgs1(arg);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid(builtinSymbol(LowLevelBuiltins.strOp(.str_to_utf8)), call_args.types.items, call_args.values.items);
    }

    fn emitStrFromUtf8Lossy(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        var call_args = try self.rocListArgs1(arg);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid(builtinSymbol(LowLevelBuiltins.strOp(.str_from_utf8_lossy)), call_args.types.items, call_args.values.items);
    }

    fn emitStrFromUtf8(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const target_slot = self.slot(target);
        const info = try self.resolveStrFromUtf8Layout(target_slot.layout_idx);
        if (target_slot.size > 0) try self.zeroBytes(target_slot.ptr, target_slot.size);

        const layout_ptr = try self.allocEntryBlockSlot(
            .i8,
            @sizeOf(builtins.dev_wrappers.StrFromUtf8Layout),
            LlvmBuilder.Alignment.fromByteUnits(@alignOf(builtins.dev_wrappers.StrFromUtf8Layout)),
            "str_from_utf8_layout",
        );

        try self.storeRawInt(layout_ptr, 0, .i64, info.ok_tag, 8);
        try self.storeRawInt(layout_ptr, 8, .i64, info.err_tag, 8);
        try self.storeRawInt(layout_ptr, 16, .i32, info.outer_disc_offset, 4);
        try self.storeRawInt(layout_ptr, 20, .i32, info.outer_disc_size, 4);
        try self.storeRawInt(layout_ptr, 24, .i32, info.err_index_offset, 4);
        try self.storeRawInt(layout_ptr, 28, .i32, info.err_problem_offset, 4);
        try self.storeRawInt(layout_ptr, 32, .i32, info.inner_disc_offset, 4);
        try self.storeRawInt(layout_ptr, 36, .i32, info.inner_disc_size, 4);
        try self.storeRawInt(layout_ptr, 40, .i32, info.inner_bad_utf8_tag, 4);

        var call_args = try self.rocListArgs1(arg);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), target_slot.ptr);
        try call_args.append(self.allocator, try self.ptrType(), layout_ptr);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid(builtinSymbol(LowLevelBuiltins.strOp(.str_from_utf8)), call_args.types.items, call_args.values.items);
    }

    fn emitIntFromStr(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId, width: u8, signed: bool) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const allocated = try self.allocAggregateTarget(target);
        const disc_offset = try self.tagDiscriminantOffset(allocated.layout_idx);
        var call_args = try self.rocStrArgs1(arg);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), allocated.ptr);
        try call_args.append(self.allocator, .i8, builder.intValue(.i8, width) catch return error.OutOfMemory);
        try call_args.append(self.allocator, .i1, builder.intValue(.i1, @intFromBool(signed)) catch return error.OutOfMemory);
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, disc_offset) catch return error.OutOfMemory);
        try self.callBuiltinVoid(builtinSymbol(LowLevelBuiltins.numFromStr(.int)), call_args.types.items, call_args.values.items);
    }

    fn emitDecFromStr(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const allocated = try self.allocAggregateTarget(target);
        const disc_offset = try self.tagDiscriminantOffset(allocated.layout_idx);
        var call_args = try self.rocStrArgs1(arg);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), allocated.ptr);
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, disc_offset) catch return error.OutOfMemory);
        try self.callBuiltinVoid(builtinSymbol(LowLevelBuiltins.numFromStr(.dec)), call_args.types.items, call_args.values.items);
    }

    fn emitFloatFromStr(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId, width: u8) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const allocated = try self.allocAggregateTarget(target);
        const disc_offset = try self.tagDiscriminantOffset(allocated.layout_idx);
        var call_args = try self.rocStrArgs1(arg);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), allocated.ptr);
        try call_args.append(self.allocator, .i8, builder.intValue(.i8, width) catch return error.OutOfMemory);
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, disc_offset) catch return error.OutOfMemory);
        try self.callBuiltinVoid(builtinSymbol(LowLevelBuiltins.numFromStr(.float)), call_args.types.items, call_args.values.items);
    }

    fn emitIntToStr(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const arg_layout = self.localLayout(arg);
        const bits = self.intBits(arg_layout);
        const value = try self.coerceScalar(try self.loadScalar(self.slot(arg).ptr, arg_layout), .i128, arg_layout.isSigned());
        const lo = try self.coerceScalar(value, .i64, false);
        const hi = (self.wip orelse return error.CompilationFailed).bin(.lshr, value, builder.intValue(.i128, 64) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const hi64 = try self.coerceScalar(hi, .i64, false);
        const byte_width: u8 = @intCast(bits / 8);
        try self.callBuiltinVoid(builtinSymbol(LowLevelBuiltins.numToStr(.int)), &.{ try self.ptrType(), .i64, .i64, .i8, .i1, try self.ptrType() }, &.{
            self.slot(target).ptr,
            lo,
            hi64,
            builder.intValue(.i8, byte_width) catch return error.OutOfMemory,
            builder.intValue(.i1, @intFromBool(arg_layout.isSigned())) catch return error.OutOfMemory,
            self.rocOps(),
        });
    }

    fn emitFloatToStr(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const arg_layout = self.localLayout(arg);
        const value = try self.loadScalar(self.slot(arg).ptr, arg_layout);
        const bits = if (arg_layout == .f32)
            try self.coerceScalar(wip.cast(.bitcast, value, .i32, "") catch return error.OutOfMemory, .i64, false)
        else
            wip.cast(.bitcast, value, .i64, "") catch return error.OutOfMemory;
        try self.callBuiltinVoid(builtinSymbol(LowLevelBuiltins.numToStr(.float)), &.{ try self.ptrType(), .i64, .i1, try self.ptrType() }, &.{
            self.slot(target).ptr,
            bits,
            builder.intValue(.i1, @intFromBool(arg_layout == .f32)) catch return error.OutOfMemory,
            self.rocOps(),
        });
    }

    fn emitFloatBitCast(self: *MonoLlvmCodeGen, target: LocalId, op: lir.LowLevel, arg: LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const value = try self.loadScalar(self.slot(arg).ptr, self.localLayout(arg));
        const target_ty: LlvmBuilder.Type = if (op == .f32_to_bits)
            .i32
        else if (op == .f32_from_bits)
            .float
        else if (op == .f64_to_bits)
            .i64
        else if (op == .f64_from_bits)
            .double
        else
            unreachable;
        const raw_bits = wip.cast(.bitcast, value, target_ty, "") catch return error.OutOfMemory;
        const normalized = if (op == .f32_to_bits) blk: {
            const magnitude = wip.bin(
                .@"and",
                raw_bits,
                builder.intValue(.i32, 0x7fff_ffff) catch return error.OutOfMemory,
                "",
            ) catch return error.OutOfMemory;
            const is_nan = wip.icmp(
                .ugt,
                magnitude,
                builder.intValue(.i32, 0x7f80_0000) catch return error.OutOfMemory,
                "",
            ) catch return error.OutOfMemory;
            break :blk wip.select(
                .normal,
                is_nan,
                builder.intValue(.i32, builtins.float_bits.normalized_f32_nan_bits) catch return error.OutOfMemory,
                raw_bits,
                "",
            ) catch return error.OutOfMemory;
        } else if (op == .f64_to_bits) blk: {
            const magnitude = wip.bin(
                .@"and",
                raw_bits,
                builder.intValue(.i64, 0x7fff_ffff_ffff_ffff) catch return error.OutOfMemory,
                "",
            ) catch return error.OutOfMemory;
            const is_nan = wip.icmp(
                .ugt,
                magnitude,
                builder.intValue(.i64, 0x7ff0_0000_0000_0000) catch return error.OutOfMemory,
                "",
            ) catch return error.OutOfMemory;
            break :blk wip.select(
                .normal,
                is_nan,
                builder.intValue(.i64, builtins.float_bits.normalized_f64_nan_bits) catch return error.OutOfMemory,
                raw_bits,
                "",
            ) catch return error.OutOfMemory;
        } else if (op == .f32_from_bits or op == .f64_from_bits)
            raw_bits
        else
            unreachable;
        try self.storeScalar(self.slot(target).ptr, self.localLayout(target), normalized);
    }

    fn emitNumericSqrt(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        if (self.localLayout(target) == .dec) {
            const value = try self.loadScalar(self.slot(arg).ptr, .dec);
            const result = try self.callDecUnaryBuiltin(builtinSymbol(LowLevelBuiltins.unaryMathDec(.num_sqrt)), value);
            try self.storeScalar(self.slot(target).ptr, .dec, result);
            return;
        }
        try self.emitNumericFloatUnaryIntrinsic(target, arg, .sqrt);
    }

    fn emitNumericUnaryMath(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId, op: lir.LowLevel) Error!void {
        if (self.localLayout(target) == .dec) {
            const value = try self.loadScalar(self.slot(arg).ptr, .dec);
            const result = try self.callDecUnaryBuiltin(LowLevelBuiltins.unaryMathDec(op).symbolName(), value);
            try self.storeScalar(self.slot(target).ptr, .dec, result);
            return;
        }

        const target_layout = self.localLayout(target);
        try self.emitNumericFloatUnaryBuiltin(
            target,
            arg,
            LowLevelBuiltins.unaryMathFloat(op, target_layout == .f32).symbolName(),
        );
    }

    fn emitNumericFloatUnaryIntrinsic(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId, intrinsic: LlvmBuilder.Intrinsic) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const target_layout = self.localLayout(target);
        const target_ty = self.scalarType(target_layout);
        const value = try self.coerceScalar(try self.loadScalar(self.slot(arg).ptr, self.localLayout(arg)), target_ty, false);
        const result = wip.callIntrinsic(
            .normal,
            .none,
            intrinsic,
            &.{target_ty},
            &.{value},
            "",
        ) catch return error.OutOfMemory;
        try self.storeScalar(self.slot(target).ptr, target_layout, result);
    }

    fn emitNumericFloatUnaryBuiltin(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId, name: []const u8) Error!void {
        const target_layout = self.localLayout(target);
        const target_ty: LlvmBuilder.Type = switch (target_layout) {
            .f32 => .float,
            .f64 => .double,
            .bool, .str, .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .dec, .opaque_ptr, .zst, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2, _ => return error.UnsupportedLowLevel,
        };
        const value = try self.coerceScalar(try self.loadScalar(self.slot(arg).ptr, self.localLayout(arg)), target_ty, false);
        const result = try self.callBuiltin(
            name,
            target_ty,
            &.{target_ty},
            &.{value},
        );
        try self.storeScalar(self.slot(target).ptr, target_layout, result);
    }

    fn emitNumericFloatPow(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        const target_layout = self.localLayout(target);
        const target_ty: LlvmBuilder.Type = switch (target_layout) {
            .f32 => .float,
            .f64 => .double,
            .bool, .str, .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .dec, .opaque_ptr, .zst, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2, _ => return error.UnsupportedLowLevel,
        };
        const lhs = try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 0)).ptr, self.localLayout(GuardedList.at(args, 0))), target_ty, false);
        const rhs = try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1))), target_ty, false);
        const result = try self.callBuiltin(
            LowLevelBuiltins.floatPow(target_layout == .f32).symbolName(),
            target_ty,
            &.{ target_ty, target_ty },
            &.{ lhs, rhs },
        );
        try self.storeScalar(self.slot(target).ptr, target_layout, result);
    }

    fn emitDecPow(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        const lhs = try self.loadScalar(self.slot(GuardedList.at(args, 0)).ptr, .dec);
        const rhs = try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, .dec);
        const result = try self.callI128BinaryBuiltin(builtinSymbol(LowLevelBuiltins.decBinaryArith(.num_pow)), lhs, rhs, true);
        try self.storeScalar(self.slot(target).ptr, .dec, result);
    }

    fn emitDecToStr(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const value = try self.loadScalar(self.slot(arg).ptr, .dec);
        const lo = try self.coerceScalar(value, .i64, false);
        const hi = (self.wip orelse return error.CompilationFailed).bin(.lshr, value, builder.intValue(.i128, 64) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const hi64 = try self.coerceScalar(hi, .i64, false);
        try self.callBuiltinVoid(builtinSymbol(LowLevelBuiltins.numToStr(.dec)), &.{ try self.ptrType(), .i64, .i64, try self.ptrType() }, &.{ self.slot(target).ptr, lo, hi64, self.rocOps() });
    }

    fn emitNumToStr(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        switch (self.localLayout(arg)) {
            .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => try self.emitIntToStr(target, arg),
            .dec => try self.emitDecToStr(target, arg),
            .f32, .f64 => try self.emitFloatToStr(target, arg),
            .bool, .str, .opaque_ptr, .zst, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2, _ => return error.CompilationFailed,
        }
    }

    const CallArgs = struct {
        types: std.ArrayList(LlvmBuilder.Type),
        values: std.ArrayList(LlvmBuilder.Value),

        fn init() CallArgs {
            return .{ .types = .empty, .values = .empty };
        }

        fn deinit(self: *CallArgs, allocator: Allocator) void {
            self.types.deinit(allocator);
            self.values.deinit(allocator);
        }

        fn append(self: *CallArgs, allocator: Allocator, ty: LlvmBuilder.Type, value: LlvmBuilder.Value) Allocator.Error!void {
            try self.types.append(allocator, ty);
            try self.values.append(allocator, value);
        }

        fn prepend(self: *CallArgs, allocator: Allocator, ty: LlvmBuilder.Type, value: LlvmBuilder.Value) Allocator.Error!void {
            try self.types.insert(allocator, 0, ty);
            try self.values.insert(allocator, 0, value);
        }
    };

    fn rocStrArgs1(self: *MonoLlvmCodeGen, arg: LocalId) Error!CallArgs {
        try self.materializeLocalIfDeferred(arg);
        var result = CallArgs.init();
        const ptr = self.slot(arg).ptr;
        try result.append(self.allocator, try self.ptrType(), try self.loadPointer(ptr));
        try result.append(self.allocator, self.ptrSizedIntType(), try self.loadUsize(try self.offsetPtr(ptr, self.rocStrLenOffset())));
        try result.append(self.allocator, self.ptrSizedIntType(), try self.loadUsize(try self.offsetPtr(ptr, self.rocStrCapacityOffset())));
        return result;
    }

    fn rocStrArgs2(self: *MonoLlvmCodeGen, a: LocalId, b: LocalId, _: bool) Error!CallArgs {
        var result = try self.rocStrArgs1(a);
        const rhs = try self.rocStrArgs1(b);
        defer {
            var owned = rhs;
            owned.deinit(self.allocator);
        }
        try result.types.appendSlice(self.allocator, rhs.types.items);
        try result.values.appendSlice(self.allocator, rhs.values.items);
        return result;
    }

    fn rocListArgs1(self: *MonoLlvmCodeGen, arg: LocalId) Error!CallArgs {
        var result = CallArgs.init();
        const ptr = self.slot(arg).ptr;
        try result.append(self.allocator, try self.ptrType(), try self.loadPointer(ptr));
        try result.append(self.allocator, self.ptrSizedIntType(), try self.loadUsize(try self.offsetPtr(ptr, self.rocListLenOffset())));
        try result.append(self.allocator, self.ptrSizedIntType(), try self.loadUsize(try self.offsetPtr(ptr, self.rocListCapacityOffset())));
        return result;
    }

    fn boxyListElementDescForLocals(
        self: *MonoLlvmCodeGen,
        abi: layout.Store.BuiltinListAbi,
        list_locals: []const LocalId,
        target_local: ?LocalId,
    ) ?BoxyListElementDesc {
        const elem_layout = abi.elem_layout_idx orelse return null;
        const elem_layout_value = self.layoutValue(elem_layout);
        const elem_is_erased_box = elem_layout_value.tag == .erased_box;
        if (!elem_is_erased_box and elem_layout_value.tag != .box) return null;

        for (list_locals) |local| {
            if (self.store.getLocal(local).boxy_desc) |desc| {
                return .{ .elem_layout = elem_layout, .desc = desc };
            }
        }
        if (target_local) |local| {
            if (self.store.getLocal(local).boxy_desc) |desc| {
                return .{ .elem_layout = elem_layout, .desc = desc };
            }
        }
        if (elem_is_erased_box) {
            llvmInvariantFmt(
                "erased-box list element layout {d} reached a refcounted list builtin without a Boxy list descriptor",
                .{@intFromEnum(elem_layout)},
            );
        }
        return null;
    }

    fn appendBoxyListElementDescArgs(
        self: *MonoLlvmCodeGen,
        call_args: *CallArgs,
        boxy_elem: BoxyListElementDesc,
    ) Error!void {
        try call_args.append(self.allocator, .i32, try self.boxyInt(.i32, @intFromEnum(boxy_elem.elem_layout)));
        try call_args.append(self.allocator, try self.ptrType(), try self.resolveBoxyDesc(boxy_elem.desc));
    }

    /// Appends element incref/decref callbacks for a runtime-checked list op.
    /// That RC is internal to the op, which serves both modes and makes no
    /// thread-confinement claim, so the callbacks are always the atomic
    /// helpers.
    fn appendListElementRcArgs(
        self: *MonoLlvmCodeGen,
        call_args: *CallArgs,
        abi: layout.Store.BuiltinListAbi,
        needs_incref: bool,
        needs_decref: bool,
    ) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const ptr_ty = try self.ptrType();
        const null_ptr = builder.nullValue(ptr_ty) catch return error.OutOfMemory;
        const enabled = abi.contains_refcounted;

        try call_args.append(
            self.allocator,
            .i1,
            builder.intValue(.i1, @intFromBool(enabled)) catch return error.OutOfMemory,
        );

        if (needs_incref) {
            const incref_fn = if (enabled)
                (try self.declareRcHelper(.{ .op = .incref, .layout_idx = abi.elem_layout_idx.? }, .atomic))
            else
                null;
            try call_args.append(
                self.allocator,
                ptr_ty,
                if (incref_fn) |func| func.toValue(builder) else null_ptr,
            );
        }

        if (needs_decref) {
            const decref_fn = if (enabled)
                (try self.declareRcHelper(.{ .op = .decref, .layout_idx = abi.elem_layout_idx.? }, .atomic))
            else
                null;
            try call_args.append(
                self.allocator,
                ptr_ty,
                if (decref_fn) |func| func.toValue(builder) else null_ptr,
            );
        }
    }

    /// Append a builtin wrapper's update-mode argument selected by the
    /// statement's statically-proven-unique argument mask: `.InPlace` when bit
    /// 0 says argument 0's runtime uniqueness check is redundant, `.Immutable`
    /// (checked) otherwise.
    fn appendUpdateModeArg(self: *MonoLlvmCodeGen, call_args: *CallArgs, unique_args: u64) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const mode = if ((unique_args & 1) != 0) builtins.utils.UpdateMode.InPlace else builtins.utils.UpdateMode.Immutable;
        try call_args.append(
            self.allocator,
            .i8,
            builder.intValue(.i8, @intFromEnum(mode)) catch return error.OutOfMemory,
        );
    }

    fn emitListGetUnsafe(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        const list_layout = self.localLayout(GuardedList.at(args, 0));
        const abi = self.boxyAwareBuiltinListAbi(list_layout);
        if (abi.elem_size == 0) return;
        const bytes = try self.loadPointer(self.slot(GuardedList.at(args, 0)).ptr);
        const idx = try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1))), self.ptrSizedIntType(), false);
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const offset = wip.bin(.mul, idx, builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const src = wip.gep(.inbounds, .i8, bytes, &.{offset}, "") catch return error.OutOfMemory;
        try self.copyBytes(self.slot(target).ptr, src, self.slot(target).size, self.slot(target).alignment);
    }

    fn emitListMapCanReuse(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, interchangeable: layout.WidthValues(bool)) Error!void {
        if (!interchangeable.get(self.layouts().targetUsize())) {
            // On this width the element layouts are not interchangeable, so the
            // in-place branch is statically dead: the result is a constant 0.
            const builder = self.builder orelse return error.CompilationFailed;
            const zero = builder.intValue(.i8, 0) catch return error.OutOfMemory;
            try self.storeIntToLayout(self.slot(target).ptr, zero, self.localLayout(target));
            return;
        }
        var call_args = try self.rocListArgs1(GuardedList.at(args, 0));
        defer call_args.deinit(self.allocator);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        const result = try self.callBuiltin(builtinSymbol(LowLevelBuiltins.listOp(.list_map_can_reuse)), .i8, call_args.types.items, call_args.values.items);
        try self.storeIntToLayout(self.slot(target).ptr, result, self.localLayout(target));
    }

    fn emitListMapExtractUnsafe(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        // Reads the element of the input type out of a buffer already typed
        // as the output element type; lowering guarantees both share one
        // stride, so the result layout supplies the stride and the copy size.
        const elem_size = self.slot(target).size;
        if (elem_size == 0) return;
        const bytes = try self.loadPointer(self.slot(GuardedList.at(args, 0)).ptr);
        const idx = try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1))), self.ptrSizedIntType(), false);
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const offset = wip.bin(.mul, idx, builder.intValue(self.ptrSizedIntType(), elem_size) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const src = wip.gep(.inbounds, .i8, bytes, &.{offset}, "") catch return error.OutOfMemory;
        try self.copyBytes(self.slot(target).ptr, src, elem_size, self.slot(target).alignment);
    }

    fn emitListMapWriteUnsafe(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        const elem_size = self.slot(GuardedList.at(args, 2)).size;
        if (elem_size != 0) {
            const bytes = try self.loadPointer(self.slot(GuardedList.at(args, 0)).ptr);
            const idx = try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1))), self.ptrSizedIntType(), false);
            const wip = self.wip orelse return error.CompilationFailed;
            const builder = self.builder orelse return error.CompilationFailed;
            const offset = wip.bin(.mul, idx, builder.intValue(self.ptrSizedIntType(), elem_size) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
            const dst = wip.gep(.inbounds, .i8, bytes, &.{offset}, "") catch return error.OutOfMemory;
            try self.copyBytes(dst, self.slot(GuardedList.at(args, 2)).ptr, elem_size, self.slot(GuardedList.at(args, 2)).alignment);
        }
        // The result is the same list value.
        try self.copyBytes(self.slot(target).ptr, self.slot(GuardedList.at(args, 0)).ptr, self.slot(target).size, self.slot(target).alignment);
    }

    fn emitListWithCapacity(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const abi = self.boxyAwareBuiltinListAbi(self.localLayout(target));
        // Reserving room for zero-sized elements needs no memory, and every
        // other zero-sized branch in this backend represents such a list as a
        // null pointer with zero capacity. Going to the builtin here would
        // hand back a refcounted zero-byte allocation instead, which those
        // branches then drop on the floor.
        if (abi.elem_size == 0) {
            const out_ptr = self.slot(target).ptr;
            const zero = builder.intValue(self.ptrSizedIntType(), 0) catch return error.OutOfMemory;
            try self.storePointer(out_ptr, builder.nullValue(try self.ptrType()) catch return error.OutOfMemory);
            try self.storeListLen(out_ptr, zero);
            try self.storeListCapacity(out_ptr, zero);
            return;
        }
        const cap = try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 0)).ptr, self.localLayout(GuardedList.at(args, 0))), .i64, false);
        try self.callBuiltinOut(builtinSymbol(LowLevelBuiltins.listOp(.list_with_capacity)), &.{ try self.ptrType(), .i64, .i32, self.ptrSizedIntType(), .i1, try self.ptrType() }, &.{
            self.slot(target).ptr,
            cap,
            builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory,
            builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory,
            builder.intValue(.i1, @intFromBool(abi.contains_refcounted)) catch return error.OutOfMemory,
            self.rocOps(),
        });
    }

    fn emitListCapacity(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const usize_ty = self.ptrSizedIntType();
        const one = builder.intValue(usize_ty, 1) catch return error.OutOfMemory;
        const zero = builder.intValue(usize_ty, 0) catch return error.OutOfMemory;
        const list_ptr = self.slot(arg).ptr;
        const len = try self.loadUsize(try self.offsetPtr(list_ptr, self.rocListLenOffset()));
        const cap_or_alloc = try self.loadUsize(try self.offsetPtr(list_ptr, self.rocListCapacityOffset()));
        const decoded_capacity = wip.bin(.lshr, cap_or_alloc, one, "") catch return error.OutOfMemory;
        const slice_tag = wip.bin(.@"and", cap_or_alloc, one, "") catch return error.OutOfMemory;
        const is_slice = wip.icmp(.ne, slice_tag, zero, "") catch return error.OutOfMemory;
        const capacity = wip.select(.normal, is_slice, len, decoded_capacity, "") catch return error.OutOfMemory;
        try self.storeIntToLayout(self.slot(target).ptr, capacity, self.localLayout(target));
    }

    fn emitListAppendUnsafe(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        const abi = self.boxyAwareBuiltinListAbi(self.localLayout(GuardedList.at(args, 0)));
        // A zero-sized element has no bytes to copy, so appending one only
        // bumps the length. Handled here for the same reason emitListConcat
        // handles it here: there is no data pointer to walk. The pointer and
        // capacity still travel across, so this stays correct for a list that
        // does hold an allocation rather than depending on the zero-sized
        // representation being unallocated.
        if (abi.elem_size == 0) {
            const builder = self.builder orelse return error.CompilationFailed;
            const wip = self.wip orelse return error.CompilationFailed;
            const src_ptr = self.slot(GuardedList.at(args, 0)).ptr;
            const out_ptr = self.slot(target).ptr;
            const len = try self.loadUsize(try self.offsetPtr(src_ptr, self.rocListLenOffset()));
            const one = builder.intValue(self.ptrSizedIntType(), 1) catch return error.OutOfMemory;
            const new_len = wip.bin(.add, len, one, "") catch return error.OutOfMemory;
            // listAppendUnsafe hands the incoming list's allocation to its
            // result, so carry the pointer and capacity across rather than
            // dropping them, which would leak whatever the source owned.
            try self.storePointer(out_ptr, try self.loadPointer(src_ptr));
            try self.storeListLen(out_ptr, new_len);
            try self.storeListCapacity(out_ptr, try self.loadUsize(try self.offsetPtr(src_ptr, self.rocListCapacityOffset())));
            return;
        }
        var call_args = try self.rocListArgs1(GuardedList.at(args, 0));
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, try self.ptrType(), self.slot(GuardedList.at(args, 1)).ptr);
        try call_args.append(self.allocator, self.ptrSizedIntType(), (self.builder orelse return error.CompilationFailed).intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinOut(builtinSymbol(LowLevelBuiltins.listOp(.list_append_unsafe)), call_args.types.items, call_args.values.items);
    }

    fn emitListConcat(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, unique_args: u64) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const lhs_local = GuardedList.at(args, 0);
        const rhs_local = GuardedList.at(args, 1);
        const abi = self.boxyAwareBuiltinListAbi(self.localLayout(target));
        if (abi.elem_size == 0) {
            const lhs_len = try self.loadUsize(try self.offsetPtr(self.slot(lhs_local).ptr, self.rocListLenOffset()));
            const rhs_len = try self.loadUsize(try self.offsetPtr(self.slot(rhs_local).ptr, self.rocListLenOffset()));
            const total_len = (self.wip orelse return error.CompilationFailed).bin(.add, lhs_len, rhs_len, "") catch return error.OutOfMemory;
            const null_ptr = builder.nullValue(try self.ptrType()) catch return error.OutOfMemory;
            try self.storePointer(self.slot(target).ptr, null_ptr);
            try self.storeListLen(self.slot(target).ptr, total_len);
            try self.storeListCapacity(self.slot(target).ptr, builder.intValue(self.ptrSizedIntType(), 0) catch return error.OutOfMemory);
            return;
        }
        var call_args = try self.rocListArgs1(lhs_local);
        defer call_args.deinit(self.allocator);
        const rhs = try self.rocListArgs1(rhs_local);
        defer {
            var owned = rhs;
            owned.deinit(self.allocator);
        }
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.types.appendSlice(self.allocator, rhs.types.items);
        try call_args.values.appendSlice(self.allocator, rhs.values.items);
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory);
        try call_args.append(self.allocator, self.ptrSizedIntType(), builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
        const boxy_elem = self.boxyListElementDescForLocals(abi, &.{ lhs_local, rhs_local }, target);
        if (boxy_elem) |elem| {
            try self.appendBoxyListElementDescArgs(&call_args, elem);
        } else {
            try self.appendListElementRcArgs(&call_args, abi, true, true);
        }
        // One bit per list argument (bit 0 = lhs, bit 1 = rhs), as one 8-byte
        // parameter so no two sub-8-byte parameters land adjacent on the stack.
        try call_args.append(self.allocator, .i64, builder.intValue(.i64, unique_args & 0b11) catch return error.OutOfMemory);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        if (boxy_elem != null) {
            try self.callBoxyVoid("roc_boxy_list_concat", call_args.types.items, call_args.values.items);
        } else {
            try self.callBuiltinOut(builtinSymbol(LowLevelBuiltins.listOp(.list_concat)), call_args.types.items, call_args.values.items);
        }
    }

    fn emitListAppendRangeWithin(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, unique_args: u64) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const abi = self.layouts().builtinListAbi(self.localLayout(target));
        if (abi.elem_size == 0) {
            const count = try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 2)).ptr, self.localLayout(GuardedList.at(args, 2))), self.ptrSizedIntType(), false);
            const len = try self.loadUsize(try self.offsetPtr(self.slot(GuardedList.at(args, 0)).ptr, self.rocListLenOffset()));
            const total_len = (self.wip orelse return error.CompilationFailed).bin(.add, len, count, "") catch return error.OutOfMemory;
            const null_ptr = builder.nullValue(try self.ptrType()) catch return error.OutOfMemory;
            try self.storePointer(self.slot(target).ptr, null_ptr);
            try self.storeListLen(self.slot(target).ptr, total_len);
            try self.storeListCapacity(self.slot(target).ptr, builder.intValue(self.ptrSizedIntType(), 0) catch return error.OutOfMemory);
            return;
        }
        var call_args = try self.rocListArgs1(GuardedList.at(args, 0));
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, .i64, try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1))), .i64, false));
        try call_args.append(self.allocator, .i64, try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 2)).ptr, self.localLayout(GuardedList.at(args, 2))), .i64, false));
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory);
        try call_args.append(self.allocator, self.ptrSizedIntType(), builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
        try self.appendListElementRcArgs(&call_args, abi, true, true);
        try self.appendUpdateModeArg(&call_args, unique_args);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinOut(builtinSymbol(LowLevelBuiltins.listOp(.list_append_range_within)), call_args.types.items, call_args.values.items);
    }

    fn emitListCopyRangeWithin(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const abi = self.layouts().builtinListAbi(self.localLayout(target));
        if (abi.elem_size == 0) {
            // Copying zero-sized elements within the list changes nothing.
            try self.copyLocal(target, GuardedList.at(args, 0));
            return;
        }
        var call_args = try self.rocListArgs1(GuardedList.at(args, 0));
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, .i64, try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1))), .i64, false));
        try call_args.append(self.allocator, .i64, try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 2)).ptr, self.localLayout(GuardedList.at(args, 2))), .i64, false));
        try call_args.append(self.allocator, .i64, try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 3)).ptr, self.localLayout(GuardedList.at(args, 3))), .i64, false));
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory);
        try call_args.append(self.allocator, self.ptrSizedIntType(), builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
        try self.appendListElementRcArgs(&call_args, abi, true, true);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinOut(builtinSymbol(LowLevelBuiltins.listOp(.list_copy_range_within)), call_args.types.items, call_args.values.items);
    }

    fn emitListAppendRangeWithinUnsafe(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const abi = self.layouts().builtinListAbi(self.localLayout(target));
        if (abi.elem_size == 0) {
            const count = try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 2)).ptr, self.localLayout(GuardedList.at(args, 2))), self.ptrSizedIntType(), false);
            const len = try self.loadUsize(try self.offsetPtr(self.slot(GuardedList.at(args, 0)).ptr, self.rocListLenOffset()));
            const total_len = (self.wip orelse return error.CompilationFailed).bin(.add, len, count, "") catch return error.OutOfMemory;
            const null_ptr = builder.nullValue(try self.ptrType()) catch return error.OutOfMemory;
            try self.storePointer(self.slot(target).ptr, null_ptr);
            try self.storeListLen(self.slot(target).ptr, total_len);
            try self.storeListCapacity(self.slot(target).ptr, builder.intValue(self.ptrSizedIntType(), 0) catch return error.OutOfMemory);
            return;
        }
        var call_args = try self.rocListArgs1(GuardedList.at(args, 0));
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, .i64, try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1))), .i64, false));
        try call_args.append(self.allocator, .i64, try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 2)).ptr, self.localLayout(GuardedList.at(args, 2))), .i64, false));
        try call_args.append(self.allocator, self.ptrSizedIntType(), builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
        try self.appendListElementRcArgs(&call_args, abi, true, false);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinOut(builtinSymbol(LowLevelBuiltins.listOp(.list_append_range_within_unsafe)), call_args.types.items, call_args.values.items);
    }

    fn emitListAppendLeBytes(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, unique_args: u64) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        var call_args = try self.rocListArgs1(GuardedList.at(args, 0));
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, .i64, try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1))), .i64, false));
        try call_args.append(self.allocator, .i64, try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 2)).ptr, self.localLayout(GuardedList.at(args, 2))), .i64, false));
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, 1) catch return error.OutOfMemory);
        try self.appendUpdateModeArg(&call_args, unique_args);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinOut(builtinSymbol(LowLevelBuiltins.listOp(.list_append_le_bytes)), call_args.types.items, call_args.values.items);
    }

    fn emitListAppendSublist(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, unique_args: u64) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const abi = self.layouts().builtinListAbi(self.localLayout(target));
        if (abi.elem_size == 0) {
            const count = try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 3)).ptr, self.localLayout(GuardedList.at(args, 3))), self.ptrSizedIntType(), false);
            const len = try self.loadUsize(try self.offsetPtr(self.slot(GuardedList.at(args, 0)).ptr, self.rocListLenOffset()));
            const total_len = (self.wip orelse return error.CompilationFailed).bin(.add, len, count, "") catch return error.OutOfMemory;
            const null_ptr = builder.nullValue(try self.ptrType()) catch return error.OutOfMemory;
            try self.storePointer(self.slot(target).ptr, null_ptr);
            try self.storeListLen(self.slot(target).ptr, total_len);
            try self.storeListCapacity(self.slot(target).ptr, builder.intValue(self.ptrSizedIntType(), 0) catch return error.OutOfMemory);
            return;
        }
        var call_args = try self.rocListArgs1(GuardedList.at(args, 0));
        defer call_args.deinit(self.allocator);
        const src = try self.rocListArgs1(GuardedList.at(args, 1));
        defer {
            var owned = src;
            owned.deinit(self.allocator);
        }
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.types.appendSlice(self.allocator, src.types.items);
        try call_args.values.appendSlice(self.allocator, src.values.items);
        try call_args.append(self.allocator, .i64, try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 2)).ptr, self.localLayout(GuardedList.at(args, 2))), .i64, false));
        try call_args.append(self.allocator, .i64, try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 3)).ptr, self.localLayout(GuardedList.at(args, 3))), .i64, false));
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory);
        try call_args.append(self.allocator, self.ptrSizedIntType(), builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
        try self.appendListElementRcArgs(&call_args, abi, true, true);
        try self.appendUpdateModeArg(&call_args, unique_args);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinOut(builtinSymbol(LowLevelBuiltins.listOp(.list_append_sublist)), call_args.types.items, call_args.values.items);
    }

    /// Store the canonical value for a list of zero-width elements into
    /// `target`: such a list owns no allocation, so its data pointer is null
    /// and its stored capacity is zero, and it carries nothing but `len`.
    fn storeZstList(self: *MonoLlvmCodeGen, target: LocalId, len: LlvmBuilder.Value) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const out_ptr = self.slot(target).ptr;
        try self.storePointer(out_ptr, builder.nullValue(try self.ptrType()) catch return error.OutOfMemory);
        try self.storeListLen(out_ptr, len);
        try self.storeListCapacity(out_ptr, builder.intValue(self.ptrSizedIntType(), 0) catch return error.OutOfMemory);
    }

    fn emitListPrepend(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, unique_args: u64) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const list_local = GuardedList.at(args, 0);
        const abi = self.boxyAwareBuiltinListAbi(self.localLayout(list_local));
        // A zero-width element has no bytes to place at the front, so
        // prepending one only bumps the length.
        if (abi.elem_size == 0) {
            const len = try self.loadUsize(try self.offsetPtr(self.slot(list_local).ptr, self.rocListLenOffset()));
            const one = builder.intValue(self.ptrSizedIntType(), 1) catch return error.OutOfMemory;
            const grown = (self.wip orelse return error.CompilationFailed).bin(.add, len, one, "") catch return error.OutOfMemory;
            return self.storeZstList(target, grown);
        }
        var call_args = try self.rocListArgs1(list_local);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory);
        try call_args.append(self.allocator, try self.ptrType(), self.slot(GuardedList.at(args, 1)).ptr);
        try call_args.append(self.allocator, self.ptrSizedIntType(), builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
        const boxy_elem = self.boxyListElementDescForLocals(abi, &.{list_local}, target);
        if (boxy_elem) |elem| {
            try self.appendBoxyListElementDescArgs(&call_args, elem);
        } else {
            try self.appendListElementRcArgs(&call_args, abi, true, true);
        }
        try self.appendUpdateModeArg(&call_args, unique_args);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        if (boxy_elem != null) {
            try self.callBoxyVoid("roc_boxy_list_prepend", call_args.types.items, call_args.values.items);
        } else {
            try self.callBuiltinOut(builtinSymbol(LowLevelBuiltins.listOp(.list_prepend)), call_args.types.items, call_args.values.items);
        }
    }

    fn emitListSublist(self: *MonoLlvmCodeGen, target: LocalId, op: lir.LowLevel, args: anytype, unique_args: u64) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const list_local = GuardedList.at(args, 0);
        const abi = self.boxyAwareBuiltinListAbi(self.localLayout(list_local));
        const len = try self.loadUsize(try self.offsetPtr(self.slot(list_local).ptr, self.rocListLenOffset()));
        const zero = builder.intValue(self.ptrSizedIntType(), 0) catch return error.OutOfMemory;
        const one = builder.intValue(self.ptrSizedIntType(), 1) catch return error.OutOfMemory;
        const max_count = builder.intValue(self.ptrSizedIntType(), -1) catch return error.OutOfMemory;
        const slice = if (op == .list_drop_first)
            ListSlice{ .start = one, .len = max_count }
        else if (op == .list_drop_last) blk: {
            const len_is_zero = (self.wip orelse return error.CompilationFailed).icmp(.eq, len, zero, "") catch return error.OutOfMemory;
            const decremented = (self.wip orelse return error.CompilationFailed).bin(.sub, len, one, "") catch return error.OutOfMemory;
            const safe_len = (self.wip orelse return error.CompilationFailed).select(.normal, len_is_zero, zero, decremented, "") catch return error.OutOfMemory;
            break :blk ListSlice{ .start = zero, .len = safe_len };
        } else if (op == .list_take_first)
            ListSlice{ .start = zero, .len = try self.loadIntegerLocalAsUsize(GuardedList.at(args, 1)) }
        else if (op == .list_take_last) blk: {
            const count = try self.loadIntegerLocalAsUsize(GuardedList.at(args, 1));
            const takes_all = (self.wip orelse return error.CompilationFailed).icmp(.uge, count, len, "") catch return error.OutOfMemory;
            const suffix_start = (self.wip orelse return error.CompilationFailed).bin(.sub, len, count, "") catch return error.OutOfMemory;
            const safe_start = (self.wip orelse return error.CompilationFailed).select(.normal, takes_all, zero, suffix_start, "") catch return error.OutOfMemory;
            break :blk ListSlice{ .start = safe_start, .len = count };
        } else if (op == .list_sublist or op == .list_sublist_borrowed)
            try self.loadSublistStartLen(GuardedList.at(args, 1))
        else
            return error.UnsupportedLowLevel;

        // Zero-width elements have no bytes to slice, so the result is the
        // window length alone: min(len, size -| start).
        if (abi.elem_size == 0) {
            const wip = self.wip orelse return error.CompilationFailed;
            const past_end = wip.icmp(.uge, slice.start, len, "") catch return error.OutOfMemory;
            const remaining = wip.bin(.sub, len, slice.start, "") catch return error.OutOfMemory;
            const avail = wip.select(.normal, past_end, zero, remaining, "") catch return error.OutOfMemory;
            const wants_more = wip.icmp(.ugt, slice.len, avail, "") catch return error.OutOfMemory;
            const kept = wip.select(.normal, wants_more, avail, slice.len, "") catch return error.OutOfMemory;
            return self.storeZstList(target, kept);
        }

        var call_args = try self.rocListArgs1(GuardedList.at(args, 0));
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        if (op == .list_sublist_borrowed) {
            try call_args.append(self.allocator, self.ptrSizedIntType(), builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
            try call_args.append(self.allocator, .i64, try self.coerceScalar(slice.start, .i64, false));
            try call_args.append(self.allocator, .i64, try self.coerceScalar(slice.len, .i64, false));
            try call_args.append(self.allocator, .i1, builder.intValue(.i1, @intFromBool(abi.contains_refcounted)) catch return error.OutOfMemory);
            try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
            try self.callBuiltinVoid(LowLevelBuiltins.listOp(op).symbolName(), call_args.types.items, call_args.values.items);
        } else {
            try call_args.append(self.allocator, .i32, builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory);
            try call_args.append(self.allocator, self.ptrSizedIntType(), builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
            try call_args.append(self.allocator, .i64, try self.coerceScalar(slice.start, .i64, false));
            try call_args.append(self.allocator, .i64, try self.coerceScalar(slice.len, .i64, false));
            const boxy_elem = self.boxyListElementDescForLocals(abi, &.{list_local}, target);
            if (boxy_elem) |elem| {
                try self.appendBoxyListElementDescArgs(&call_args, elem);
            } else {
                try self.appendListElementRcArgs(&call_args, abi, false, true);
            }
            try self.appendUpdateModeArg(&call_args, unique_args);
            try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
            if (boxy_elem != null) {
                try self.callBoxyVoid("roc_boxy_list_sublist", call_args.types.items, call_args.values.items);
            } else {
                try self.callBuiltinVoid(LowLevelBuiltins.listOp(op).symbolName(), call_args.types.items, call_args.values.items);
            }
        }
    }

    const ListSlice = struct {
        start: LlvmBuilder.Value,
        len: LlvmBuilder.Value,
    };

    fn loadSublistStartLen(self: *MonoLlvmCodeGen, record: LocalId) Error!ListSlice {
        const record_layout = self.localLayout(record);
        const record_layout_val = self.layoutValue(record_layout);
        if (record_layout_val.tag != .struct_) return error.CompilationFailed;
        const record_idx = record_layout_val.getStruct().idx;
        const len_offset = self.layouts().getStructFieldOffsetByOriginalIndex(record_idx, 0);
        const start_offset = self.layouts().getStructFieldOffsetByOriginalIndex(record_idx, 1);
        const len_layout = self.layouts().getStructFieldLayoutByOriginalIndex(record_idx, 0);
        const start_layout = self.layouts().getStructFieldLayoutByOriginalIndex(record_idx, 1);
        return .{
            .start = try self.loadIntegerPtrAsUsize(try self.offsetPtr(self.slot(record).ptr, start_offset), start_layout),
            .len = try self.loadIntegerPtrAsUsize(try self.offsetPtr(self.slot(record).ptr, len_offset), len_layout),
        };
    }

    fn loadIntegerLocalAsUsize(self: *MonoLlvmCodeGen, local: LocalId) Error!LlvmBuilder.Value {
        return self.loadIntegerPtrAsUsize(self.slot(local).ptr, self.localLayout(local));
    }

    fn loadIntegerPtrAsUsize(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, value_layout: layout.Idx) Error!LlvmBuilder.Value {
        switch (value_layout) {
            .dec => {
                const value = try self.loadScalar(ptr, .dec);
                const parts = try self.splitI128Value(value);
                const truncated = try self.callBuiltin(builtinSymbol(.dec_to_i64_trunc), .i64, &.{ .i64, .i64 }, &.{ parts.low, parts.high });
                return self.coerceScalar(truncated, self.ptrSizedIntType(), true);
            },
            .f32, .f64 => return error.CompilationFailed,
            .bool, .str, .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .opaque_ptr, .zst, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2, _ => {
                const value = try self.loadScalar(ptr, value_layout);
                return self.coerceScalar(value, self.ptrSizedIntType(), value_layout.isSigned());
            },
        }
    }

    fn emitListDropAt(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, unique_args: u64) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const list_local = GuardedList.at(args, 0);
        const abi = self.boxyAwareBuiltinListAbi(self.localLayout(list_local));
        // Zero-width elements have no bytes to move, so dropping one only
        // shortens the length, and only when the index is in bounds.
        if (abi.elem_size == 0) {
            const wip = self.wip orelse return error.CompilationFailed;
            const len = try self.loadUsize(try self.offsetPtr(self.slot(list_local).ptr, self.rocListLenOffset()));
            const index = try self.loadIntegerLocalAsUsize(GuardedList.at(args, 1));
            const one = builder.intValue(self.ptrSizedIntType(), 1) catch return error.OutOfMemory;
            const out_of_bounds = wip.icmp(.uge, index, len, "") catch return error.OutOfMemory;
            const shortened = wip.bin(.sub, len, one, "") catch return error.OutOfMemory;
            const kept = wip.select(.normal, out_of_bounds, len, shortened, "") catch return error.OutOfMemory;
            return self.storeZstList(target, kept);
        }
        var call_args = try self.rocListArgs1(list_local);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory);
        try call_args.append(self.allocator, self.ptrSizedIntType(), builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
        try call_args.append(self.allocator, .i64, try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1))), .i64, false));
        const boxy_elem = self.boxyListElementDescForLocals(abi, &.{list_local}, target);
        if (boxy_elem) |elem| {
            try self.appendBoxyListElementDescArgs(&call_args, elem);
        } else {
            try self.appendListElementRcArgs(&call_args, abi, true, true);
        }
        try self.appendUpdateModeArg(&call_args, unique_args);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        if (boxy_elem != null) {
            try self.callBoxyVoid("roc_boxy_list_drop_at", call_args.types.items, call_args.values.items);
        } else {
            try self.callBuiltinOut(builtinSymbol(LowLevelBuiltins.listOp(.list_drop_at)), call_args.types.items, call_args.values.items);
        }
    }

    fn emitListSwap(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, unique_args: u64) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const list_local = GuardedList.at(args, 0);
        const abi = self.boxyAwareBuiltinListAbi(self.localLayout(list_local));
        var call_args = try self.rocListArgs1(list_local);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory);
        try call_args.append(self.allocator, self.ptrSizedIntType(), builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
        try call_args.append(self.allocator, .i64, try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1))), .i64, false));
        try call_args.append(self.allocator, .i64, try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 2)).ptr, self.localLayout(GuardedList.at(args, 2))), .i64, false));
        const boxy_elem = self.boxyListElementDescForLocals(abi, &.{list_local}, target);
        if (boxy_elem) |elem| {
            try self.appendBoxyListElementDescArgs(&call_args, elem);
        } else {
            try self.appendListElementRcArgs(&call_args, abi, true, true);
        }
        try self.appendUpdateModeArg(&call_args, unique_args);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        if (boxy_elem != null) {
            try self.callBoxyVoid("roc_boxy_list_swap", call_args.types.items, call_args.values.items);
        } else {
            try self.callBuiltinOut(builtinSymbol(LowLevelBuiltins.listOp(.list_swap)), call_args.types.items, call_args.values.items);
        }
    }

    fn emitListSet(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, unique_args: u64) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const list_local = GuardedList.at(args, 0);
        const abi = self.boxyAwareBuiltinListAbi(self.localLayout(list_local));
        if (abi.elem_size == 0) {
            try self.copyBytes(self.slot(target).ptr, self.slot(list_local).ptr, self.slot(target).size, self.slot(target).alignment);
            return;
        }

        var call_args = try self.rocListArgs1(list_local);
        defer call_args.deinit(self.allocator);

        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory);
        try call_args.append(self.allocator, .i64, try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1))), .i64, false));
        try call_args.append(self.allocator, try self.ptrType(), self.slot(GuardedList.at(args, 2)).ptr);
        try call_args.append(self.allocator, self.ptrSizedIntType(), builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
        const boxy_elem = self.boxyListElementDescForLocals(abi, &.{list_local}, target);
        if (boxy_elem) |elem| {
            try self.appendBoxyListElementDescArgs(&call_args, elem);
        } else {
            // `listSet` decrefs the element it displaces before overwriting it,
            // so unlike `list_replace` it has no out_element parameter and the
            // caller owes no drop afterwards.
            try self.appendListElementRcArgs(&call_args, abi, true, true);
        }
        try self.appendUpdateModeArg(&call_args, unique_args);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        if (boxy_elem != null) {
            try self.callBoxyVoid("roc_boxy_list_set", call_args.types.items, call_args.values.items);
        } else {
            try self.callBuiltinOut(builtinSymbol(LowLevelBuiltins.listOp(.list_set)), call_args.types.items, call_args.values.items);
        }
    }

    fn emitListReplaceUnsafe(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, unique_args: u64) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const list_local = GuardedList.at(args, 0);
        // The result is a { list, prev } record. Reuse roc_builtins_list_replace
        // and aim its (out_list, out_element) outputs directly at the record's
        // fields, disambiguated by layout tag like the dev backend does.
        const record_layout_val = self.layoutValue(self.localLayout(target));
        if (record_layout_val.tag != .struct_) return error.CompilationFailed;
        const rec_idx = record_layout_val.getStruct().idx;
        const f0_layout = self.layoutValue(self.layouts().getStructFieldLayoutByOriginalIndex(rec_idx, 0));
        const f0_offset = self.layouts().getStructFieldOffsetByOriginalIndex(rec_idx, 0);
        const f1_offset = self.layouts().getStructFieldOffsetByOriginalIndex(rec_idx, 1);
        const f0_is_list = f0_layout.tag == .list or f0_layout.tag == .list_of_zst;
        const list_out_ptr = try self.offsetPtr(self.slot(target).ptr, if (f0_is_list) f0_offset else f1_offset);
        const value_out_ptr = try self.offsetPtr(self.slot(target).ptr, if (f0_is_list) f1_offset else f0_offset);

        const abi = self.boxyAwareBuiltinListAbi(self.localLayout(list_local));
        if (abi.elem_size == 0) {
            // listReplace would dereference a NULL element pointer for ZST
            // elements; the result list is the input unchanged and the prev
            // field is zero-sized.
            try self.copyBytes(list_out_ptr, self.slot(list_local).ptr, self.slot(list_local).size, self.slot(list_local).alignment);
            return;
        }

        var call_args = try self.rocListArgs1(list_local);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), list_out_ptr);
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory);
        try call_args.append(self.allocator, .i64, try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1))), .i64, false));
        try call_args.append(self.allocator, try self.ptrType(), self.slot(GuardedList.at(args, 2)).ptr);
        try call_args.append(self.allocator, self.ptrSizedIntType(), builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
        try call_args.append(self.allocator, try self.ptrType(), value_out_ptr);
        const boxy_elem = self.boxyListElementDescForLocals(abi, &.{list_local}, null);
        if (boxy_elem) |elem| {
            try self.appendBoxyListElementDescArgs(&call_args, elem);
        } else {
            try self.appendListElementRcArgs(&call_args, abi, true, true);
        }
        try self.appendUpdateModeArg(&call_args, unique_args);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        if (boxy_elem != null) {
            try self.callBoxyVoid("roc_boxy_list_replace", call_args.types.items, call_args.values.items);
        } else {
            try self.callBuiltinOut(builtinSymbol(LowLevelBuiltins.listOp(.list_replace_unsafe)), call_args.types.items, call_args.values.items);
        }
    }

    fn emitListSlackUnique(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        var call_args = try self.rocListArgs1(GuardedList.at(args, 0));
        defer call_args.deinit(self.allocator);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        const slack = try self.callBuiltin(builtinSymbol(LowLevelBuiltins.listOp(.list_slack_unique)), .i64, call_args.types.items, call_args.values.items);
        try self.storeIntToLayout(self.slot(target).ptr, slack, self.localLayout(target));
    }

    fn emitListOwnedUnique(self: *MonoLlvmCodeGen, target: LocalId, args: anytype) Error!void {
        var call_args = try self.rocListArgs1(GuardedList.at(args, 0));
        defer call_args.deinit(self.allocator);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        const owned = try self.callBuiltin(builtinSymbol(LowLevelBuiltins.listOp(.list_owned_unique)), .i64, call_args.types.items, call_args.values.items);
        try self.storeIntToLayout(self.slot(target).ptr, owned, self.localLayout(target));
    }

    fn emitListReserve(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, unique_args: u64) Error!void {
        const abi = self.layouts().builtinListAbi(self.localLayout(GuardedList.at(args, 0)));
        // A list of zero-sized elements owns no allocation, so it carries a
        // length and nothing else and a reserve cannot change anything
        // observable. Passing it to the builtin instead goes through capacity
        // bookkeeping that a zero-width element makes degenerate, which loses
        // the length, so copy the list across here.
        if (abi.elem_size == 0) {
            const src_ptr = self.slot(GuardedList.at(args, 0)).ptr;
            const out_ptr = self.slot(target).ptr;
            try self.storePointer(out_ptr, try self.loadPointer(src_ptr));
            try self.storeListLen(out_ptr, try self.loadUsize(try self.offsetPtr(src_ptr, self.rocListLenOffset())));
            try self.storeListCapacity(out_ptr, try self.loadUsize(try self.offsetPtr(src_ptr, self.rocListCapacityOffset())));
            return;
        }

        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const usize_ty = self.ptrSizedIntType();

        // The no-growth outcome is the hot one, so its checks are emitted
        // inline where this backend controls their shape; growth and shared
        // lists fall through to the builtin. This mirrors listReserve's fast
        // path exactly: exclusive ownership and spare <= capacity - length,
        // where a seamless slice's capacity is its visible window length.
        const list_ptr = self.slot(GuardedList.at(args, 0)).ptr;
        const bytes = try self.loadPointer(list_ptr);
        const len = try self.loadUsize(try self.offsetPtr(list_ptr, self.rocListLenOffset()));
        const cap_word = try self.loadUsize(try self.offsetPtr(list_ptr, self.rocListCapacityOffset()));
        // The spare count is a u64 regardless of target width; compare in 64
        // bits so a huge request on a 32-bit target is not truncated into a
        // satisfiable one.
        const spare = try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1))), .i64, false);

        const zero = builder.intValue(usize_ty, 0) catch return error.OutOfMemory;
        const one = builder.intValue(usize_ty, 1) catch return error.OutOfMemory;
        const slice_tag = wip.bin(.@"and", cap_word, one, "") catch return error.OutOfMemory;
        const is_slice = wip.icmp(.ne, slice_tag, zero, "") catch return error.OutOfMemory;
        const decoded_cap = wip.bin(.lshr, cap_word, one, "") catch return error.OutOfMemory;
        const capacity = wip.select(.normal, is_slice, len, decoded_cap, "") catch return error.OutOfMemory;
        const slack = wip.bin(.sub, capacity, len, "") catch return error.OutOfMemory;
        const slack_wide = try self.coerceScalar(slack, .i64, false);
        const slack_ok = wip.icmp(.ule, spare, slack_wide, "") catch return error.OutOfMemory;

        const fast_block = wip.block(0, "list_reserve_fast") catch return error.OutOfMemory;
        const slow_block = wip.block(0, "list_reserve_grow") catch return error.OutOfMemory;
        const merge_block = wip.block(0, "list_reserve_done") catch return error.OutOfMemory;

        if ((unique_args & 1) != 0) {
            // Statically in place: ownership needs no runtime evidence.
            _ = wip.brCond(slack_ok, fast_block, slow_block, .then_likely) catch return error.OutOfMemory;
        } else {
            const check_rc_block = wip.block(0, "list_reserve_check_rc") catch return error.OutOfMemory;
            _ = wip.brCond(slack_ok, check_rc_block, slow_block, .then_likely) catch return error.OutOfMemory;

            // Unique when the list owns no allocation at all (capacity zero
            // and not a slice), or the allocation's refcount is one. A slice
            // stores its backing allocation's data pointer in the capacity
            // word with the tag bit set, so subtracting the tag recovers it.
            wip.cursor = .{ .block = check_rc_block };
            const cap_zero = wip.icmp(.eq, capacity, zero, "") catch return error.OutOfMemory;
            const not_slice = wip.icmp(.eq, slice_tag, zero, "") catch return error.OutOfMemory;
            const unallocated = wip.bin(.@"and", cap_zero, not_slice, "") catch return error.OutOfMemory;
            const load_rc_block = wip.block(0, "list_reserve_load_rc") catch return error.OutOfMemory;
            _ = wip.brCond(unallocated, fast_block, load_rc_block, .none) catch return error.OutOfMemory;

            wip.cursor = .{ .block = load_rc_block };
            const untagged_alloc = wip.bin(.sub, cap_word, slice_tag, "") catch return error.OutOfMemory;
            const bytes_int = wip.cast(.ptrtoint, bytes, usize_ty, "") catch return error.OutOfMemory;
            const alloc_int = wip.select(.normal, is_slice, untagged_alloc, bytes_int, "") catch return error.OutOfMemory;
            const alloc_ptr = wip.cast(.inttoptr, alloc_int, try self.ptrType(), "") catch return error.OutOfMemory;
            const word_bytes: i64 = @intCast(self.rocListLenOffset());
            const rc_ptr = try self.offsetPtrValue(alloc_ptr, builder.intValue(usize_ty, -word_bytes) catch return error.OutOfMemory);
            const rc = try self.loadUsize(rc_ptr);
            const rc_is_one = wip.icmp(.eq, rc, one, "") catch return error.OutOfMemory;
            _ = wip.brCond(rc_is_one, fast_block, slow_block, .then_likely) catch return error.OutOfMemory;
        }

        wip.cursor = .{ .block = fast_block };
        const out_ptr = self.slot(target).ptr;
        try self.storePointer(out_ptr, bytes);
        try self.storeUsize(try self.offsetPtr(out_ptr, self.rocListLenOffset()), len);
        try self.storeUsize(try self.offsetPtr(out_ptr, self.rocListCapacityOffset()), cap_word);
        _ = wip.br(merge_block) catch return error.OutOfMemory;

        wip.cursor = .{ .block = slow_block };
        try self.emitListReserveCall(target, args, unique_args);
        _ = wip.br(merge_block) catch return error.OutOfMemory;

        wip.cursor = .{ .block = merge_block };
    }

    fn emitListReserveCall(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, unique_args: u64) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const list_local = GuardedList.at(args, 0);
        const abi = self.boxyAwareBuiltinListAbi(self.localLayout(list_local));
        var call_args = try self.rocListArgs1(list_local);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory);
        try call_args.append(self.allocator, .i64, try self.coerceScalar(try self.loadScalar(self.slot(GuardedList.at(args, 1)).ptr, self.localLayout(GuardedList.at(args, 1))), .i64, false));
        try call_args.append(self.allocator, self.ptrSizedIntType(), builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
        const boxy_elem = self.boxyListElementDescForLocals(abi, &.{list_local}, target);
        if (boxy_elem) |elem| {
            try self.appendBoxyListElementDescArgs(&call_args, elem);
        } else {
            try self.appendListElementRcArgs(&call_args, abi, true, true);
        }
        try self.appendUpdateModeArg(&call_args, unique_args);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        if (boxy_elem != null) {
            try self.callBoxyVoid("roc_boxy_list_reserve", call_args.types.items, call_args.values.items);
        } else {
            try self.callBuiltinOut(builtinSymbol(LowLevelBuiltins.listOp(.list_reserve)), call_args.types.items, call_args.values.items);
        }
    }

    fn emitListReleaseExcess(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, unique_args: u64) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const list_local = GuardedList.at(args, 0);
        const abi = self.boxyAwareBuiltinListAbi(self.localLayout(list_local));
        // A zero-width list holds no allocation, so it has no excess to release
        // and passes through unchanged.
        if (abi.elem_size == 0) {
            const len = try self.loadUsize(try self.offsetPtr(self.slot(list_local).ptr, self.rocListLenOffset()));
            return self.storeZstList(target, len);
        }
        var call_args = try self.rocListArgs1(list_local);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory);
        try call_args.append(self.allocator, self.ptrSizedIntType(), builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
        const boxy_elem = self.boxyListElementDescForLocals(abi, &.{list_local}, target);
        if (boxy_elem) |elem| {
            try self.appendBoxyListElementDescArgs(&call_args, elem);
        } else {
            try self.appendListElementRcArgs(&call_args, abi, true, true);
        }
        try self.appendUpdateModeArg(&call_args, unique_args);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        if (boxy_elem != null) {
            try self.callBoxyVoid("roc_boxy_list_release_excess_capacity", call_args.types.items, call_args.values.items);
        } else {
            try self.callBuiltinOut(builtinSymbol(LowLevelBuiltins.listOp(.list_release_excess_capacity)), call_args.types.items, call_args.values.items);
        }
    }

    fn emitListReverse(self: *MonoLlvmCodeGen, target: LocalId, args: anytype, unique_args: u64) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const list_local = GuardedList.at(args, 0);
        const abi = self.boxyAwareBuiltinListAbi(self.localLayout(list_local));
        if (abi.elem_size == 0) {
            try self.copyBytes(self.slot(target).ptr, self.slot(list_local).ptr, self.slot(target).size, self.slot(target).alignment);
            return;
        }

        var call_args = try self.rocListArgs1(list_local);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory);
        try call_args.append(self.allocator, self.ptrSizedIntType(), builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
        const boxy_elem = self.boxyListElementDescForLocals(abi, &.{list_local}, target);
        if (boxy_elem) |elem| {
            try self.appendBoxyListElementDescArgs(&call_args, elem);
        } else {
            try self.appendListElementRcArgs(&call_args, abi, true, true);
        }
        try self.appendUpdateModeArg(&call_args, unique_args);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        if (boxy_elem != null) {
            try self.callBoxyVoid("roc_boxy_list_reverse", call_args.types.items, call_args.values.items);
        } else {
            try self.callBuiltinOut(builtinSymbol(LowLevelBuiltins.listOp(.list_reverse)), call_args.types.items, call_args.values.items);
        }
    }

    fn emitListFirstLast(self: *MonoLlvmCodeGen, target: LocalId, op: lir.LowLevel, args: anytype) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const list_ptr = self.slot(GuardedList.at(args, 0)).ptr;
        const len = try self.loadUsize(try self.offsetPtr(list_ptr, self.rocListLenOffset()));
        const non_empty = wip.icmp(.ne, len, builder.intValue(self.ptrSizedIntType(), 0) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const empty_block = wip.block(0, "list_empty") catch return error.OutOfMemory;
        const full_block = wip.block(0, "list_full") catch return error.OutOfMemory;
        const after = wip.block(0, "list_first_after") catch return error.OutOfMemory;
        _ = wip.brCond(non_empty, full_block, empty_block, .then_likely) catch return error.OutOfMemory;
        wip.cursor = .{ .block = empty_block };
        try self.emitTagLiteral(target, 0, null);
        _ = wip.br(after) catch return error.OutOfMemory;
        wip.cursor = .{ .block = full_block };
        const abi = self.boxyAwareBuiltinListAbi(self.localLayout(GuardedList.at(args, 0)));
        const bytes = try self.loadPointer(list_ptr);
        const idx = if (op == .list_first)
            builder.intValue(self.ptrSizedIntType(), 0) catch return error.OutOfMemory
        else
            wip.bin(.sub, len, builder.intValue(self.ptrSizedIntType(), 1) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const offset = wip.bin(.mul, idx, builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const elem_src = wip.gep(.inbounds, .i8, bytes, &.{offset}, "") catch return error.OutOfMemory;
        const payload_layout = self.tagPayloadLayout(self.localLayout(target), 1);
        try self.emitTagLiteral(target, 1, null);
        try self.copyBytes(self.slot(target).ptr, elem_src, self.layoutByteSize(payload_layout), self.alignmentForLayout(payload_layout));
        _ = wip.br(after) catch return error.OutOfMemory;
        wip.cursor = .{ .block = after };
    }

    fn emitBoxBox(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const allocated = try self.allocAggregateTarget(target);
        try self.copyBytes(allocated.ptr, self.slot(arg).ptr, self.slot(arg).size, self.slot(arg).alignment);
    }

    fn emitBoxUnbox(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const ptr = try self.loadPointer(self.slot(arg).ptr);
        if (self.slot(target).size > 0) try self.copyBytes(self.slot(target).ptr, ptr, self.slot(target).size, self.slot(target).alignment);
    }

    fn emitBoxPrepareUpdate(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId, unique_args: u64) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const ptr_ty = try self.ptrType();
        const target_layout = self.localLayout(target);
        switch (self.layoutValue(target_layout).tag) {
            .box_of_zst => {
                try self.storePointer(self.slot(target).ptr, builder.nullValue(ptr_ty) catch return error.OutOfMemory);
            },
            .box => {
                const abi = self.layouts().builtinBoxAbi(target_layout);
                const enabled = abi.contains_refcounted and abi.elem_layout_idx != null;
                const null_ptr = builder.nullValue(ptr_ty) catch return error.OutOfMemory;
                const payload_incref = if (enabled)
                    (try self.declareRcHelper(.{ .op = .incref, .layout_idx = abi.elem_layout_idx.? }, .atomic))
                else
                    null;
                const payload_decref = if (enabled)
                    (try self.declareRcHelper(.{ .op = .decref, .layout_idx = abi.elem_layout_idx.? }, .atomic))
                else
                    null;
                const mode = if ((unique_args & 1) != 0) builtins.utils.UpdateMode.InPlace else builtins.utils.UpdateMode.Immutable;
                const result = try self.callBuiltin(
                    builtinSymbol(.box_prepare_update),
                    ptr_ty,
                    &.{ ptr_ty, self.ptrSizedIntType(), .i32, .i1, ptr_ty, ptr_ty, .i8, ptr_ty },
                    &.{
                        try self.loadPointer(self.slot(arg).ptr),
                        builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory,
                        builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory,
                        builder.intValue(.i1, @intFromBool(enabled)) catch return error.OutOfMemory,
                        if (payload_incref) |func| func.toValue(builder) else null_ptr,
                        if (payload_decref) |func| func.toValue(builder) else null_ptr,
                        builder.intValue(.i8, @intFromEnum(mode)) catch return error.OutOfMemory,
                        self.rocOps(),
                    },
                );
                try self.storePointer(self.slot(target).ptr, result);
            },
            .scalar, .erased_box, .list, .list_of_zst, .struct_, .closure, .erased_callable, .zst, .tag_union, .ptr => return error.CompilationFailed,
        }
    }

    fn emitErasedCaptureLoad(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const capture_ptr = try self.loadPointer(self.slot(arg).ptr);
        if (self.slot(target).size > 0) try self.copyBytes(self.slot(target).ptr, capture_ptr, self.slot(target).size, self.slot(target).alignment);
    }

    /// ptr_alloca: () -> Ptr(T). Reserve a zeroed slot for T and store its
    /// address into the target. TRMC emits this once per proc entry (pre-loop),
    /// and allocEntryBlockSlot keeps the physical slot in the entry frame.
    fn emitPtrAlloca(self: *MonoLlvmCodeGen, target: LocalId) Error!void {
        const elem_idx = self.layoutValue(self.localLayout(target)).getIdx();
        const sa = self.sizeAlignOf(elem_idx);
        const slot_ptr = try self.allocEntryBlockSlot(.i8, @max(sa.size, 1), self.llvmAlignment(sa.alignment), "trmc_slot");
        if (sa.size > 0) try self.zeroBytes(slot_ptr, sa.size);
        try self.storePointer(self.slot(target).ptr, slot_ptr);
    }

    /// box_alloc_zeroed: () -> Box(T). allocAggregateTarget's box branch is
    /// exactly this op: allocate_with_refcount + zero payload + store the data
    /// pointer into the target slot.
    fn emitBoxAllocZeroed(self: *MonoLlvmCodeGen, target: LocalId) Error!void {
        _ = try self.allocAggregateTarget(target);
    }

    /// ptr_store: (Ptr(T), T) -> {}. Copy sizeOf(T) bytes into *ptr.
    fn emitPtrStore(self: *MonoLlvmCodeGen, ptr_arg: LocalId, value_arg: LocalId) Error!void {
        const value_size = self.slot(value_arg).size;
        if (value_size == 0) return;
        const dst = try self.loadPointer(self.slot(ptr_arg).ptr);
        try self.copyBytes(dst, self.slot(value_arg).ptr, value_size, self.slot(value_arg).alignment);
    }

    /// ptr_load: (Ptr(T)) -> T. Same load-through-pointer as emitBoxUnbox.
    fn emitPtrLoad(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const src = try self.loadPointer(self.slot(arg).ptr);
        if (self.slot(target).size > 0) try self.copyBytes(self.slot(target).ptr, src, self.slot(target).size, self.slot(target).alignment);
    }

    /// ptr_cast: identity bits (box(T) -> ptr(T) or ptr -> ptr).
    fn emitPtrCast(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const ptr_value = try self.loadPointer(self.slot(arg).ptr);
        try self.storePointer(self.slot(target).ptr, ptr_value);
    }

    /// Heap-backed glue carried across the per-field children of one struct
    /// equality. `acc` is the running AND of the field comparisons; `field_out`
    /// receives each child's result before it is folded into `acc`.
    const StructEqState = struct {
        lhs_ptr: LlvmBuilder.Value,
        rhs_ptr: LlvmBuilder.Value,
        layout_idx: layout.Idx,
        field_count: usize,
        index: usize,
        acc: LlvmBuilder.Value,
        field_out: LlvmBuilder.Value,
        out: *LlvmBuilder.Value,
    };

    /// Heap-backed glue carried across the element child of one list equality.
    /// The loop scaffolding is already emitted and the cursor sits in the body
    /// block; `elem_out` receives the element comparison before the loop tail
    /// and the post-loop result load are emitted.
    const ListEqState = struct {
        result_ptr: LlvmBuilder.Value,
        idx_ptr: LlvmBuilder.Value,
        idx: LlvmBuilder.Value,
        header: LlvmBuilder.Function.Block.Index,
        after: LlvmBuilder.Function.Block.Index,
        elem_out: LlvmBuilder.Value,
        out: *LlvmBuilder.Value,
    };

    /// Heap-backed glue carried across the payload children of one tag-union
    /// equality. One frame per variant case block emits the discriminant guard,
    /// queues the payload comparison, stores it, and branches to `after`.
    const TagEqState = struct {
        lhs_ptr: LlvmBuilder.Value,
        rhs_ptr: LlvmBuilder.Value,
        layout_idx: layout.Idx,
        lhs_disc: LlvmBuilder.Value,
        result_ptr: LlvmBuilder.Value,
        after: LlvmBuilder.Function.Block.Index,
        case_blocks: []LlvmBuilder.Function.Block.Index,
        index: usize,
        payload_out: LlvmBuilder.Value,
        out: *LlvmBuilder.Value,
    };

    /// Work item for the explicit equality-emission stack. `.eval` computes the
    /// structural equality of a value at `lhs_ptr`/`rhs_ptr` of layout
    /// `layout_idx`, writing the resulting `i1` into `out`. The remaining
    /// variants reproduce the post-children glue that recursion previously
    /// interleaved for struct fields, list elements, and tag-union payloads.
    const EqWork = union(enum) {
        eval: struct {
            lhs_ptr: LlvmBuilder.Value,
            rhs_ptr: LlvmBuilder.Value,
            layout_idx: layout.Idx,
            out: *LlvmBuilder.Value,
        },
        struct_step: *StructEqState,
        struct_combine: *StructEqState,
        list_finish: *ListEqState,
        tag_case: *TagEqState,
        tag_case_after: *TagEqState,
    };

    /// Drives structural equality emission with an explicit heap-backed work
    /// stack so deeply nested layouts cannot overflow the native stack. The
    /// emission order matches the former recursion exactly: continuations are
    /// pushed before their child so the child's whole subtree is emitted first,
    /// and each value-returning child writes into a stable heap result slot the
    /// parent reads in its continuation.
    fn emitValueEqual(self: *MonoLlvmCodeGen, lhs_ptr: LlvmBuilder.Value, rhs_ptr: LlvmBuilder.Value, layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        var result: LlvmBuilder.Value = undefined;
        var sfa = std.heap.stackFallback(64 * @sizeOf(EqWork), self.allocator);
        const wa = sfa.get();
        var work = std.ArrayList(EqWork).empty;
        defer work.deinit(wa);
        try work.append(wa, .{ .eval = .{ .lhs_ptr = lhs_ptr, .rhs_ptr = rhs_ptr, .layout_idx = layout_idx, .out = &result } });
        while (work.pop()) |item| {
            switch (item) {
                .eval => |e| try self.emitValueEqualNode(e.lhs_ptr, e.rhs_ptr, e.layout_idx, e.out, wa, &work),
                .struct_step => |state| {
                    if (state.index == state.field_count) {
                        state.out.* = state.acc;
                        self.allocator.destroy(state);
                    } else {
                        const layout_val = self.layoutValue(state.layout_idx);
                        const info = self.layouts().getStructInfo(layout_val);
                        const field = info.fields.get(@intCast(state.index));
                        if (field.is_padding) {
                            // Padding spacers hold uninitialized bytes; never compare them.
                            state.index += 1;
                            try work.append(wa, .{ .struct_step = state });
                        } else {
                            const offset = self.layouts().getStructFieldOffset(layout_val.getStruct().idx, @intCast(state.index));
                            try work.append(wa, .{ .struct_combine = state });
                            try work.append(wa, .{ .eval = .{
                                .lhs_ptr = try self.offsetPtr(state.lhs_ptr, offset),
                                .rhs_ptr = try self.offsetPtr(state.rhs_ptr, offset),
                                .layout_idx = field.layout,
                                .out = &state.field_out,
                            } });
                        }
                    }
                },
                .struct_combine => |state| {
                    const wip = self.wip orelse return error.CompilationFailed;
                    state.acc = wip.bin(.@"and", state.acc, state.field_out, "") catch return error.CompilationFailed;
                    state.index += 1;
                    try work.append(wa, .{ .struct_step = state });
                },
                .list_finish => |state| try self.emitListEqualFinish(state),
                .tag_case => |state| try self.emitTagEqualCase(state, wa, &work),
                .tag_case_after => |state| try self.emitTagEqualCaseAfter(state, wa, &work),
            }
        }
        return result;
    }

    /// Handles one `.eval` work item: emits the leaf comparison directly, or, for
    /// composite layouts, emits the per-layout scaffolding and queues child
    /// comparisons plus the continuation that consumes them.
    fn emitValueEqualNode(
        self: *MonoLlvmCodeGen,
        lhs_ptr: LlvmBuilder.Value,
        rhs_ptr: LlvmBuilder.Value,
        layout_idx: layout.Idx,
        out: *LlvmBuilder.Value,
        wa: Allocator,
        work: *std.ArrayList(EqWork),
    ) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const layout_val = self.layoutValue(layout_idx);
        if (self.layoutByteSize(layout_idx) == 0) {
            out.* = builder.intValue(.i1, 1) catch return error.OutOfMemory;
            return;
        }
        switch (layout_val.tag) {
            .scalar => switch (layout_val.getScalar().tag) {
                .str => {
                    const lhs_fields = try self.rocStrArgFields(lhs_ptr);
                    const rhs_fields = try self.rocStrArgFields(rhs_ptr);
                    out.* = try self.callBuiltin(builtinSymbol(LowLevelBuiltins.strOp(.str_is_eq)), .i1, &.{ try self.ptrType(), self.ptrSizedIntType(), self.ptrSizedIntType(), try self.ptrType(), self.ptrSizedIntType(), self.ptrSizedIntType() }, &.{ lhs_fields[0], lhs_fields[1], lhs_fields[2], rhs_fields[0], rhs_fields[1], rhs_fields[2] });
                },
                .int, .frac, .opaque_ptr, .vector => {
                    const lhs = try self.loadScalar(lhs_ptr, layout_idx);
                    const rhs = try self.loadScalar(rhs_ptr, layout_idx);
                    out.* = if (isFloatLayout(layout_idx))
                        wip.fcmp(.normal, .oeq, lhs, rhs, "") catch return error.OutOfMemory
                    else
                        wip.icmp(.eq, lhs, rhs, "") catch return error.OutOfMemory;
                },
            },
            .box, .erased_callable => {
                const lhs = try self.loadPointer(lhs_ptr);
                const rhs = try self.loadPointer(rhs_ptr);
                out.* = wip.icmp(.eq, lhs, rhs, "") catch return error.OutOfMemory;
            },
            .erased_box => return error.CompilationFailed,
            .list, .list_of_zst => try self.emitListEqual(lhs_ptr, rhs_ptr, layout_idx, out, wa, work),
            .struct_ => {
                const info = self.layouts().getStructInfo(layout_val);
                const state = try self.allocator.create(StructEqState);
                state.* = .{
                    .lhs_ptr = lhs_ptr,
                    .rhs_ptr = rhs_ptr,
                    .layout_idx = layout_idx,
                    .field_count = info.fields.len,
                    .index = 0,
                    .acc = builder.intValue(.i1, 1) catch return error.OutOfMemory,
                    .field_out = undefined,
                    .out = out,
                };
                try work.append(wa, .{ .struct_step = state });
            },
            .tag_union => try self.emitTagEqual(lhs_ptr, rhs_ptr, layout_idx, out, wa, work),
            .box_of_zst, .closure, .zst, .ptr => out.* = try self.emitMemoryEqual(lhs_ptr, rhs_ptr, self.layoutByteSize(layout_idx)),
        }
    }

    fn emitMemoryEqual(self: *MonoLlvmCodeGen, lhs_ptr: LlvmBuilder.Value, rhs_ptr: LlvmBuilder.Value, size: u32) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        var result = builder.intValue(.i1, 1) catch return error.OutOfMemory;
        var offset: u32 = 0;
        while (offset < size) : (offset += 1) {
            const lhs = wip.load(.normal, .i8, try self.offsetPtr(lhs_ptr, offset), LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
            const rhs = wip.load(.normal, .i8, try self.offsetPtr(rhs_ptr, offset), LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
            const eq = wip.icmp(.eq, lhs, rhs, "") catch return error.OutOfMemory;
            result = wip.bin(.@"and", result, eq, "") catch return error.OutOfMemory;
        }
        return result;
    }

    /// Emits the list-equality length check and loop scaffolding, then queues the
    /// element comparison and the `.list_finish` continuation. For empty/ZST
    /// element layouts the length comparison is the whole result.
    fn emitListEqual(
        self: *MonoLlvmCodeGen,
        lhs_ptr: LlvmBuilder.Value,
        rhs_ptr: LlvmBuilder.Value,
        list_layout: layout.Idx,
        out: *LlvmBuilder.Value,
        wa: Allocator,
        work: *std.ArrayList(EqWork),
    ) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const abi = self.boxyAwareBuiltinListAbi(list_layout);
        const lhs_len = try self.loadUsize(try self.offsetPtr(lhs_ptr, self.rocListLenOffset()));
        const rhs_len = try self.loadUsize(try self.offsetPtr(rhs_ptr, self.rocListLenOffset()));
        const len_eq = wip.icmp(.eq, lhs_len, rhs_len, "") catch return error.OutOfMemory;
        if (abi.elem_size == 0) {
            out.* = len_eq;
            return;
        }

        const result_ptr = try self.allocEntryBlockSlot(.i8, 1, LlvmBuilder.Alignment.fromByteUnits(1), "list_eq");
        try self.storeBool(result_ptr, len_eq);
        const header = wip.block(0, "list_eq_header") catch return error.OutOfMemory;
        const body = wip.block(0, "list_eq_body") catch return error.OutOfMemory;
        const after = wip.block(0, "list_eq_after") catch return error.OutOfMemory;
        const idx_ptr = try self.allocEntryBlockSlot(.i64, 1, LlvmBuilder.Alignment.fromByteUnits(8), "list_eq_idx");
        _ = wip.store(.normal, builder.intValue(.i64, 0) catch return error.OutOfMemory, idx_ptr, LlvmBuilder.Alignment.fromByteUnits(8)) catch return error.OutOfMemory;
        _ = wip.br(header) catch return error.OutOfMemory;
        wip.cursor = .{ .block = header };
        const so_far = try self.loadBool(result_ptr);
        const idx = wip.load(.normal, .i64, idx_ptr, LlvmBuilder.Alignment.fromByteUnits(8), "") catch return error.OutOfMemory;
        const idx_usize = try self.coerceScalar(idx, self.ptrSizedIntType(), false);
        const in_range = wip.icmp(.ult, idx_usize, lhs_len, "") catch return error.OutOfMemory;
        const continue_loop = wip.bin(.@"and", so_far, in_range, "") catch return error.OutOfMemory;
        _ = wip.brCond(continue_loop, body, after, .none) catch return error.OutOfMemory;
        wip.cursor = .{ .block = body };
        const lhs_bytes = try self.loadPointer(lhs_ptr);
        const rhs_bytes = try self.loadPointer(rhs_ptr);
        const offset = wip.bin(.mul, idx_usize, builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory, "") catch return error.OutOfMemory;

        const state = try self.allocator.create(ListEqState);
        state.* = .{
            .result_ptr = result_ptr,
            .idx_ptr = idx_ptr,
            .idx = idx,
            .header = header,
            .after = after,
            .elem_out = undefined,
            .out = out,
        };
        try work.append(wa, .{ .list_finish = state });
        try work.append(wa, .{ .eval = .{
            .lhs_ptr = wip.gep(.inbounds, .i8, lhs_bytes, &.{offset}, "") catch return error.OutOfMemory,
            .rhs_ptr = wip.gep(.inbounds, .i8, rhs_bytes, &.{offset}, "") catch return error.OutOfMemory,
            .layout_idx = abi.elem_layout_idx orelse .zst,
            .out = &state.elem_out,
        } });
    }

    /// Stores the element comparison, emits the loop-tail increment, and loads
    /// the final list-equality result after the loop.
    fn emitListEqualFinish(self: *MonoLlvmCodeGen, state: *ListEqState) Error!void {
        defer self.allocator.destroy(state);
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        try self.storeBool(state.result_ptr, state.elem_out);
        const next = wip.bin(.add, state.idx, builder.intValue(.i64, 1) catch return error.OutOfMemory, "") catch return error.CompilationFailed;
        _ = wip.store(.normal, next, state.idx_ptr, LlvmBuilder.Alignment.fromByteUnits(8)) catch return error.CompilationFailed;
        _ = wip.br(state.header) catch return error.CompilationFailed;
        wip.cursor = .{ .block = state.after };
        state.out.* = try self.loadBool(state.result_ptr);
    }

    /// Emits the tag-equality discriminant check and case-block scaffolding, then
    /// queues processing of the first variant case.
    fn emitTagEqual(
        self: *MonoLlvmCodeGen,
        lhs_ptr: LlvmBuilder.Value,
        rhs_ptr: LlvmBuilder.Value,
        tag_layout: layout.Idx,
        out: *LlvmBuilder.Value,
        wa: Allocator,
        work: *std.ArrayList(EqWork),
    ) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const lhs_disc = try self.readTagDiscriminant(lhs_ptr, tag_layout);
        const rhs_disc = try self.readTagDiscriminant(rhs_ptr, tag_layout);
        const disc_eq = wip.icmp(.eq, lhs_disc, rhs_disc, "") catch return error.OutOfMemory;
        const data = self.layouts().getTagUnionData(self.layoutValue(tag_layout).getTagUnion().idx);
        const variants = self.layouts().getTagUnionVariants(data);
        const result_ptr = try self.allocEntryBlockSlot(.i8, 1, LlvmBuilder.Alignment.fromByteUnits(1), "tag_eq");
        try self.storeBool(result_ptr, disc_eq);
        const after = wip.block(0, "tag_eq_after") catch return error.OutOfMemory;
        const mismatch = wip.block(0, "tag_eq_mismatch") catch return error.OutOfMemory;
        const case_blocks = try self.allocator.alloc(LlvmBuilder.Function.Block.Index, variants.len);
        for (case_blocks) |*block| block.* = wip.block(0, "tag_eq_case") catch return error.OutOfMemory;
        _ = wip.brCond(disc_eq, case_blocks[0], mismatch, .then_likely) catch return error.OutOfMemory;
        wip.cursor = .{ .block = mismatch };
        try self.storeBool(result_ptr, builder.intValue(.i1, 0) catch return error.OutOfMemory);
        _ = wip.br(after) catch return error.OutOfMemory;

        const state = try self.allocator.create(TagEqState);
        state.* = .{
            .lhs_ptr = lhs_ptr,
            .rhs_ptr = rhs_ptr,
            .layout_idx = tag_layout,
            .lhs_disc = lhs_disc,
            .result_ptr = result_ptr,
            .after = after,
            .case_blocks = case_blocks,
            .index = 0,
            .payload_out = undefined,
            .out = out,
        };
        try work.append(wa, .{ .tag_case = state });
    }

    /// Emits the discriminant guard for one variant case block and queues the
    /// payload comparison plus the `.tag_case_after` continuation.
    fn emitTagEqualCase(self: *MonoLlvmCodeGen, state: *TagEqState, wa: Allocator, work: *std.ArrayList(EqWork)) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const i = state.index;
        wip.cursor = .{ .block = state.case_blocks[i] };
        if (i + 1 < state.case_blocks.len) {
            const is_case = wip.icmp(.eq, state.lhs_disc, builder.intValue(state.lhs_disc.typeOfWip(wip), i) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
            const next_case = state.case_blocks[i + 1];
            const do_case = wip.block(0, "tag_eq_do_case") catch return error.OutOfMemory;
            _ = wip.brCond(is_case, do_case, next_case, .none) catch return error.OutOfMemory;
            wip.cursor = .{ .block = do_case };
        }
        const data = self.layouts().getTagUnionData(self.layoutValue(state.layout_idx).getTagUnion().idx);
        const variants = self.layouts().getTagUnionVariants(data);
        const payload_layout = variants.get(@intCast(i)).payload_layout;
        try work.append(wa, .{ .tag_case_after = state });
        try work.append(wa, .{ .eval = .{
            .lhs_ptr = state.lhs_ptr,
            .rhs_ptr = state.rhs_ptr,
            .layout_idx = payload_layout,
            .out = &state.payload_out,
        } });
    }

    /// Stores one variant's payload comparison and branches to `after`, then
    /// either queues the next case or finishes the tag equality by loading the
    /// accumulated result.
    fn emitTagEqualCaseAfter(self: *MonoLlvmCodeGen, state: *TagEqState, wa: Allocator, work: *std.ArrayList(EqWork)) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        try self.storeBool(state.result_ptr, state.payload_out);
        _ = wip.br(state.after) catch return error.OutOfMemory;
        if (state.index + 1 < state.case_blocks.len) {
            state.index += 1;
            try work.append(wa, .{ .tag_case = state });
        } else {
            wip.cursor = .{ .block = state.after };
            state.out.* = try self.loadBool(state.result_ptr);
            self.allocator.free(state.case_blocks);
            self.allocator.destroy(state);
        }
    }

    fn emitExplicitRcStmt(
        self: *MonoLlvmCodeGen,
        helper: lir.LIR.RcHelper,
        op: layout.RcOp,
        local: LocalId,
        count: u16,
        atomicity: RcAtomicity,
    ) Error!void {
        switch (helper) {
            .concrete => |helper_key| try self.emitConcreteRcForLocal(helper_key, local, count, atomicity),
            .boxy => |desc| {
                const ptr_ty = try self.ptrType();
                try self.callBoxyVoid(
                    "roc_boxy_drop",
                    &.{ ptr_ty, .i32, ptr_ty, .i8, .i16, .i8 },
                    &.{
                        try self.boxyValuePtr(local),
                        try self.boxyInt(.i32, @intFromEnum(self.localLayout(local))),
                        try self.resolveBoxyDesc(desc),
                        try self.boxyInt(.i8, @intFromEnum(op)),
                        try self.boxyInt(.i16, count),
                        try self.boxyInt(.i8, @intFromEnum(atomicity)),
                    },
                );
            },
        }
    }

    /// Byte size of the shared scratch slot for RC-helper arguments; values
    /// wider than this pass their own slot pointer as before.
    const rc_arg_scratch_size = 64;

    fn emitRcForLocal(self: *MonoLlvmCodeGen, op: layout.RcOp, local: LocalId, count: u16, atomicity: RcAtomicity) Error!void {
        const slot_v = self.slot(local);
        if (slot_v.size == 0) return;

        const layout_val = self.layoutValue(slot_v.layout_idx);
        if (!self.layouts().layoutContainsRefcounted(layout_val)) return;
        const helper_key: layout.RcHelperKey = if (layout_val.tag == .closure)
            .{
                .op = if (op == .free) .decref else op,
                .layout_idx = layout_val.getClosure().captures_layout_idx,
            }
        else
            .{ .op = op, .layout_idx = slot_v.layout_idx };
        try self.emitConcreteRcForLocal(helper_key, local, count, atomicity);
    }

    fn emitConcreteRcForLocal(self: *MonoLlvmCodeGen, helper_key: layout.RcHelperKey, local: LocalId, count: u16, atomicity: RcAtomicity) Error!void {
        const slot_v = self.slot(local);
        if (slot_v.size == 0) return;

        if (self.deferredStrCapture(local) != null) {
            switch (helper_key.op) {
                .incref => try self.noteDeferredStrCaptureIncref(local, count, atomicity),
                .decref,
                .free,
                => self.clearDeferredStrCapture(local),
            }
            return;
        }
        // Handing the helper the local's own slot pointer makes that slot's
        // address escape, which forces every dominating store of the value
        // to stay in memory -- for a loop-carried value whose only RC traffic
        // sits on cold exit paths, that means refreshing the slot on every
        // hot iteration. Copying into a shared per-function scratch slot at
        // the call site keeps those stores next to the (usually cold) call,
        // and the helpers only read through the pointer.
        const rc_ptr = if (slot_v.size <= rc_arg_scratch_size) blk: {
            if (self.rc_arg_scratch == null) {
                self.rc_arg_scratch = try self.allocEntryBlockSlot(
                    .i8,
                    rc_arg_scratch_size,
                    LlvmBuilder.Alignment.fromByteUnits(16),
                    "rc_arg_scratch",
                );
            }
            const tmp = self.rc_arg_scratch.?;
            try self.copyBytes(tmp, slot_v.ptr, slot_v.size, slot_v.alignment);
            break :blk tmp;
        } else slot_v.ptr;

        const builder = self.builder orelse return error.CompilationFailed;
        const count_value = if (helper_key.op == .incref)
            builder.intValue(self.ptrSizedIntType(), count) catch return error.OutOfMemory
        else
            null;
        try self.emitRcHelperCall(helper_key, atomicity, rc_ptr, count_value);
    }

    /// Backend cache key for one generated RC helper. `HelperKey.encode` packs
    /// the op into bits 32..33, so the atomicity bit goes above it.
    fn rcHelperCacheKey(helper_key: layout.RcHelperKey, atomicity: RcAtomicity) u64 {
        return helper_key.encode() | (@as(u64, @intFromEnum(atomicity)) << 34);
    }

    fn staticDataRequiresRcHelper(
        self: *const MonoLlvmCodeGen,
        helper_key: layout.RcHelperKey,
        atomicity: RcAtomicity,
    ) bool {
        if (atomicity != .atomic) return false;
        for (self.static_data_rc_helpers) |required| {
            if (required.op == helper_key.op and required.layout_idx == helper_key.layout_idx) return true;
        }
        return false;
    }

    fn declareRcHelper(self: *MonoLlvmCodeGen, helper_key: layout.RcHelperKey, atomicity: RcAtomicity) Error!?LlvmBuilder.Function.Index {
        const builder = self.builder orelse return error.CompilationFailed;
        if (self.layouts().rcHelperPlan(helper_key) == .noop) return null;

        const cache_key = rcHelperCacheKey(helper_key, atomicity);
        if (self.rc_helpers.get(cache_key)) |entry| return entry.function;

        const ptr_ty = try self.ptrType();
        const params: []const LlvmBuilder.Type = switch (helper_key.op) {
            .incref => &.{ ptr_ty, self.ptrSizedIntType(), ptr_ty },
            .decref, .free => &.{ ptr_ty, ptr_ty },
        };
        const fn_ty = builder.fnType(.void, params, .normal) catch return error.OutOfMemory;
        const is_static_data_helper = self.proc_symbol_mode == .lir_symbol and
            self.staticDataRequiresRcHelper(helper_key, atomicity);
        const fn_name = if (is_static_data_helper)
            builder.strtabStringFmt("roc__rc_helper_{x}", .{cache_key}) catch return error.OutOfMemory
        else
            builder.strtabStringFmt("roc_llvm_rc_{s}_{d}{s}", .{
                @tagName(helper_key.op),
                @intFromEnum(helper_key.layout_idx),
                switch (atomicity) {
                    .atomic => "",
                    .single_thread => "_single_thread",
                },
            }) catch return error.OutOfMemory;
        const func = builder.addFunction(fn_ty, fn_name, .default) catch return error.OutOfMemory;
        func.setLinkage(if (is_static_data_helper) .external else .internal, builder);
        var attrs: LlvmBuilder.FunctionAttributes.Wip = .{};
        defer attrs.deinit(builder);
        try self.addGeneratedFunctionStackProbeAttrs(&attrs);
        switch (helper_key.op) {
            .incref => {},
            .decref, .free => {
                // Drop/free helpers include the recursive teardown code for a layout.
                // Keeping them out of line prevents LLVM from cloning large cleanup
                // trees into hot callers such as generated parsers. This does not
                // mark ordinary decrefs as cold; it only preserves the explicit RC
                // helper boundary that LIR ARC already selected.
                try attrs.addFnAttr(.@"noinline", builder);
            },
        }
        func.setAttributes(attrs.finish(builder) catch return error.OutOfMemory, builder);
        try self.rc_helpers.put(cache_key, .{
            .key = helper_key,
            .atomicity = atomicity,
            .function = func,
        });
        return func;
    }

    fn compilePendingRcHelpers(self: *MonoLlvmCodeGen) Error!void {
        while (true) {
            var pending: ?RcHelperEntry = null;
            var iter = self.rc_helpers.iterator();
            while (iter.next()) |entry| {
                if (!entry.value_ptr.compiled) {
                    pending = entry.value_ptr.*;
                    break;
                }
            }
            const helper = pending orelse return;
            try self.compileRcHelper(helper.key, helper.atomicity);
        }
    }

    fn compileRcHelper(self: *MonoLlvmCodeGen, helper_key: layout.RcHelperKey, atomicity: RcAtomicity) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const cache_key = rcHelperCacheKey(helper_key, atomicity);
        const func = blk: {
            const entry = self.rc_helpers.getPtr(cache_key) orelse return error.CompilationFailed;
            if (entry.compiled) return;
            entry.compiled = true;
            break :blk entry.function;
        };
        errdefer {
            if (self.rc_helpers.getPtr(cache_key)) |entry| {
                entry.compiled = false;
            }
        }

        const outer_wip = self.wip;
        const outer_rc_scratch = self.rc_arg_scratch;
        const outer_roc_ops = self.roc_ops_arg;
        const outer_test_context = self.test_context_arg;
        const outer_ret = self.ret_ptr_arg;
        const outer_args = self.args_ptr_arg;
        const outer_capture = self.capture_ptr_arg;
        const outer_reuse = self.reuse_ptr_arg;
        const outer_ret_desc_ptr = self.ret_desc_ptr_arg;
        const outer_runtime_ret_desc = self.current_runtime_ret_desc;
        const outer_ret_layout = self.current_ret_layout;
        const outer_slots = self.local_slots;
        defer {
            self.wip = outer_wip;
            self.rc_arg_scratch = outer_rc_scratch;
            self.roc_ops_arg = outer_roc_ops;
            self.test_context_arg = outer_test_context;
            self.ret_ptr_arg = outer_ret;
            self.args_ptr_arg = outer_args;
            self.capture_ptr_arg = outer_capture;
            self.reuse_ptr_arg = outer_reuse;
            self.ret_desc_ptr_arg = outer_ret_desc_ptr;
            self.current_runtime_ret_desc = outer_runtime_ret_desc;
            self.current_ret_layout = outer_ret_layout;
            self.local_slots = outer_slots;
        }

        var wip = LlvmBuilder.WipFunction.init(builder, .{ .function = func, .strip = true }) catch return error.OutOfMemory;
        defer wip.deinit();
        self.wip = &wip;
        self.rc_arg_scratch = null;
        self.test_context_arg = null;
        self.ret_ptr_arg = null;
        self.args_ptr_arg = null;
        self.capture_ptr_arg = null;
        self.reuse_ptr_arg = null;
        self.ret_desc_ptr_arg = null;
        self.current_runtime_ret_desc = null;
        self.current_ret_layout = .zst;
        self.local_slots = &.{};

        const entry = wip.block(0, "entry") catch return error.OutOfMemory;
        const body = wip.block(0, "rc_body") catch return error.OutOfMemory;
        const done = wip.block(0, "rc_done") catch return error.OutOfMemory;
        wip.cursor = .{ .block = entry };

        const value_ptr = wip.arg(0);
        const count_value: ?LlvmBuilder.Value = switch (helper_key.op) {
            .incref => wip.arg(1),
            .decref, .free => null,
        };
        self.roc_ops_arg = switch (helper_key.op) {
            .incref => wip.arg(2),
            .decref, .free => wip.arg(1),
        };

        const is_null = wip.icmp(.eq, value_ptr, builder.nullValue(try self.ptrType()) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        _ = wip.brCond(is_null, done, body, .else_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = body };
        try self.emitRcHelperBody(helper_key, atomicity, value_ptr, count_value);
        if (!self.currentBlockHasTerminator()) {
            _ = wip.br(done) catch return error.OutOfMemory;
        }

        wip.cursor = .{ .block = done };
        _ = wip.retVoid() catch return error.OutOfMemory;
        try self.finishCurrentWipFunction();
    }

    /// Emits one helper's body. The helper tree below a single RC statement
    /// shares the statement's atomicity: nested struct/tag/closure helpers are
    /// direct calls, so they keep it, and the element/payload callbacks handed
    /// to the teardown builtins name the helper variant matching it—the C
    /// function-pointer ABI carries no atomicity parameter, so the atomicity
    /// is baked into which function the pointer names. Free builtins never
    /// update a count, so they have no single-thread entries.
    fn emitRcHelperBody(self: *MonoLlvmCodeGen, helper_key: layout.RcHelperKey, atomicity: RcAtomicity, value_ptr: LlvmBuilder.Value, count_value: ?LlvmBuilder.Value) Error!void {
        switch (self.layouts().rcHelperPlan(helper_key)) {
            .noop => {},
            .str_incref => try self.emitRcHelperStrIncref(value_ptr, count_value.?, atomicity),
            .str_decref => try self.emitRcHelperStrDrop(value_ptr, LowLevelBuiltins.rcHelper(.data_ptr_decref, tableAtomicity(atomicity)).symbolName()),
            .str_free => try self.emitRcHelperStrDrop(value_ptr, builtinSymbol(LowLevelBuiltins.rcHelper(.data_ptr_free, .atomic))),
            .list_incref => |list_plan| try self.emitRcHelperListIncref(list_plan, value_ptr, count_value.?, atomicity),
            .list_decref => |list_plan| try self.emitRcHelperListDrop(list_plan, value_ptr, atomicity, LowLevelBuiltins.rcHelper(.list_decref, tableAtomicity(atomicity)).symbolName()),
            .list_free => |list_plan| try self.emitRcHelperListDrop(list_plan, value_ptr, atomicity, builtinSymbol(LowLevelBuiltins.rcHelper(.list_free, .atomic))),
            .box_incref => try self.emitRcHelperBoxIncref(value_ptr, count_value.?, atomicity),
            .box_decref => |box_plan| try self.emitRcHelperBoxDrop(box_plan, value_ptr, atomicity, LowLevelBuiltins.rcHelper(.box_decref, tableAtomicity(atomicity)).symbolName()),
            .box_free => |box_plan| try self.emitRcHelperBoxDrop(box_plan, value_ptr, atomicity, builtinSymbol(LowLevelBuiltins.rcHelper(.box_free, .atomic))),
            .erased_callable_incref => try self.emitRcHelperErasedCallableIncref(value_ptr, count_value.?, atomicity),
            .erased_callable_decref => try self.emitRcHelperErasedCallableDrop(value_ptr, LowLevelBuiltins.rcHelper(.erased_callable_decref, tableAtomicity(atomicity)).symbolName()),
            .erased_callable_free => try self.emitRcHelperErasedCallableDrop(value_ptr, builtinSymbol(LowLevelBuiltins.rcHelper(.erased_callable_free, .atomic))),
            .struct_ => |struct_plan| try self.emitRcHelperStruct(struct_plan, value_ptr, count_value, atomicity),
            .tag_union => |tag_plan| try self.emitRcHelperTagUnion(tag_plan, value_ptr, count_value, atomicity),
            .closure => |child_key| {
                const captures_ptr = try self.loadPointer(value_ptr);
                try self.emitRcHelperCall(child_key, atomicity, captures_ptr, if (child_key.op == .incref) count_value else null);
            },
        }
    }

    fn emitRcHelperCall(self: *MonoLlvmCodeGen, helper_key: layout.RcHelperKey, atomicity: RcAtomicity, value_ptr: LlvmBuilder.Value, count_value: ?LlvmBuilder.Value) Error!void {
        const func = (try self.declareRcHelper(helper_key, atomicity)) orelse return;
        switch (helper_key.op) {
            .incref => _ = try self.callFunctionIndex(func, &.{ value_ptr, count_value.?, self.rocOps() }, false),
            .decref, .free => _ = try self.callFunctionIndex(func, &.{ value_ptr, self.rocOps() }, false),
        }
    }

    fn loadStrDataPtrForRc(self: *MonoLlvmCodeGen, value_ptr: LlvmBuilder.Value) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const bytes = try self.loadPointer(value_ptr);
        const cap_or_alloc = try self.loadUsize(try self.offsetPtr(value_ptr, self.rocStrCapacityOffset()));
        const ptr_int_ty = self.ptrSizedIntType();
        const bytes_int = wip.cast(.ptrtoint, bytes, ptr_int_ty, "") catch return error.OutOfMemory;
        const slice_tag = wip.bin(.@"and", cap_or_alloc, builder.intValue(ptr_int_ty, 1) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const slice_mask = wip.bin(.sub, builder.intValue(ptr_int_ty, 0) catch return error.OutOfMemory, slice_tag, "") catch return error.OutOfMemory;
        const owned_mask = wip.not(slice_mask, "") catch return error.OutOfMemory;
        const owned_ptr = wip.bin(.@"and", bytes_int, owned_mask, "") catch return error.OutOfMemory;
        const slice_alloc = wip.bin(.@"and", cap_or_alloc, builder.intValue(ptr_int_ty, -2) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const slice_ptr = wip.bin(.@"and", slice_alloc, slice_mask, "") catch return error.OutOfMemory;
        const data_ptr = wip.bin(.@"or", owned_ptr, slice_ptr, "") catch return error.OutOfMemory;
        return wip.cast(.inttoptr, data_ptr, try self.ptrType(), "") catch return error.OutOfMemory;
    }

    fn increfDataPtrBuiltinName(atomicity: RcAtomicity) []const u8 {
        return LowLevelBuiltins.rcHelper(.data_ptr_incref, tableAtomicity(atomicity)).symbolName();
    }

    fn emitRcHelperStrIncref(self: *MonoLlvmCodeGen, value_ptr: LlvmBuilder.Value, count_value: LlvmBuilder.Value, atomicity: RcAtomicity) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const len = try self.loadUsize(try self.offsetPtr(value_ptr, self.rocStrLenOffset()));
        const is_small = wip.icmp(.slt, len, builder.intValue(self.ptrSizedIntType(), 0) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const heap_str = wip.block(0, "str_heap") catch return error.OutOfMemory;
        const after = wip.block(0, "str_after") catch return error.OutOfMemory;
        _ = wip.brCond(is_small, after, heap_str, .else_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = heap_str };
        const data_ptr = try self.loadStrDataPtrForRc(value_ptr);
        try self.callBuiltinVoid(increfDataPtrBuiltinName(atomicity), &.{ try self.ptrType(), self.ptrSizedIntType(), try self.ptrType() }, &.{ data_ptr, count_value, self.rocOps() });
        _ = wip.br(after) catch return error.OutOfMemory;

        wip.cursor = .{ .block = after };
    }

    fn emitRcHelperStrDrop(self: *MonoLlvmCodeGen, value_ptr: LlvmBuilder.Value, builtin_name: []const u8) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const len = try self.loadUsize(try self.offsetPtr(value_ptr, self.rocStrLenOffset()));
        const is_small = wip.icmp(.slt, len, builder.intValue(self.ptrSizedIntType(), 0) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const heap_str = wip.block(0, "str_heap") catch return error.OutOfMemory;
        const after = wip.block(0, "str_after") catch return error.OutOfMemory;
        _ = wip.brCond(is_small, after, heap_str, .else_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = heap_str };
        const data_ptr = try self.loadStrDataPtrForRc(value_ptr);
        try self.callBuiltinVoid(
            builtin_name,
            &.{ try self.ptrType(), .i32, .i1, try self.ptrType() },
            &.{
                data_ptr,
                builder.intValue(.i32, 1) catch return error.OutOfMemory,
                builder.intValue(.i1, 0) catch return error.OutOfMemory,
                self.rocOps(),
            },
        );
        _ = wip.br(after) catch return error.OutOfMemory;

        wip.cursor = .{ .block = after };
    }

    fn emitRcHelperListIncref(self: *MonoLlvmCodeGen, list_plan: layout.RcListPlan, value_ptr: LlvmBuilder.Value, count_value: LlvmBuilder.Value, atomicity: RcAtomicity) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const fields = try self.rocListArgFields(value_ptr);
        try self.callBuiltinVoid(
            LowLevelBuiltins.rcHelper(.list_incref, tableAtomicity(atomicity)).symbolName(),
            &.{ try self.ptrType(), self.ptrSizedIntType(), self.ptrSizedIntType(), self.ptrSizedIntType(), .i1, try self.ptrType() },
            &.{
                fields[0],
                fields[1],
                fields[2],
                count_value,
                builder.intValue(.i1, @intFromBool(list_plan.child != null)) catch return error.OutOfMemory,
                self.rocOps(),
            },
        );
    }

    fn emitRcHelperListDrop(self: *MonoLlvmCodeGen, list_plan: layout.RcListPlan, value_ptr: LlvmBuilder.Value, atomicity: RcAtomicity, builtin_name: []const u8) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const fields = try self.rocListArgFields(value_ptr);
        // The element callback's C ABI carries no atomicity parameter, so the
        // statement's atomicity is baked into which helper variant the pointer
        // names. Visibility is containment-closed (design.md "Thread-Confined
        // Reference Counts"), so a single-thread teardown covers the elements
        // as well.
        const child_fn = if (list_plan.child) |child_key|
            (try self.declareRcHelper(child_key, atomicity))
        else
            null;
        try self.callBuiltinVoid(
            builtin_name,
            &.{ try self.ptrType(), self.ptrSizedIntType(), self.ptrSizedIntType(), .i32, self.ptrSizedIntType(), try self.ptrType(), try self.ptrType() },
            &.{
                fields[0],
                fields[1],
                fields[2],
                builder.intValue(.i32, list_plan.elem_alignment) catch return error.OutOfMemory,
                builder.intValue(self.ptrSizedIntType(), list_plan.elem_width) catch return error.OutOfMemory,
                if (child_fn) |func| func.toValue(builder) else builder.nullValue(try self.ptrType()) catch return error.OutOfMemory,
                self.rocOps(),
            },
        );
    }

    fn emitRcHelperBoxIncref(self: *MonoLlvmCodeGen, value_ptr: LlvmBuilder.Value, count_value: LlvmBuilder.Value, atomicity: RcAtomicity) Error!void {
        const payload_ptr = try self.loadPointer(value_ptr);
        try self.callBuiltinVoid(
            increfDataPtrBuiltinName(atomicity),
            &.{ try self.ptrType(), self.ptrSizedIntType(), try self.ptrType() },
            &.{ payload_ptr, count_value, self.rocOps() },
        );
    }

    fn emitRcHelperBoxDrop(self: *MonoLlvmCodeGen, box_plan: layout.RcBoxPlan, value_ptr: LlvmBuilder.Value, atomicity: RcAtomicity, builtin_name: []const u8) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const payload_ptr = try self.loadPointer(value_ptr);
        // The payload callback's C ABI carries no atomicity parameter, so the
        // statement's atomicity is baked into which helper variant the pointer
        // names (see emitRcHelperListDrop).
        const child_fn = if (box_plan.child) |child_key|
            (try self.declareRcHelper(child_key, atomicity))
        else
            null;
        try self.callBuiltinVoid(
            builtin_name,
            &.{ try self.ptrType(), .i32, try self.ptrType(), try self.ptrType() },
            &.{
                payload_ptr,
                builder.intValue(.i32, box_plan.elem_alignment) catch return error.OutOfMemory,
                if (child_fn) |func| func.toValue(builder) else builder.nullValue(try self.ptrType()) catch return error.OutOfMemory,
                self.rocOps(),
            },
        );
    }

    fn emitRcHelperErasedCallableIncref(self: *MonoLlvmCodeGen, value_ptr: LlvmBuilder.Value, count_value: LlvmBuilder.Value, atomicity: RcAtomicity) Error!void {
        const payload_ptr = try self.loadPointer(value_ptr);
        // An erased-callable incref is a plain data-pointer incref on the
        // payload allocation, so the single-thread mode uses the data-pointer
        // entry directly.
        try self.callBuiltinVoid(
            LowLevelBuiltins.rcHelper(.erased_callable_incref, tableAtomicity(atomicity)).symbolName(),
            &.{ try self.ptrType(), self.ptrSizedIntType(), try self.ptrType() },
            &.{ payload_ptr, count_value, self.rocOps() },
        );
    }

    fn emitRcHelperErasedCallableDrop(self: *MonoLlvmCodeGen, value_ptr: LlvmBuilder.Value, builtin_name: []const u8) Error!void {
        const payload_ptr = try self.loadPointer(value_ptr);
        try self.callBuiltinVoid(
            builtin_name,
            &.{ try self.ptrType(), try self.ptrType() },
            &.{ payload_ptr, self.rocOps() },
        );
    }

    fn emitRcHelperStruct(self: *MonoLlvmCodeGen, struct_plan: layout.RcStructPlan, value_ptr: LlvmBuilder.Value, count_value: ?LlvmBuilder.Value, atomicity: RcAtomicity) Error!void {
        const field_count = self.layouts().rcHelperStructFieldCount(struct_plan);
        var i: u32 = 0;
        while (i < field_count) : (i += 1) {
            const field_plan = self.layouts().rcHelperStructFieldPlan(struct_plan, i) orelse continue;
            const field_ptr = try self.offsetPtr(value_ptr, field_plan.offset);
            try self.emitRcHelperCall(field_plan.child, atomicity, field_ptr, if (field_plan.child.op == .incref) count_value else null);
        }
    }

    fn emitRcHelperTagUnion(self: *MonoLlvmCodeGen, tag_plan: layout.RcTagUnionPlan, value_ptr: LlvmBuilder.Value, count_value: ?LlvmBuilder.Value, atomicity: RcAtomicity) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const variant_count = self.layouts().rcHelperTagUnionVariantCount(tag_plan);
        if (variant_count == 0) return;

        if (variant_count == 1) {
            if (self.layouts().rcHelperTagUnionVariantPlan(tag_plan, 0)) |child_key| {
                try self.emitRcHelperCall(child_key, atomicity, value_ptr, if (child_key.op == .incref) count_value else null);
            }
            return;
        }

        const disc_size = self.layouts().rcHelperTagUnionDiscriminantSize(tag_plan);
        if (disc_size == 0) return;
        const disc_offset = self.layouts().rcHelperTagUnionDiscriminantOffset(tag_plan);
        const disc_ptr = try self.offsetPtr(value_ptr, disc_offset);
        const disc_raw = wip.load(.normal, intTypeForBytes(disc_size), disc_ptr, LlvmBuilder.Alignment.fromByteUnits(@max(disc_size, 1)), "") catch return error.OutOfMemory;
        const disc = try self.coerceScalar(disc_raw, .i64, false);
        const after = wip.block(0, "rc_tag_after") catch return error.OutOfMemory;

        var variant_i: u32 = 0;
        while (variant_i < variant_count) : (variant_i += 1) {
            const child_key = self.layouts().rcHelperTagUnionVariantPlan(tag_plan, variant_i) orelse continue;
            const do_case = wip.block(0, "rc_tag_case") catch return error.OutOfMemory;
            const next_case = wip.block(0, "rc_tag_next") catch return error.OutOfMemory;
            const is_case = wip.icmp(.eq, disc, builder.intValue(.i64, variant_i) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
            _ = wip.brCond(is_case, do_case, next_case, .none) catch return error.OutOfMemory;

            wip.cursor = .{ .block = do_case };
            try self.emitRcHelperCall(child_key, atomicity, value_ptr, if (child_key.op == .incref) count_value else null);
            _ = wip.br(after) catch return error.OutOfMemory;

            wip.cursor = .{ .block = next_case };
        }

        _ = wip.br(after) catch return error.OutOfMemory;
        wip.cursor = .{ .block = after };
    }

    fn ptrType(self: *MonoLlvmCodeGen) Error!LlvmBuilder.Type {
        return (self.builder orelse return error.CompilationFailed).ptrType(.default) catch return error.OutOfMemory;
    }

    fn ptrSizedIntType(self: *const MonoLlvmCodeGen) LlvmBuilder.Type {
        return switch (self.target.ptrBitWidth()) {
            16 => .i16,
            32 => .i32,
            64 => .i64,
            else => {
                if (builtin.mode == .Debug) {
                    std.debug.panic("LLVM codegen invariant violated: unsupported target pointer width {d}", .{self.target.ptrBitWidth()});
                }
                unreachable;
            },
        };
    }

    fn targetWordSize(self: *const MonoLlvmCodeGen) u32 {
        return switch (self.target.ptrBitWidth()) {
            16 => 2,
            32 => 4,
            64 => 8,
            else => {
                if (builtin.mode == .Debug) {
                    std.debug.panic("LLVM codegen invariant violated: unsupported target pointer width {d}", .{self.target.ptrBitWidth()});
                }
                unreachable;
            },
        };
    }

    fn targetPointerAlignment(self: *const MonoLlvmCodeGen) LlvmBuilder.Alignment {
        return LlvmBuilder.Alignment.fromByteUnits(self.targetWordSize());
    }

    fn rocListLenOffset(self: *const MonoLlvmCodeGen) u32 {
        return self.targetWordSize();
    }

    fn rocListCapacityOffset(self: *const MonoLlvmCodeGen) u32 {
        return 2 * self.targetWordSize();
    }

    fn rocStrCapacityOffset(self: *const MonoLlvmCodeGen) u32 {
        return self.targetWordSize();
    }

    fn rocStrLenOffset(self: *const MonoLlvmCodeGen) u32 {
        return 2 * self.targetWordSize();
    }

    fn rocOpsCallbackOffset(self: *const MonoLlvmCodeGen, callback: RocOpsCallback) u32 {
        if (self.target.cpu.arch == .wasm32) {
            return switch (callback) {
                .dbg => 16,
                .expect_failed => 20,
                .crashed => 24,
            };
        }
        return switch (callback) {
            .dbg => @intCast(@offsetOf(builtins.host_abi.RocOps, "roc_dbg")),
            .expect_failed => @intCast(@offsetOf(builtins.host_abi.RocOps, "roc_expect_failed")),
            .crashed => @intCast(@offsetOf(builtins.host_abi.RocOps, "roc_crashed")),
        };
    }

    fn rocOpsHostedFnsPtrOffset(self: *const MonoLlvmCodeGen) u32 {
        if (self.target.cpu.arch == .wasm32) return 32;
        return @as(u32, @intCast(@offsetOf(builtins.host_abi.RocOps, "hosted_fns"))) +
            @as(u32, @intCast(@offsetOf(builtins.host_abi.HostedFunctions, "fns")));
    }

    fn rocOps(self: *MonoLlvmCodeGen) LlvmBuilder.Value {
        return self.roc_ops_arg orelse unreachable;
    }

    fn testInvocationContext(self: *MonoLlvmCodeGen) LlvmBuilder.Value {
        return self.test_context_arg orelse unreachable;
    }

    fn layouts(self: *MonoLlvmCodeGen) *const layout.Store {
        return self.layout_store orelse @panic("LLVM codegen missing layout_store");
    }

    fn boxyAwareBuiltinListAbi(self: *MonoLlvmCodeGen, list_layout: layout.Idx) layout.Store.BuiltinListAbi {
        return self.layouts().builtinListAbi(list_layout);
    }

    fn layoutValue(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) layout.Layout {
        return self.layouts().getLayout(layout_idx);
    }

    fn localLayout(self: *MonoLlvmCodeGen, local: LocalId) layout.Idx {
        return self.store.getLocal(local).layout_idx;
    }

    fn isStrLocal(self: *MonoLlvmCodeGen, local: LocalId) bool {
        const layout_val = self.layoutValue(self.localLayout(local));
        return layout_val.tag == .scalar and layout_val.getScalar().tag == .str;
    }

    fn slot(self: *MonoLlvmCodeGen, local: LocalId) LocalSlot {
        const local_slot = self.local_slots[@intFromEnum(local)];
        if (!local_slot.allocated) {
            if (builtin.mode == .Debug) {
                std.debug.panic("LLVM codegen invariant violated: local {d} was not in the current proc frame", .{@intFromEnum(local)});
            }
            unreachable;
        }
        return local_slot;
    }

    fn sizeAlignOf(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) layout.SizeAlign {
        return self.layouts().layoutSizeAlign(self.layoutValue(layout_idx));
    }

    fn layoutByteSize(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) u32 {
        return self.sizeAlignOf(layout_idx).size;
    }

    fn llvmAlignment(_: *MonoLlvmCodeGen, roc_alignment: layout.RocAlignment) LlvmBuilder.Alignment {
        return LlvmBuilder.Alignment.fromByteUnits(@max(roc_alignment.toByteUnits(), 1));
    }

    fn alignmentForLayout(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) LlvmBuilder.Alignment {
        return self.llvmAlignment(self.sizeAlignOf(layout_idx).alignment);
    }

    fn alignmentForLayoutOffset(self: *MonoLlvmCodeGen, layout_idx: layout.Idx, offset: u32) LlvmBuilder.Alignment {
        const base_alignment = @max(self.sizeAlignOf(layout_idx).alignment.toByteUnits(), 1);
        if (offset == 0) return LlvmBuilder.Alignment.fromByteUnits(base_alignment);
        const offset_alignment = @as(u32, 1) << @intCast(@ctz(offset));
        return LlvmBuilder.Alignment.fromByteUnits(@min(base_alignment, offset_alignment));
    }

    fn offsetPtr(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, offset: u32) Error!LlvmBuilder.Value {
        if (offset == 0) return ptr;
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        return wip.gep(.inbounds, .i8, ptr, &.{builder.intValue(.i32, offset) catch return error.OutOfMemory}, "") catch return error.OutOfMemory;
    }

    fn copyBytes(self: *MonoLlvmCodeGen, dst: LlvmBuilder.Value, src: LlvmBuilder.Value, size: u32, alignment: LlvmBuilder.Alignment) Error!void {
        if (size == 0) return;
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        _ = wip.callMemCpy(dst, alignment, src, alignment, builder.intValue(self.ptrSizedIntType(), size) catch return error.OutOfMemory, .normal, false) catch return error.OutOfMemory;
    }

    fn zeroBytes(self: *MonoLlvmCodeGen, dst: LlvmBuilder.Value, size: u32) Error!void {
        if (size == 0) return;
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        _ = wip.callMemSet(dst, LlvmBuilder.Alignment.fromByteUnits(1), builder.intValue(.i8, 0) catch return error.OutOfMemory, builder.intValue(self.ptrSizedIntType(), size) catch return error.OutOfMemory, .normal, false) catch return error.OutOfMemory;
    }

    fn storePointer(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, value: LlvmBuilder.Value) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        _ = wip.store(.normal, value, ptr, self.targetPointerAlignment()) catch return error.OutOfMemory;
    }

    fn storeUsize(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, value: LlvmBuilder.Value) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        _ = wip.store(.normal, value, ptr, self.targetPointerAlignment()) catch return error.OutOfMemory;
    }

    fn storeListLen(self: *MonoLlvmCodeGen, list_ptr: LlvmBuilder.Value, value: LlvmBuilder.Value) Error!void {
        try self.storeUsize(try self.offsetPtr(list_ptr, self.rocListLenOffset()), value);
    }

    fn storeListCapacity(self: *MonoLlvmCodeGen, list_ptr: LlvmBuilder.Value, value: LlvmBuilder.Value) Error!void {
        try self.storeUsize(try self.offsetPtr(list_ptr, self.rocListCapacityOffset()), value);
    }

    fn storeRawInt(self: *MonoLlvmCodeGen, base: LlvmBuilder.Value, offset: u32, ty: LlvmBuilder.Type, value: u64, alignment: u32) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const ptr = try self.offsetPtr(base, offset);
        _ = wip.store(
            .normal,
            builder.intValue(ty, @as(i64, @intCast(value))) catch return error.OutOfMemory,
            ptr,
            LlvmBuilder.Alignment.fromByteUnits(alignment),
        ) catch return error.OutOfMemory;
    }

    fn loadPointer(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        return wip.load(.normal, try self.ptrType(), ptr, self.targetPointerAlignment(), "") catch return error.OutOfMemory;
    }

    fn loadUsize(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        return wip.load(.normal, self.ptrSizedIntType(), ptr, self.targetPointerAlignment(), "") catch return error.OutOfMemory;
    }

    fn scalarType(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) LlvmBuilder.Type {
        return switch (layout_idx) {
            .bool, .u8, .i8 => .i8,
            .u16, .i16 => .i16,
            .u32, .i32 => .i32,
            .u64, .i64 => .i64,
            .u128, .i128, .dec => .i128,
            .f32 => .float,
            .f64 => .double,
            .opaque_ptr => self.ptrSizedIntType(),
            .str, .zst, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2, _ => self.ptrSizedIntType(),
        };
    }

    fn loadScalar(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        return wip.load(.normal, self.scalarType(layout_idx), ptr, self.alignmentForLayout(layout_idx), "") catch return error.OutOfMemory;
    }

    fn storeScalar(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, layout_idx: layout.Idx, value: LlvmBuilder.Value) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const store_value = try self.coerceScalar(value, self.scalarType(layout_idx), layout_idx.isSigned());
        _ = wip.store(.normal, store_value, ptr, self.alignmentForLayout(layout_idx)) catch return error.OutOfMemory;
    }

    fn storeIntToLayout(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, value: LlvmBuilder.Value, layout_idx: layout.Idx) Error!void {
        if (self.layoutValue(layout_idx).tag == .tag_union) {
            try self.storeTagDiscriminant(ptr, layout_idx, value);
            return;
        }
        try self.storeScalar(ptr, layout_idx, value);
    }

    fn storeIntLiteral(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, layout_idx: layout.Idx, value: i64) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        if (self.simdVectorForLayout(layout_idx)) |vector| {
            const wip = self.wip orelse return error.CompilationFailed;
            const bits = builder.intValue(.i128, @as(u128, @as(u64, @bitCast(value)))) catch return error.OutOfMemory;
            const vector_value = wip.cast(.bitcast, bits, try self.simdType(vector), "") catch return error.OutOfMemory;
            _ = wip.store(.normal, vector_value, ptr, self.alignmentForLayout(layout_idx)) catch return error.OutOfMemory;
            return;
        }
        const ty = self.scalarType(layout_idx);
        const bits: u64 = @bitCast(value);
        const literal = switch (self.intBits(layout_idx)) {
            8 => builder.intValue(ty, @as(u8, @truncate(bits))) catch return error.OutOfMemory,
            16 => builder.intValue(ty, @as(u16, @truncate(bits))) catch return error.OutOfMemory,
            32 => builder.intValue(ty, @as(u32, @truncate(bits))) catch return error.OutOfMemory,
            64 => builder.intValue(ty, bits) catch return error.OutOfMemory,
            128 => builder.intValue(ty, @as(u128, bits)) catch return error.OutOfMemory,
            else => return error.CompilationFailed,
        };
        try self.storeScalar(ptr, layout_idx, literal);
    }

    fn storeI128Literal(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, layout_idx: layout.Idx, value: i128) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        if (self.simdVectorForLayout(layout_idx)) |vector| {
            const wip = self.wip orelse return error.CompilationFailed;
            const bits = builder.intValue(.i128, @as(u128, @bitCast(value))) catch return error.OutOfMemory;
            const vector_value = wip.cast(.bitcast, bits, try self.simdType(vector), "") catch return error.OutOfMemory;
            _ = wip.store(.normal, vector_value, ptr, self.alignmentForLayout(layout_idx)) catch return error.OutOfMemory;
            return;
        }
        const ty = self.scalarType(layout_idx);
        const bits: u128 = @bitCast(value);
        const literal = switch (self.intBits(layout_idx)) {
            8 => builder.intValue(ty, @as(u8, @truncate(bits))) catch return error.OutOfMemory,
            16 => builder.intValue(ty, @as(u16, @truncate(bits))) catch return error.OutOfMemory,
            32 => builder.intValue(ty, @as(u32, @truncate(bits))) catch return error.OutOfMemory,
            64 => builder.intValue(ty, @as(u64, @truncate(bits))) catch return error.OutOfMemory,
            128 => builder.intValue(ty, bits) catch return error.OutOfMemory,
            else => return error.CompilationFailed,
        };
        try self.storeScalar(ptr, layout_idx, literal);
    }

    fn storeFloatLiteral(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, layout_idx: layout.Idx, value: anytype) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const llvm_value = if (layout_idx == .f32)
            builder.floatValue(@floatCast(value)) catch return error.OutOfMemory
        else
            builder.doubleValue(@floatCast(value)) catch return error.OutOfMemory;
        try self.storeScalar(ptr, layout_idx, llvm_value);
    }

    fn coerceScalar(self: *MonoLlvmCodeGen, value: LlvmBuilder.Value, target_ty: LlvmBuilder.Type, signed: bool) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const value_ty = value.typeOfWip(wip);
        if (value_ty == target_ty) return value;
        return wip.conv(if (signed) .signed else .unsigned, value, target_ty, "") catch return error.OutOfMemory;
    }

    fn loadBool(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const byte = wip.load(.normal, .i8, ptr, LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
        return wip.icmp(.ne, byte, builder.intValue(.i8, 0) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
    }

    fn storeBool(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, cond: LlvmBuilder.Value) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const byte = try self.coerceScalar(cond, .i8, false);
        _ = wip.store(.normal, byte, ptr, LlvmBuilder.Alignment.fromByteUnits(1)) catch return error.OutOfMemory;
    }

    fn readSwitchValue(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        const layout_val = self.layoutValue(layout_idx);
        if (layout_val.tag == .tag_union) {
            return self.readTagDiscriminant(ptr, layout_idx);
        }
        const value = if (layout_idx == .bool) blk: {
            const b = try self.loadBool(ptr);
            break :blk try self.coerceScalar(b, .i64, false);
        } else try self.loadScalar(ptr, layout_idx);
        return self.coerceScalar(value, .i64, false);
    }

    fn readTagDiscriminant(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const layout_val = self.layoutValue(layout_idx);
        if (layout_val.tag != .tag_union) return builder.intValue(.i64, 0) catch return error.OutOfMemory;
        const data = self.layouts().getTagUnionData(layout_val.getTagUnion().idx);
        if (data.discriminant_size == 0) return builder.intValue(.i64, 0) catch return error.OutOfMemory;
        const disc_ptr = try self.offsetPtr(ptr, data.discriminant_offset.get(self.layouts().targetUsize()));
        const ty = intTypeForBytes(data.discriminant_size);
        const raw = wip.load(.normal, ty, disc_ptr, LlvmBuilder.Alignment.fromByteUnits(@max(data.discriminant_size, 1)), "") catch return error.OutOfMemory;
        return self.coerceScalar(raw, .i64, false);
    }

    fn tagDiscriminantOffset(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) Error!u32 {
        const layout_val = self.layoutValue(layout_idx);
        if (layout_val.tag != .tag_union) return error.CompilationFailed;
        const data = self.layouts().getTagUnionData(layout_val.getTagUnion().idx);
        return data.discriminant_offset.get(self.layouts().targetUsize());
    }

    fn writeTagDiscriminant(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, layout_idx: layout.Idx, discriminant: u16) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const layout_val = self.layoutValue(layout_idx);
        if (layout_val.tag != .tag_union) return;
        const data = self.layouts().getTagUnionData(layout_val.getTagUnion().idx);
        if (data.discriminant_size == 0) return;
        const disc_ptr = try self.offsetPtr(ptr, data.discriminant_offset.get(self.layouts().targetUsize()));
        const ty = intTypeForBytes(data.discriminant_size);
        _ = wip.store(.normal, builder.intValue(ty, discriminant) catch return error.OutOfMemory, disc_ptr, LlvmBuilder.Alignment.fromByteUnits(@max(data.discriminant_size, 1))) catch return error.OutOfMemory;
    }

    fn storeTagDiscriminant(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, layout_idx: layout.Idx, value: LlvmBuilder.Value) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const layout_val = self.layoutValue(layout_idx);
        if (layout_val.tag != .tag_union) return error.CompilationFailed;
        const data = self.layouts().getTagUnionData(layout_val.getTagUnion().idx);
        if (data.discriminant_size == 0) return;
        const disc_ptr = try self.offsetPtr(ptr, data.discriminant_offset.get(self.layouts().targetUsize()));
        const ty = intTypeForBytes(data.discriminant_size);
        const store_value = try self.coerceScalar(value, ty, false);
        _ = wip.store(.normal, store_value, disc_ptr, LlvmBuilder.Alignment.fromByteUnits(@max(data.discriminant_size, 1))) catch return error.OutOfMemory;
    }

    fn tagPayloadLayout(self: *MonoLlvmCodeGen, layout_idx: layout.Idx, discriminant: u16) layout.Idx {
        var tag_layout = self.layoutValue(layout_idx);
        if (tag_layout.tag == .box) tag_layout = self.layoutValue(tag_layout.getIdx());
        if (tag_layout.tag != .tag_union) return .zst;
        const data = self.layouts().getTagUnionData(tag_layout.getTagUnion().idx);
        const variants = self.layouts().getTagUnionVariants(data);
        if (discriminant >= variants.len) return .zst;
        return variants.get(discriminant).payload_layout;
    }

    fn resolveStrFromUtf8Layout(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) Error!StrFromUtf8LayoutInfo {
        const ret_layout_val = self.layoutValue(layout_idx);
        if (ret_layout_val.tag != .tag_union) return error.CompilationFailed;
        const tu_data = self.layouts().getTagUnionData(ret_layout_val.getTagUnion().idx);
        const variants = self.layouts().getTagUnionVariants(tu_data);

        var ok_disc: ?u16 = null;
        var err_disc: ?u16 = null;
        var err_record_idx: ?layout.StructIdx = null;
        var inner_disc_offset: u32 = 0;
        var inner_disc_size: u32 = 0;
        var inner_bad_utf8_disc: u32 = 0;

        for (0..variants.len) |i| {
            const payload = variants.get(@intCast(i)).payload_layout;
            const candidate = self.unwrapSingleFieldPayloadLayout(payload) orelse payload;
            if (candidate == .str) {
                ok_disc = @intCast(i);
                continue;
            }

            err_disc = @intCast(i);
            const err_layout = self.layoutValue(candidate);
            if (err_layout.tag == .struct_) {
                err_record_idx = err_layout.getStruct().idx;
            } else if (err_layout.tag == .tag_union) {
                const inner_tu = self.layouts().getTagUnionData(err_layout.getTagUnion().idx);
                if (self.findBadUtf8Variant(inner_tu)) |info| {
                    err_record_idx = info.struct_idx;
                    inner_disc_offset = inner_tu.discriminant_offset.get(self.layouts().targetUsize());
                    inner_disc_size = inner_tu.discriminant_size;
                    inner_bad_utf8_disc = info.disc;
                }
            }
        }

        const rec_idx = err_record_idx orelse return error.CompilationFailed;
        const struct_data = self.layouts().getStructData(rec_idx);
        const fields = self.layouts().struct_fields.sliceRange(struct_data.getFields());
        var index_offset: ?u32 = null;
        var problem_offset: ?u32 = null;
        for (0..fields.len) |i| {
            const field = fields.get(i);
            const field_layout = self.layoutValue(field.layout);
            const field_size = self.layoutByteSize(field.layout);
            const field_offset = self.layouts().getStructFieldOffsetByOriginalIndex(rec_idx, field.index);
            const is_index = field_layout.tag == .scalar and
                field_layout.getScalar().tag == .int and
                (field_layout.getScalar().getInt() == .u64 or field_layout.getScalar().getInt() == .i64);
            if (is_index and field_size == 8) {
                index_offset = field_offset;
            } else if (field_size == 1) {
                problem_offset = field_offset;
            }
        }

        return .{
            .ok_tag = ok_disc orelse return error.CompilationFailed,
            .err_tag = err_disc orelse return error.CompilationFailed,
            .outer_disc_offset = tu_data.discriminant_offset.get(self.layouts().targetUsize()),
            .outer_disc_size = tu_data.discriminant_size,
            .err_index_offset = index_offset orelse return error.CompilationFailed,
            .err_problem_offset = problem_offset orelse return error.CompilationFailed,
            .inner_disc_offset = inner_disc_offset,
            .inner_disc_size = inner_disc_size,
            .inner_bad_utf8_tag = inner_bad_utf8_disc,
        };
    }

    fn resolveStrSplitFirstLayout(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) Error!StrSplitFirstLayoutInfo {
        const ret_layout_val = self.layoutValue(layout_idx);
        if (ret_layout_val.tag != .struct_) return error.CompilationFailed;

        const record_idx = ret_layout_val.getStruct().idx;
        const record_data = self.layouts().getStructData(record_idx);
        const fields = self.layouts().struct_fields.sliceRange(record_data.getFields());
        if (fields.len != 3) return error.CompilationFailed;

        if (self.layouts().getStructFieldLayoutByOriginalIndex(record_idx, 0) != .str) return error.CompilationFailed;
        if (self.layouts().getStructFieldLayoutByOriginalIndex(record_idx, 1) != .str) return error.CompilationFailed;
        if (self.layouts().getStructFieldLayoutByOriginalIndex(record_idx, 2) != .bool) return error.CompilationFailed;

        return .{
            .after_offset = self.layouts().getStructFieldOffsetByOriginalIndex(record_idx, 0),
            .before_offset = self.layouts().getStructFieldOffsetByOriginalIndex(record_idx, 1),
            .found_offset = self.layouts().getStructFieldOffsetByOriginalIndex(record_idx, 2),
        };
    }

    fn resolveStrSplitLastLayout(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) Error!StrSplitLastLayoutInfo {
        const ret_layout_val = self.layoutValue(layout_idx);
        if (ret_layout_val.tag != .struct_) return error.CompilationFailed;

        const record_idx = ret_layout_val.getStruct().idx;
        const record_data = self.layouts().getStructData(record_idx);
        const fields = self.layouts().struct_fields.sliceRange(record_data.getFields());
        if (fields.len != 3) return error.CompilationFailed;

        if (self.layouts().getStructFieldLayoutByOriginalIndex(record_idx, 0) != .str) return error.CompilationFailed;
        if (self.layouts().getStructFieldLayoutByOriginalIndex(record_idx, 1) != .str) return error.CompilationFailed;
        if (self.layouts().getStructFieldLayoutByOriginalIndex(record_idx, 2) != .bool) return error.CompilationFailed;

        return .{
            .after_offset = self.layouts().getStructFieldOffsetByOriginalIndex(record_idx, 0),
            .before_offset = self.layouts().getStructFieldOffsetByOriginalIndex(record_idx, 1),
            .found_offset = self.layouts().getStructFieldOffsetByOriginalIndex(record_idx, 2),
        };
    }

    fn resolveStrDropPrefixCaselessAsciiLayout(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) Error!StrDropPrefixCaselessAsciiLayoutInfo {
        const ret_layout_val = self.layoutValue(layout_idx);
        if (ret_layout_val.tag != .struct_) return error.CompilationFailed;

        const record_idx = ret_layout_val.getStruct().idx;
        const record_data = self.layouts().getStructData(record_idx);
        const fields = self.layouts().struct_fields.sliceRange(record_data.getFields());
        if (fields.len != 2) return error.CompilationFailed;

        if (self.layouts().getStructFieldLayoutByOriginalIndex(record_idx, 0) != .str) return error.CompilationFailed;
        if (self.layouts().getStructFieldLayoutByOriginalIndex(record_idx, 1) != .bool) return error.CompilationFailed;

        return .{
            .after_offset = self.layouts().getStructFieldOffsetByOriginalIndex(record_idx, 0),
            .found_offset = self.layouts().getStructFieldOffsetByOriginalIndex(record_idx, 1),
        };
    }

    fn unwrapSingleFieldPayloadLayout(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) ?layout.Idx {
        const layout_val = self.layoutValue(layout_idx);
        if (layout_val.tag != .struct_) return null;

        const struct_data = self.layouts().getStructData(layout_val.getStruct().idx);
        const fields = self.layouts().struct_fields.sliceRange(struct_data.getFields());
        if (fields.len != 1) return null;

        const field = fields.get(0);
        if (field.index != 0) return null;
        return field.layout;
    }

    fn findBadUtf8Variant(self: *MonoLlvmCodeGen, inner_tu: *const layout.TagUnionData) ?struct { disc: u16, struct_idx: layout.StructIdx } {
        const variants = self.layouts().getTagUnionVariants(inner_tu);
        for (0..variants.len) |i| {
            const payload = variants.get(@intCast(i)).payload_layout;
            const candidate = self.unwrapSingleFieldPayloadLayout(payload) orelse payload;
            const payload_layout = self.layoutValue(candidate);
            if (payload_layout.tag != .struct_) continue;

            const struct_idx = payload_layout.getStruct().idx;
            const struct_data = self.layouts().getStructData(struct_idx);
            const fields = self.layouts().struct_fields.sliceRange(struct_data.getFields());
            if (fields.len != 2) continue;

            var has_index_field = false;
            var has_problem_field = false;
            for (0..fields.len) |field_i| {
                const field = fields.get(field_i);
                const field_size = self.layoutByteSize(field.layout);
                switch (field.index) {
                    0 => has_index_field = field_size == 8,
                    1 => has_problem_field = field_size == 1,
                    else => {},
                }
            }
            if (has_index_field and has_problem_field) {
                return .{ .disc = @intCast(i), .struct_idx = struct_idx };
            }
        }

        return null;
    }

    fn resolveStructBase(self: *MonoLlvmCodeGen, source: LocalId) Error!ResolvedBase {
        const source_layout = self.localLayout(source);
        const layout_val = self.layoutValue(source_layout);
        if (layout_val.tag == .box) {
            return .{ .ptr = try self.loadPointer(self.slot(source).ptr), .layout_idx = layout_val.getIdx() };
        } else if (layout_val.tag == .box_of_zst) {
            return .{ .ptr = self.slot(source).ptr, .layout_idx = .zst };
        } else {
            return .{ .ptr = self.slot(source).ptr, .layout_idx = source_layout };
        }
    }

    fn resolveTagBase(self: *MonoLlvmCodeGen, source: LocalId) Error!ResolvedBase {
        return self.resolveStructBase(source);
    }

    fn rocStrArgFields(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value) Error![3]LlvmBuilder.Value {
        return .{
            try self.loadPointer(ptr),
            try self.loadUsize(try self.offsetPtr(ptr, self.rocStrLenOffset())),
            try self.loadUsize(try self.offsetPtr(ptr, self.rocStrCapacityOffset())),
        };
    }

    fn rocListArgFields(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value) Error![3]LlvmBuilder.Value {
        return .{
            try self.loadPointer(ptr),
            try self.loadUsize(try self.offsetPtr(ptr, self.rocListLenOffset())),
            try self.loadUsize(try self.offsetPtr(ptr, self.rocListCapacityOffset())),
        };
    }

    fn storeListFields(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, bytes: LlvmBuilder.Value, len: usize, cap: usize) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        try self.storePointer(ptr, bytes);
        try self.storeListLen(ptr, builder.intValue(self.ptrSizedIntType(), len) catch return error.OutOfMemory);
        try self.storeListCapacity(ptr, builder.intValue(self.ptrSizedIntType(), cap) catch return error.OutOfMemory);
    }

    fn entrypointParamSlotSize(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) u32 {
        const runtime_layout_idx = self.layouts().runtimeRepresentationLayoutIdx(layout_idx);
        if (runtime_layout_idx == .str) return 3 * self.targetWordSize();
        if (runtime_layout_idx == .i128 or runtime_layout_idx == .u128 or runtime_layout_idx == .dec) return 16;
        const layout_val = self.layoutValue(runtime_layout_idx);
        const size = self.layoutByteSize(runtime_layout_idx);
        if (layout_val.tag == .zst or size == 0) return 0;
        // Internal Roc procedure arguments use fixed-size slots in a shared
        // byte buffer. A vector occupies its full 16-byte value; rounding it
        // down to the ordinary scalar slot size makes the following argument
        // overlap the vector's upper half.
        if (layout_val.tag == .scalar and layout_val.getScalar().tag == .vector) return 16;
        if (layout_val.tag == .list or layout_val.tag == .list_of_zst) return 24;
        if (layout_val.tag == .struct_ or layout_val.tag == .tag_union) {
            if (size > 8) return @intCast(std.mem.alignForward(u32, size, 8));
        }
        return 8;
    }

    fn computeArgOffsets(self: *MonoLlvmCodeGen, arg_layouts: []const layout.Idx, rounded_slots: bool) Error![]u32 {
        const ordered = try self.allocator.alloc(ArgOrder, arg_layouts.len);
        defer self.allocator.free(ordered);
        const offsets = try self.allocator.alloc(u32, arg_layouts.len);
        for (arg_layouts, 0..) |arg_layout, i| {
            const sa = self.sizeAlignOf(arg_layout);
            ordered[i] = .{
                .index = i,
                .alignment = @intCast(@max(sa.alignment.toByteUnits(), 1)),
                .size = if (rounded_slots) self.entrypointParamSlotSize(arg_layout) else sa.size,
            };
        }
        const SortCtx = struct {
            fn lessThan(_: void, lhs: ArgOrder, rhs: ArgOrder) bool {
                if (lhs.alignment != rhs.alignment) return lhs.alignment > rhs.alignment;
                return lhs.index < rhs.index;
            }
        };
        std.mem.sort(ArgOrder, ordered, {}, SortCtx.lessThan);
        var current: u32 = 0;
        for (ordered) |arg| {
            current = std.mem.alignForward(u32, current, arg.alignment);
            offsets[arg.index] = current;
            current += arg.size;
        }
        return offsets;
    }

    fn argBufferSize(self: *MonoLlvmCodeGen, arg_layouts: []const layout.Idx, rounded_slots: bool) Error!u32 {
        if (arg_layouts.len == 0) return 8;
        const offsets = try self.computeArgOffsets(arg_layouts, rounded_slots);
        defer self.allocator.free(offsets);
        var total: u32 = 0;
        for (arg_layouts, offsets) |arg_layout, offset| {
            const slot_size = if (rounded_slots) self.entrypointParamSlotSize(arg_layout) else self.layoutByteSize(arg_layout);
            total = @max(total, offset + slot_size);
        }
        return @max(total, 8);
    }

    fn allocArgBuffer(self: *MonoLlvmCodeGen, arg_layouts: []const layout.Idx, rounded_slots: bool) Error!LlvmBuilder.Value {
        const size = try self.argBufferSize(arg_layouts, rounded_slots);
        const ptr = try self.allocEntryBlockSlot(.i8, size, LlvmBuilder.Alignment.fromByteUnits(16), "args");
        try self.zeroBytes(ptr, size);
        return ptr;
    }

    fn copyEntrypointArgsToInternalBuffer(self: *MonoLlvmCodeGen, src_args: LlvmBuilder.Value, dst_args: LlvmBuilder.Value, arg_layouts: []const layout.Idx) Error!void {
        const offsets = try self.computeArgOffsets(arg_layouts, true);
        defer self.allocator.free(offsets);
        for (arg_layouts, offsets) |arg_layout, offset| {
            const size = self.layoutByteSize(arg_layout);
            if (size == 0) continue;
            try self.copyBytes(try self.offsetPtr(dst_args, offset), try self.offsetPtr(src_args, offset), size, self.alignmentForLayout(arg_layout));
        }
    }

    fn packRocArgsFromLocals(self: *MonoLlvmCodeGen, dst_args: LlvmBuilder.Value, arg_locals: anytype, arg_layouts: []const layout.Idx) Error!void {
        const offsets = try self.computeArgOffsets(arg_layouts, true);
        defer self.allocator.free(offsets);
        for (0..arg_locals.len) |i| {
            const arg = GuardedList.at(arg_locals, i);
            const arg_layout = arg_layouts[i];
            const offset = offsets[i];
            try self.materializeLocalIfDeferred(arg);
            const size = self.layoutByteSize(arg_layout);
            if (size == 0) continue;
            try self.copyBytes(try self.offsetPtr(dst_args, offset), self.slot(arg).ptr, size, self.alignmentForLayout(arg_layout));
        }
    }

    fn packErasedArgsFromLocals(
        self: *MonoLlvmCodeGen,
        dst_args: LlvmBuilder.Value,
        arg_locals: anytype,
        arg_layouts: []const layout.Idx,
        offsets: anytype,
    ) Error!void {
        for (0..arg_locals.len) |i| {
            const arg = GuardedList.at(arg_locals, i);
            const arg_layout = arg_layouts[i];
            try self.materializeLocalIfDeferred(arg);
            const sa = self.sizeAlignOf(arg_layout);
            if (sa.size > 0) {
                try self.copyBytes(try self.offsetPtr(dst_args, GuardedList.at(offsets, i)), self.slot(arg).ptr, sa.size, self.alignmentForLayout(arg_layout));
            }
        }
    }

    /// The C-ABI target this build is compiling for.
    fn abiTarget(self: *const MonoLlvmCodeGen) layout.abi.Target {
        const arch = self.target.cpu.arch;
        if (arch == .aarch64 or arch == .aarch64_be) {
            return layout.abi.aarch64Target(self.target.os.tag);
        } else if (arch == .x86_64) {
            return if (self.target.os.tag == .windows) .x86_64_windows else .x86_64_sysv;
        } else if (arch == .wasm32) {
            return .wasm32;
        } else if (arch == .wasm64) {
            return .wasm64;
        } else {
            std.debug.panic("hosted C-ABI calls are not supported for arch {s}", .{@tagName(arch)});
        }
    }

    /// The LLVM type carrying one register piece of a value.
    fn pieceLlvmType(builder: *LlvmBuilder, piece: layout.abi.RegPiece) Error!LlvmBuilder.Type {
        return switch (piece.class) {
            .integer => builder.intType(@as(u24, piece.size) * 8) catch return error.OutOfMemory,
            .float => switch (piece.size) {
                2 => .half,
                4 => .float,
                8 => .double,
                else => .fp128,
            },
            .vector => vectorLlvmType(builder, piece.vector_kind orelse return error.CompilationFailed),
        };
    }

    fn vectorLlvmType(builder: *LlvmBuilder, vector: layout.Vector) Error!LlvmBuilder.Type {
        const lane_ty = builder.intType(vector.laneBits()) catch return error.OutOfMemory;
        return builder.vectorType(.normal, vector.laneCount(), lane_ty) catch return error.OutOfMemory;
    }

    /// The LLVM C-ABI carrier type for a register piece. LLVM IR has sub-i32
    /// integers, but WebAssembly's C ABI does not: narrow integer parameters
    /// and returns travel as i32 values with explicit extension/truncation at
    /// the ABI boundary.
    fn cAbiPieceLlvmType(self: *MonoLlvmCodeGen, builder: *LlvmBuilder, piece: layout.abi.RegPiece) Error!LlvmBuilder.Type {
        if ((self.abiTarget() == .wasm32 or self.abiTarget() == .wasm64) and
            piece.class == .integer and piece.size < 4)
        {
            return .i32;
        }
        return pieceLlvmType(builder, piece);
    }

    fn cAbiPieceIsSigned(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) bool {
        const abi_target = self.abiTarget();
        const direct_idx = if (abi_target == .wasm32 or abi_target == .wasm64)
            switch (layout.abi.wasm.classifyType(self.layouts(), layout_idx)) {
                .direct => |idx| idx,
                .indirect => layout_idx,
            }
        else
            layout_idx;

        const lay = self.layoutValue(direct_idx);
        return switch (lay.tag) {
            .scalar => switch (lay.getScalar().tag) {
                .int => direct_idx.isSigned(),
                .frac => direct_idx == .dec,
                .vector, .opaque_ptr, .str => false,
            },
            .tag_union, .box, .box_of_zst, .erased_box, .ptr => false,
            .list, .list_of_zst, .struct_, .closure, .erased_callable, .zst => direct_idx.isSigned(),
        };
    }

    /// Convert between a value's natural LLVM register-piece type and the
    /// carrier type required by the C ABI. Homogeneous AArch64 vector
    /// aggregates use the first member's vector shape for every element of
    /// their LLVM array carrier, even when the source aggregate mixes lane
    /// shapes. Those conversions preserve all 128 bits; they are bitcasts, not
    /// lane-wise integer extensions or truncations.
    fn coerceCAbiPiece(
        self: *MonoLlvmCodeGen,
        value: LlvmBuilder.Value,
        target_ty: LlvmBuilder.Type,
        piece: layout.abi.RegPiece,
        signed: bool,
    ) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        if (value.typeOfWip(wip) == target_ty) return value;
        if (piece.class == .vector) {
            return wip.cast(.bitcast, value, target_ty, "") catch return error.OutOfMemory;
        }
        return self.coerceScalar(value, target_ty, signed);
    }

    /// The one LLVM value type that preserves an ABI register placement's
    /// required grouping. Piecewise arguments are handled separately because
    /// each piece is its own parameter; aggregate returns use `structure` even
    /// when they contain only one register piece.
    fn cAbiRegisterCarrierType(
        self: *MonoLlvmCodeGen,
        builder: *LlvmBuilder,
        registers: layout.abi.RegisterPlacement,
    ) Error!LlvmBuilder.Type {
        return switch (registers.carrier) {
            .piecewise => {
                if (registers.pieces.len == 1) {
                    return self.cAbiPieceLlvmType(builder, registers.pieces[0]);
                }
                const field_types = try self.allocator.alloc(LlvmBuilder.Type, registers.pieces.len);
                defer self.allocator.free(field_types);
                for (registers.pieces, field_types) |piece, *field_ty| {
                    field_ty.* = try self.cAbiPieceLlvmType(builder, piece);
                }
                return builder.structType(.normal, field_types) catch return error.OutOfMemory;
            },
            .structure => {
                const field_types = try self.allocator.alloc(LlvmBuilder.Type, registers.pieces.len);
                defer self.allocator.free(field_types);
                for (registers.pieces, field_types) |piece, *field_ty| {
                    field_ty.* = try self.cAbiPieceLlvmType(builder, piece);
                }
                return builder.structType(.normal, field_types) catch return error.OutOfMemory;
            },
            .integer => {
                var byte_size: u16 = 0;
                for (registers.pieces) |piece| byte_size = @max(byte_size, piece.offset + piece.size);
                return builder.intType(@as(u24, byte_size) * 8) catch return error.OutOfMemory;
            },
            .array => {
                std.debug.assert(registers.pieces.len > 0);
                const elem_ty = try self.cAbiPieceLlvmType(builder, registers.pieces[0]);
                return builder.arrayType(registers.pieces.len, elem_ty) catch return error.OutOfMemory;
            },
        };
    }

    fn appendCAbiRegisterParamTypes(
        self: *MonoLlvmCodeGen,
        builder: *LlvmBuilder,
        attrs: *LlvmBuilder.FunctionAttributes.Wip,
        param_types: *std.ArrayList(LlvmBuilder.Type),
        registers: layout.abi.RegisterPlacement,
    ) Error!void {
        switch (registers.carrier) {
            .piecewise => for (registers.pieces) |piece| {
                try param_types.append(self.allocator, try self.cAbiPieceLlvmType(builder, piece));
            },
            .structure, .integer, .array => {
                if (registers.carrier == .array) {
                    if (registers.carrier.array) |alignment| {
                        try attrs.addParamAttr(param_types.items.len, .{
                            .alignstack = .wrap(LlvmBuilder.Alignment.fromByteUnits(alignment)),
                        }, builder);
                    }
                }
                try param_types.append(self.allocator, try self.cAbiRegisterCarrierType(builder, registers));
            },
        }
    }

    fn storeCAbiRegisterParam(
        self: *MonoLlvmCodeGen,
        builder: *LlvmBuilder,
        registers: layout.abi.RegisterPlacement,
        param_cursor: *u32,
        dst: LlvmBuilder.Value,
        arg_layout: layout.Idx,
    ) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        switch (registers.carrier) {
            .piecewise => for (registers.pieces) |piece| {
                const val = wip.arg(param_cursor.*);
                param_cursor.* += 1;
                const piece_dst = try self.offsetPtr(dst, piece.offset);
                const store_val = try self.coerceCAbiPiece(val, try pieceLlvmType(builder, piece), piece, self.cAbiPieceIsSigned(arg_layout));
                _ = wip.store(.normal, store_val, piece_dst, self.alignmentForLayoutOffset(arg_layout, piece.offset)) catch return error.OutOfMemory;
            },
            .integer => {
                const val = wip.arg(param_cursor.*);
                param_cursor.* += 1;
                const piece_dst = try self.offsetPtr(dst, registers.pieces[0].offset);
                _ = wip.store(.normal, val, piece_dst, self.alignmentForLayoutOffset(arg_layout, registers.pieces[0].offset)) catch return error.OutOfMemory;
            },
            .structure, .array => {
                const aggregate = wip.arg(param_cursor.*);
                param_cursor.* += 1;
                for (registers.pieces, 0..) |piece, i| {
                    const field = wip.extractValue(aggregate, &.{@intCast(i)}, "") catch return error.OutOfMemory;
                    const piece_dst = try self.offsetPtr(dst, piece.offset);
                    const store_val = try self.coerceCAbiPiece(field, try pieceLlvmType(builder, piece), piece, self.cAbiPieceIsSigned(arg_layout));
                    _ = wip.store(.normal, store_val, piece_dst, self.alignmentForLayoutOffset(arg_layout, piece.offset)) catch return error.OutOfMemory;
                }
            },
        }
    }

    fn appendCAbiRegisterCallArg(
        self: *MonoLlvmCodeGen,
        builder: *LlvmBuilder,
        attrs: *LlvmBuilder.FunctionAttributes.Wip,
        param_types: *std.ArrayList(LlvmBuilder.Type),
        call_args: *std.ArrayList(LlvmBuilder.Value),
        registers: layout.abi.RegisterPlacement,
        arg_ptr: LlvmBuilder.Value,
        arg_layout: layout.Idx,
    ) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        switch (registers.carrier) {
            .piecewise => for (registers.pieces) |piece| {
                const piece_ty = try pieceLlvmType(builder, piece);
                const src = try self.offsetPtr(arg_ptr, piece.offset);
                const val = wip.load(.normal, piece_ty, src, self.alignmentForLayoutOffset(arg_layout, piece.offset), "") catch return error.OutOfMemory;
                const carrier_ty = try self.cAbiPieceLlvmType(builder, piece);
                // A narrow C scalar owes its argument register a promotion, and
                // LLVM performs it only for a parameter marked with it. A
                // widened carrier already carries the promotion in its own
                // conversion below.
                if (carrier_ty == piece_ty) switch (piece.extend) {
                    .none => {},
                    .zero => try attrs.addParamAttr(param_types.items.len, .zeroext, builder),
                    .sign => try attrs.addParamAttr(param_types.items.len, .signext, builder),
                };
                try param_types.append(self.allocator, carrier_ty);
                try call_args.append(self.allocator, try self.coerceCAbiPiece(val, carrier_ty, piece, self.cAbiPieceIsSigned(arg_layout)));
            },
            .integer => {
                const carrier_ty = try self.cAbiRegisterCarrierType(builder, registers);
                const src = try self.offsetPtr(arg_ptr, registers.pieces[0].offset);
                const val = wip.load(.normal, carrier_ty, src, self.alignmentForLayoutOffset(arg_layout, registers.pieces[0].offset), "") catch return error.OutOfMemory;
                try param_types.append(self.allocator, carrier_ty);
                try call_args.append(self.allocator, val);
            },
            .structure => {
                const carrier_ty = try self.cAbiRegisterCarrierType(builder, registers);
                var aggregate = builder.poisonValue(carrier_ty) catch return error.OutOfMemory;
                for (registers.pieces, 0..) |piece, i| {
                    const piece_ty = try pieceLlvmType(builder, piece);
                    const src = try self.offsetPtr(arg_ptr, piece.offset);
                    const val = wip.load(.normal, piece_ty, src, self.alignmentForLayoutOffset(arg_layout, piece.offset), "") catch return error.OutOfMemory;
                    const field_ty = try self.cAbiPieceLlvmType(builder, piece);
                    const field = try self.coerceCAbiPiece(val, field_ty, piece, self.cAbiPieceIsSigned(arg_layout));
                    aggregate = wip.insertValue(aggregate, field, &.{@intCast(i)}, "") catch return error.OutOfMemory;
                }
                try param_types.append(self.allocator, carrier_ty);
                try call_args.append(self.allocator, aggregate);
            },
            .array => {
                if (registers.carrier.array) |alignment| {
                    try attrs.addParamAttr(param_types.items.len, .{
                        .alignstack = .wrap(LlvmBuilder.Alignment.fromByteUnits(alignment)),
                    }, builder);
                }
                const carrier_ty = try self.cAbiRegisterCarrierType(builder, registers);
                var aggregate = builder.poisonValue(carrier_ty) catch return error.OutOfMemory;
                const array_elem_ty = try self.cAbiPieceLlvmType(builder, registers.pieces[0]);
                for (registers.pieces, 0..) |piece, i| {
                    const piece_ty = try pieceLlvmType(builder, piece);
                    const src = try self.offsetPtr(arg_ptr, piece.offset);
                    const val = wip.load(.normal, piece_ty, src, self.alignmentForLayoutOffset(arg_layout, piece.offset), "") catch return error.OutOfMemory;
                    const field = try self.coerceCAbiPiece(val, array_elem_ty, piece, self.cAbiPieceIsSigned(arg_layout));
                    aggregate = wip.insertValue(aggregate, field, &.{@intCast(i)}, "") catch return error.OutOfMemory;
                }
                try param_types.append(self.allocator, carrier_ty);
                try call_args.append(self.allocator, aggregate);
            },
        }
    }

    fn storeCAbiRegisterResult(
        self: *MonoLlvmCodeGen,
        builder: *LlvmBuilder,
        registers: layout.abi.RegisterPlacement,
        result: LlvmBuilder.Value,
        ret_ptr: LlvmBuilder.Value,
        ret_layout: layout.Idx,
    ) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        switch (registers.carrier) {
            .integer => {
                const dst = try self.offsetPtr(ret_ptr, registers.pieces[0].offset);
                _ = wip.store(.normal, result, dst, self.alignmentForLayoutOffset(ret_layout, registers.pieces[0].offset)) catch return error.OutOfMemory;
            },
            .piecewise, .structure, .array => {
                if (registers.carrier == .piecewise and registers.pieces.len == 1) {
                    const piece = registers.pieces[0];
                    const dst = try self.offsetPtr(ret_ptr, piece.offset);
                    const store_val = try self.coerceCAbiPiece(result, try pieceLlvmType(builder, piece), piece, self.cAbiPieceIsSigned(ret_layout));
                    _ = wip.store(.normal, store_val, dst, self.alignmentForLayoutOffset(ret_layout, piece.offset)) catch return error.OutOfMemory;
                    return;
                }
                for (registers.pieces, 0..) |piece, i| {
                    const field = wip.extractValue(result, &.{@intCast(i)}, "") catch return error.OutOfMemory;
                    const dst = try self.offsetPtr(ret_ptr, piece.offset);
                    const store_val = try self.coerceCAbiPiece(field, try pieceLlvmType(builder, piece), piece, self.cAbiPieceIsSigned(ret_layout));
                    _ = wip.store(.normal, store_val, dst, self.alignmentForLayoutOffset(ret_layout, piece.offset)) catch return error.OutOfMemory;
                }
            },
        }
    }

    fn loadCAbiRegisterResult(
        self: *MonoLlvmCodeGen,
        builder: *LlvmBuilder,
        registers: layout.abi.RegisterPlacement,
        ret_ptr: LlvmBuilder.Value,
        ret_layout: layout.Idx,
    ) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const carrier_ty = try self.cAbiRegisterCarrierType(builder, registers);
        switch (registers.carrier) {
            .integer => {
                const src = try self.offsetPtr(ret_ptr, registers.pieces[0].offset);
                return wip.load(.normal, carrier_ty, src, self.alignmentForLayoutOffset(ret_layout, registers.pieces[0].offset), "") catch return error.OutOfMemory;
            },
            .piecewise, .structure, .array => {
                if (registers.carrier == .piecewise and registers.pieces.len == 1) {
                    const piece = registers.pieces[0];
                    const src = try self.offsetPtr(ret_ptr, piece.offset);
                    const val = wip.load(.normal, try pieceLlvmType(builder, piece), src, self.alignmentForLayoutOffset(ret_layout, piece.offset), "") catch return error.OutOfMemory;
                    return self.coerceCAbiPiece(val, carrier_ty, piece, self.cAbiPieceIsSigned(ret_layout));
                }
                var aggregate = builder.poisonValue(carrier_ty) catch return error.OutOfMemory;
                const array_elem_ty: ?LlvmBuilder.Type = if (registers.carrier == .array)
                    try self.cAbiPieceLlvmType(builder, registers.pieces[0])
                else
                    null;
                for (registers.pieces, 0..) |piece, i| {
                    const src = try self.offsetPtr(ret_ptr, piece.offset);
                    const val = wip.load(.normal, try pieceLlvmType(builder, piece), src, self.alignmentForLayoutOffset(ret_layout, piece.offset), "") catch return error.OutOfMemory;
                    const field_ty = array_elem_ty orelse try self.cAbiPieceLlvmType(builder, piece);
                    const field = try self.coerceCAbiPiece(val, field_ty, piece, self.cAbiPieceIsSigned(ret_layout));
                    aggregate = wip.insertValue(aggregate, field, &.{@intCast(i)}, "") catch return error.OutOfMemory;
                }
                return aggregate;
            },
        }
    }

    /// A type with the exact size and alignment of `layout_idx`, for a `byval`/`sret`
    /// pointer parameter (LLVM derives the memory convention and alignment from it).
    fn memoryLlvmTypeForLayout(self: *MonoLlvmCodeGen, builder: *LlvmBuilder, layout_idx: layout.Idx) Error!LlvmBuilder.Type {
        const sa = self.sizeAlignOf(layout_idx);
        const align_bytes: u32 = @intCast(sa.alignment.toByteUnits());
        const elem = builder.intType(@intCast(align_bytes * 8)) catch return error.OutOfMemory;
        return builder.arrayType(sa.size / align_bytes, elem) catch return error.OutOfMemory;
    }

    fn hostedIndirectArgUsesByval(self: *const MonoLlvmCodeGen) bool {
        return self.abiTarget() == .x86_64_sysv;
    }

    fn emitDefaultPlatformHostedCall(
        self: *MonoLlvmCodeGen,
        hosted: lir.LIR.HostedProc,
        arg_ptrs: []const LlvmBuilder.Value,
        arg_layouts: []const layout.Idx,
        ret_layout: layout.Idx,
    ) Error!bool {
        if (!self.enable_default_platform_hosted_calls) return false;
        if (self.host_call_mode != .extern_symbols) return false;
        if (!std.mem.eql(u8, self.store.getString(hosted.symbol), shim_symbols.roc_default_echo_line)) return false;

        switch (self.target.os.tag) {
            .linux, .macos, .windows, .freebsd, .netbsd => {},
            .freestanding, .other, .contiki, .fuchsia, .hermit, .managarm, .haiku, .hurd, .illumos, .plan9, .rtems, .serenity, .dragonfly, .openbsd, .driverkit, .ios, .maccatalyst, .tvos, .visionos, .watchos, .uefi, .@"3ds", .ps3, .ps4, .ps5, .psp, .vita, .emscripten, .wasi, .amdhsa, .amdpal, .cuda, .mesa3d, .nvcl, .opencl, .opengl, .vulkan => return error.CompilationFailed,
        }
        if (arg_ptrs.len != 1 or arg_layouts.len != 1 or arg_layouts[0] != .str or ret_layout != .zst) {
            return error.CompilationFailed;
        }

        try self.emitDefaultPlatformWrite(arg_ptrs[0]);

        return true;
    }

    fn emitDefaultPlatformWrite(self: *MonoLlvmCodeGen, str_ptr: LlvmBuilder.Value) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const usize_ty = self.ptrSizedIntType();

        const raw_len = try self.loadUsize(try self.offsetPtr(str_ptr, self.rocStrLenOffset()));
        const is_small = wip.icmp(.slt, raw_len, builder.intValue(usize_ty, 0) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const small_block = wip.block(0, "echo_small_str") catch return error.OutOfMemory;
        const heap_block = wip.block(0, "echo_heap_str") catch return error.OutOfMemory;
        const after = wip.block(0, "echo_after") catch return error.OutOfMemory;
        _ = wip.brCond(is_small, small_block, heap_block, .then_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = small_block };
        const last_byte_offset = self.targetWordSize() * 3 - 1;
        const last_byte = wip.load(.normal, .i8, try self.offsetPtr(str_ptr, last_byte_offset), LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
        const small_len_byte = wip.bin(.@"and", last_byte, builder.intValue(.i8, 0x7f) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const small_len = try self.coerceScalar(small_len_byte, usize_ty, false);
        try self.emitDefaultPlatformWriteStdout(str_ptr, small_len);
        _ = wip.br(after) catch return error.OutOfMemory;

        wip.cursor = .{ .block = heap_block };
        const big_ptr = try self.loadPointer(str_ptr);
        try self.emitDefaultPlatformWriteStdout(big_ptr, raw_len);
        _ = wip.br(after) catch return error.OutOfMemory;

        wip.cursor = .{ .block = after };
    }

    fn emitDefaultPlatformWriteStdout(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, len: LlvmBuilder.Value) Error!void {
        switch (self.target.os.tag) {
            .linux => try self.emitLinuxWriteStdout(ptr, len),
            .freebsd, .netbsd => try self.emitX86_64BsdWriteStdout(ptr, len),
            .macos, .windows => try self.emitCWriteStdout(ptr, len),
            .freestanding, .other, .contiki, .fuchsia, .hermit, .managarm, .haiku, .hurd, .illumos, .plan9, .rtems, .serenity, .dragonfly, .openbsd, .driverkit, .ios, .maccatalyst, .tvos, .visionos, .watchos, .uefi, .@"3ds", .ps3, .ps4, .ps5, .psp, .vita, .emscripten, .wasi, .amdhsa, .amdpal, .cuda, .mesa3d, .nvcl, .opencl, .opengl, .vulkan => return error.CompilationFailed,
        }
    }

    fn emitCWriteStdout(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, len: LlvmBuilder.Value) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const ptr_ty = try self.ptrType();
        const len_ty: LlvmBuilder.Type = if (self.target.os.tag == .windows) .i32 else self.ptrSizedIntType();
        const ret_ty: LlvmBuilder.Type = if (self.target.os.tag == .windows) .i32 else self.ptrSizedIntType();
        const symbol = if (self.target.os.tag == .windows) "_write" else "write";
        const fn_ty = builder.fnType(ret_ty, &.{ .i32, ptr_ty, len_ty }, .normal) catch return error.OutOfMemory;
        const write_fn = try self.declareExternSymbol(symbol, fn_ty);

        _ = wip.call(
            .normal,
            .ccc,
            .none,
            fn_ty,
            write_fn.toValue(builder),
            &.{
                builder.intValue(.i32, 1) catch return error.OutOfMemory,
                ptr,
                try self.coerceScalar(len, len_ty, false),
            },
            "",
        ) catch return error.OutOfMemory;
    }

    fn emitLinuxWriteStdout(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, len: LlvmBuilder.Value) Error!void {
        if (self.target.cpu.arch == .x86_64) {
            try self.emitX86_64LinuxWriteStdout(ptr, len);
        } else if (self.target.cpu.arch == .aarch64) {
            try self.emitAarch64LinuxWriteStdout(ptr, len);
        } else {
            return error.CompilationFailed;
        }
    }

    fn emitX86_64LinuxWriteStdout(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, len: LlvmBuilder.Value) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const usize_ty = self.ptrSizedIntType();
        const fn_ty = builder.fnType(.i64, &.{ .i64, .i64, try self.ptrType(), usize_ty }, .normal) catch return error.OutOfMemory;

        _ = wip.callAsm(
            .none,
            fn_ty,
            .{ .sideeffect = true },
            builder.string("syscall") catch return error.OutOfMemory,
            builder.string("={rax},{rax},{rdi},{rsi},{rdx},~{rcx},~{r11},~{memory}") catch return error.OutOfMemory,
            &.{
                builder.intValue(.i64, 1) catch return error.OutOfMemory,
                builder.intValue(.i64, 1) catch return error.OutOfMemory,
                ptr,
                len,
            },
            "",
        ) catch return error.OutOfMemory;
    }

    fn emitX86_64BsdWriteStdout(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, len: LlvmBuilder.Value) Error!void {
        if (self.target.cpu.arch != .x86_64) return error.CompilationFailed;

        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const usize_ty = self.ptrSizedIntType();
        const fn_ty = builder.fnType(.i64, &.{ .i64, .i64, try self.ptrType(), usize_ty }, .normal) catch return error.OutOfMemory;

        _ = wip.callAsm(
            .none,
            fn_ty,
            .{ .sideeffect = true },
            builder.string("syscall") catch return error.OutOfMemory,
            builder.string("={rax},{rax},{rdi},{rsi},{rdx},~{rcx},~{r11},~{memory}") catch return error.OutOfMemory,
            &.{
                builder.intValue(.i64, 4) catch return error.OutOfMemory,
                builder.intValue(.i64, 1) catch return error.OutOfMemory,
                ptr,
                len,
            },
            "",
        ) catch return error.OutOfMemory;
    }

    fn emitAarch64LinuxWriteStdout(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, len: LlvmBuilder.Value) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const usize_ty = self.ptrSizedIntType();
        const fn_ty = builder.fnType(usize_ty, &.{ usize_ty, usize_ty, try self.ptrType(), usize_ty }, .normal) catch return error.OutOfMemory;

        _ = wip.callAsm(
            .none,
            fn_ty,
            .{ .sideeffect = true },
            builder.string("svc #0") catch return error.OutOfMemory,
            builder.string("={x0},{x8},{x0},{x1},{x2},~{memory}") catch return error.OutOfMemory,
            &.{
                builder.intValue(usize_ty, 64) catch return error.OutOfMemory,
                builder.intValue(usize_ty, 1) catch return error.OutOfMemory,
                ptr,
                len,
            },
            "",
        ) catch return error.OutOfMemory;
    }

    /// Emit a hosted-function call using the platform C ABI: small arguments and the return
    /// travel in registers per `abi.lower`, with `*RocOps` threaded only when the signature
    /// touches Roc-managed memory. `arg_ptrs` point at each argument's value bytes; the result
    /// is written into `ret_ptr` (used directly as the sret pointer for memory-class returns).
    fn emitHostedCallCAbi(
        self: *MonoLlvmCodeGen,
        hosted: lir.LIR.HostedProc,
        arg_ptrs: []const LlvmBuilder.Value,
        arg_layouts: []const layout.Idx,
        ret_ptr: LlvmBuilder.Value,
        ret_layout: layout.Idx,
    ) Error!void {
        if (try self.emitDefaultPlatformHostedCall(hosted, arg_ptrs, arg_layouts, ret_layout)) {
            return;
        }

        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const ptr_ty = try self.ptrType();

        var arena_state = std.heap.ArenaAllocator.init(self.allocator);
        defer arena_state.deinit();
        const arena = arena_state.allocator();

        // Under the symbol ABI the host reaches its own runtime operations
        // directly, so hosted functions never take a leading *RocOps.
        const needs_ops = self.host_call_mode == .vtable and
            layout.abi.needsRocOps(self.layouts(), arg_layouts, ret_layout);
        const lowered = layout.abi.lower(arena, self.layouts(), self.abiTarget(), arg_layouts, ret_layout, needs_ops) catch return error.OutOfMemory;

        var param_types = std.ArrayList(LlvmBuilder.Type).empty;
        defer param_types.deinit(self.allocator);
        var call_args = std.ArrayList(LlvmBuilder.Value).empty;
        defer call_args.deinit(self.allocator);
        var attrs_wip: LlvmBuilder.FunctionAttributes.Wip = .{};
        defer attrs_wip.deinit(builder);

        // Return value: registers (coerced struct/scalar) or sret pointer.
        var ret_ty: LlvmBuilder.Type = .void;
        var ret_registers: ?layout.abi.RegisterPlacement = null;
        switch (lowered.ret) {
            .none => {},
            .indirect => {
                const r_ty = try self.memoryLlvmTypeForLayout(builder, ret_layout);
                try attrs_wip.addParamAttr(param_types.items.len, .{ .sret = r_ty }, builder);
                try param_types.append(self.allocator, ptr_ty);
                try call_args.append(self.allocator, ret_ptr);
            },
            .registers => |registers| {
                ret_registers = registers;
                ret_ty = try self.cAbiRegisterCarrierType(builder, registers);
            },
        }

        if (lowered.leading_ops) {
            try param_types.append(self.allocator, ptr_ty);
            try call_args.append(self.allocator, self.rocOps());
        }

        // Arguments use the classifier's explicit scalar/array/piecewise
        // carrier. Each memory piece is loaded from its exact byte range;
        // memory-class arguments pass their ABI pointer/byval form.
        for (lowered.args, arg_ptrs, arg_layouts) |placement, arg_ptr, arg_layout| {
            switch (placement) {
                .none => {},
                .indirect => {
                    if (self.hostedIndirectArgUsesByval()) {
                        const a_ty = try self.memoryLlvmTypeForLayout(builder, arg_layout);
                        try attrs_wip.addParamAttr(param_types.items.len, .{ .byval = a_ty }, builder);
                    }
                    try param_types.append(self.allocator, ptr_ty);
                    try call_args.append(self.allocator, arg_ptr);
                },
                .registers => |registers| try self.appendCAbiRegisterCallArg(
                    builder,
                    &attrs_wip,
                    &param_types,
                    &call_args,
                    registers,
                    arg_ptr,
                    arg_layout,
                ),
            }
        }

        const fn_ty = builder.fnType(ret_ty, param_types.items, .normal) catch return error.OutOfMemory;
        const attrs = attrs_wip.finish(builder) catch return error.OutOfMemory;
        const callee = switch (self.host_call_mode) {
            .vtable => blk: {
                const table_ptr_ptr = try self.offsetPtr(self.rocOps(), self.rocOpsHostedFnsPtrOffset());
                const table_ptr = try self.loadPointer(table_ptr_ptr);
                const fn_ptr_ptr = try self.offsetPtr(table_ptr, hosted.dispatch_index * self.targetWordSize());
                break :blk try self.loadPointer(fn_ptr_ptr);
            },
            .extern_symbols => blk: {
                const func = try self.declareHostSymbol(self.store.getString(hosted.symbol), fn_ty);
                break :blk func.toValue(builder);
            },
        };
        const result = wip.call(.normal, .ccc, attrs, fn_ty, callee, call_args.items, "") catch return error.OutOfMemory;

        // Register return: store each piece back into the result slot at its byte offset.
        if (ret_registers) |registers| {
            try self.storeCAbiRegisterResult(builder, registers, result, ret_ptr, ret_layout);
        }
    }

    fn callBuiltin(
        self: *MonoLlvmCodeGen,
        name: []const u8,
        ret_type: LlvmBuilder.Type,
        param_types: []const LlvmBuilder.Type,
        args: []const LlvmBuilder.Value,
    ) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        if (self.cold_depth > 0) {
            // A call on a cold path still clobbers the whole caller-saved
            // register set, which forces the register allocator to spill any
            // loop-carried value alive across it -- in the hot loop, not the
            // cold arm. Routing the call through a noinline preserve_most
            // wrapper moves those saves into the wrapper, so the hot loop
            // keeps its values in registers.
            const shim = try self.declareColdShim(name, ret_type, param_types);
            // An out-parameter builtin must go through callBuiltinOut, whose
            // shims drop the out parameter; a full-signature call against one
            // would corrupt the stack.
            std.debug.assert(!shim.by_value_out);
            return wip.call(.normal, .preserve_mostcc, .none, shim.fn_ty, shim.shim.toValue(builder), args, "") catch return error.OutOfMemory;
        }
        const func = try self.declareBuiltin(name, ret_type, param_types);
        return wip.call(.normal, .ccc, .none, func.typeOf(builder), func.toValue(builder), args, "") catch return error.OutOfMemory;
    }

    fn declareColdShim(self: *MonoLlvmCodeGen, name: []const u8, ret_type: LlvmBuilder.Type, param_types: []const LlvmBuilder.Type) Error!ColdShim {
        const builder = self.builder orelse return error.CompilationFailed;
        if (self.cold_shims.get(name)) |shim| return shim;
        const target = try self.declareBuiltin(name, ret_type, param_types);
        const fn_ty = builder.fnType(ret_type, param_types, .normal) catch return error.OutOfMemory;
        const shim_name = try std.fmt.allocPrint(self.allocator, "roc_cold_shim.{s}", .{name});
        defer self.allocator.free(shim_name);
        const func = builder.addFunction(fn_ty, builder.strtabString(shim_name) catch return error.OutOfMemory, .default) catch return error.OutOfMemory;
        func.setLinkage(.internal, builder);
        func.setCallConv(.preserve_mostcc, builder);
        {
            var attrs_wip: LlvmBuilder.FunctionAttributes.Wip = .{};
            defer attrs_wip.deinit(builder);
            try attrs_wip.addFnAttr(.@"noinline", builder);
            try attrs_wip.addFnAttr(.cold, builder);
            func.setAttributes(attrs_wip.finish(builder) catch return error.OutOfMemory, builder);
        }
        const shim = ColdShim{ .shim = func, .target = target, .fn_ty = fn_ty, .target_fn_ty = fn_ty, .by_value_out = false, .compiled = false };
        try self.cold_shims.put(name, shim);
        return shim;
    }

    /// Emit the body of every cold shim declared while compiling procs: a
    /// plain call to the wrapped builtin and a return. Runs after the procs,
    /// when no function is mid-emission.
    fn compilePendingColdShims(self: *MonoLlvmCodeGen) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        var iter = self.cold_shims.valueIterator();
        while (iter.next()) |shim| {
            if (shim.compiled) continue;
            shim.compiled = true;

            var wip = LlvmBuilder.WipFunction.init(builder, .{ .function = shim.shim, .strip = true }) catch return error.OutOfMemory;
            defer wip.deinit();
            const entry = wip.block(0, "entry") catch return error.OutOfMemory;
            wip.cursor = .{ .block = entry };

            if (shim.by_value_out) {
                // Adapt the builtin's out-parameter ABI inside the shim: give
                // the builtin a slot that is private to this cold function,
                // then hand the three words back by value.
                const slot_ty = try self.listValueType();
                const out_slot = wip.alloca(
                    .normal,
                    slot_ty,
                    builder.intValue(.i32, 1) catch return error.OutOfMemory,
                    LlvmBuilder.Alignment.fromByteUnits(self.targetWordSize()),
                    .default,
                    "out",
                ) catch return error.OutOfMemory;

                const param_count = shim.fn_ty.functionParameters(builder).len;
                const args = try self.allocator.alloc(LlvmBuilder.Value, param_count + 1);
                defer self.allocator.free(args);
                args[0] = out_slot;
                for (args[1..], 0..) |*arg, i| arg.* = wip.arg(@intCast(i));

                _ = wip.call(.normal, .ccc, .none, shim.target_fn_ty, shim.target.toValue(builder), args, "") catch return error.OutOfMemory;

                // `self.wip` is null between procs, so read the slot with
                // this shim's own instruction builder.
                const word = self.targetWordSize();
                const len_ptr = wip.gep(.inbounds, .i8, out_slot, &.{builder.intValue(.i32, word) catch return error.OutOfMemory}, "") catch return error.OutOfMemory;
                const cap_ptr = wip.gep(.inbounds, .i8, out_slot, &.{builder.intValue(.i32, 2 * word) catch return error.OutOfMemory}, "") catch return error.OutOfMemory;
                const fields = [_]LlvmBuilder.Value{
                    wip.load(.normal, try self.ptrType(), out_slot, self.targetPointerAlignment(), "") catch return error.OutOfMemory,
                    wip.load(.normal, self.ptrSizedIntType(), len_ptr, self.targetPointerAlignment(), "") catch return error.OutOfMemory,
                    wip.load(.normal, self.ptrSizedIntType(), cap_ptr, self.targetPointerAlignment(), "") catch return error.OutOfMemory,
                };
                const aggregate = wip.buildAggregate(slot_ty, &fields, "") catch return error.OutOfMemory;
                _ = wip.ret(aggregate) catch return error.OutOfMemory;
                wip.finish() catch return error.OutOfMemory;
                continue;
            }

            const param_count = shim.fn_ty.functionParameters(builder).len;
            const args = try self.allocator.alloc(LlvmBuilder.Value, param_count);
            defer self.allocator.free(args);
            for (args, 0..) |*arg, i| arg.* = wip.arg(@intCast(i));

            const result = wip.call(.normal, .ccc, .none, shim.target.typeOf(builder), shim.target.toValue(builder), args, "") catch return error.OutOfMemory;
            if (shim.fn_ty.functionReturn(builder) == .void) {
                _ = wip.retVoid() catch return error.OutOfMemory;
            } else {
                _ = wip.ret(result) catch return error.OutOfMemory;
            }
            wip.finish() catch return error.OutOfMemory;
        }
    }

    fn callBuiltinVoid(self: *MonoLlvmCodeGen, name: []const u8, param_types: []const LlvmBuilder.Type, args: []const LlvmBuilder.Value) Error!void {
        _ = try self.callBuiltin(name, .void, param_types, args);
    }

    /// Call a void builtin whose first argument is a pointer it writes a
    /// three-word list or string result through. On hot paths this is an
    /// ordinary call: the builtin inlines and the store through the pointer
    /// dissolves. On cold paths the call becomes a shim that returns the
    /// three words by value instead, because handing the out pointer to a
    /// never-inlined call would make the result slot's address escape, and an
    /// escaped slot pins the local to memory for every hot-path update too.
    fn callBuiltinOut(self: *MonoLlvmCodeGen, name: []const u8, param_types: []const LlvmBuilder.Type, args: []const LlvmBuilder.Value) Error!void {
        if (self.cold_depth == 0) {
            _ = try self.callBuiltin(name, .void, param_types, args);
            return;
        }
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const shim = try self.declareColdShimOut(name, param_types);
        if (!shim.by_value_out) {
            // The shim was first declared by a caller that keeps the full
            // out-parameter signature; match it.
            _ = wip.call(.normal, .preserve_mostcc, .none, shim.fn_ty, shim.shim.toValue(builder), args, "") catch return error.OutOfMemory;
            return;
        }
        const result = wip.call(.normal, .preserve_mostcc, .none, shim.fn_ty, shim.shim.toValue(builder), args[1..], "") catch return error.OutOfMemory;
        const out_ptr = args[0];
        try self.storePointer(out_ptr, wip.extractValue(result, &.{0}, "") catch return error.OutOfMemory);
        try self.storeListLen(out_ptr, wip.extractValue(result, &.{1}, "") catch return error.OutOfMemory);
        try self.storeListCapacity(out_ptr, wip.extractValue(result, &.{2}, "") catch return error.OutOfMemory);
    }

    /// Three-word list/string value as an aggregate returned in registers.
    fn listValueType(self: *MonoLlvmCodeGen) Error!LlvmBuilder.Type {
        const builder = self.builder orelse return error.CompilationFailed;
        const usize_ty = self.ptrSizedIntType();
        return builder.structType(.normal, &.{ try self.ptrType(), usize_ty, usize_ty }) catch return error.OutOfMemory;
    }

    fn declareColdShimOut(self: *MonoLlvmCodeGen, name: []const u8, param_types: []const LlvmBuilder.Type) Error!ColdShim {
        const builder = self.builder orelse return error.CompilationFailed;
        if (self.cold_shims.get(name)) |shim| return shim;
        const target = try self.declareBuiltin(name, .void, param_types);
        const target_fn_ty = builder.fnType(.void, param_types, .normal) catch return error.OutOfMemory;
        const fn_ty = builder.fnType(try self.listValueType(), param_types[1..], .normal) catch return error.OutOfMemory;
        const shim_name = try std.fmt.allocPrint(self.allocator, "roc_cold_shim.{s}", .{name});
        defer self.allocator.free(shim_name);
        const func = builder.addFunction(fn_ty, builder.strtabString(shim_name) catch return error.OutOfMemory, .default) catch return error.OutOfMemory;
        func.setLinkage(.internal, builder);
        func.setCallConv(.preserve_mostcc, builder);
        {
            var attrs_wip: LlvmBuilder.FunctionAttributes.Wip = .{};
            defer attrs_wip.deinit(builder);
            try attrs_wip.addFnAttr(.@"noinline", builder);
            try attrs_wip.addFnAttr(.cold, builder);
            func.setAttributes(attrs_wip.finish(builder) catch return error.OutOfMemory, builder);
        }
        const shim = ColdShim{ .shim = func, .target = target, .fn_ty = fn_ty, .target_fn_ty = target_fn_ty, .by_value_out = true, .compiled = false };
        try self.cold_shims.put(name, shim);
        return shim;
    }

    fn declareBuiltin(self: *MonoLlvmCodeGen, name: []const u8, ret_type: LlvmBuilder.Type, param_types: []const LlvmBuilder.Type) Error!LlvmBuilder.Function.Index {
        const builder = self.builder orelse return error.CompilationFailed;
        if (self.builtin_functions.get(name)) |func| return func;
        const fn_ty = builder.fnType(ret_type, param_types, .normal) catch return error.OutOfMemory;
        // Always use the plain builtin name so the IR global matches the symbol in
        // the builtin bitcode linked in for inlining. LLVM still applies the target's
        // symbol mangling (e.g. the macOS leading underscore) when emitting the final
        // object, so non-inlined calls still resolve against roc_builtins.o at link.
        const fn_name = builder.strtabString(name) catch return error.OutOfMemory;
        const func = builder.addFunction(fn_ty, fn_name, .default) catch return error.OutOfMemory;
        try self.builtin_functions.put(name, func);
        return func;
    }

    /// Declare (once) a host-provided function under its literal linker symbol.
    /// Weak linkage breaks the app/host reference cycle: the symbol resolves at
    /// the end of the link against whichever host object defines it.
    fn declareHostSymbol(self: *MonoLlvmCodeGen, name: []const u8, fn_ty: LlvmBuilder.Type) Error!LlvmBuilder.Function.Index {
        const builder = self.builder orelse return error.CompilationFailed;
        if (self.builtin_functions.get(name)) |func| return func;
        const fn_name = builder.strtabString(name) catch return error.OutOfMemory;
        const func = builder.addFunction(fn_ty, fn_name, .default) catch return error.OutOfMemory;
        func.setLinkage(.extern_weak, builder);
        try self.builtin_functions.put(name, func);
        return func;
    }

    /// Declare (once) a strong extern function: interpreter-shim symbols must
    /// pull their archive members at link time, which weak references do not.
    fn declareExternSymbol(self: *MonoLlvmCodeGen, name: []const u8, fn_ty: LlvmBuilder.Type) Error!LlvmBuilder.Function.Index {
        const builder = self.builder orelse return error.CompilationFailed;
        if (self.builtin_functions.get(name)) |func| return func;
        const fn_name = builder.strtabString(name) catch return error.OutOfMemory;
        const func = builder.addFunction(fn_ty, fn_name, .default) catch return error.OutOfMemory;
        try self.builtin_functions.put(name, func);
        return func;
    }

    fn callFunctionIndex(self: *MonoLlvmCodeGen, func: LlvmBuilder.Function.Index, args: []const LlvmBuilder.Value, is_cold: bool) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        if (is_cold) {
            var attrs: LlvmBuilder.FunctionAttributes.Wip = .{};
            defer attrs.deinit(builder);
            try attrs.addFnAttr(.cold, builder);
            try attrs.addFnAttr(.@"noinline", builder);
            return wip.call(.normal, .ccc, attrs.finish(builder) catch return error.OutOfMemory, func.typeOf(builder), func.toValue(builder), args, "") catch return error.OutOfMemory;
        }
        return wip.call(.normal, .ccc, .none, func.typeOf(builder), func.toValue(builder), args, "") catch return error.OutOfMemory;
    }

    fn currentBlockHasTerminator(self: *const MonoLlvmCodeGen) bool {
        const wip = self.wip orelse return false;
        const block = wip.cursor.block.ptrConst(wip);
        if (block.instructions.items.len == 0) return false;
        return block.instructions.items[block.instructions.items.len - 1].isTerminatorWip(wip);
    }

    fn finishCurrentWipFunction(self: *MonoLlvmCodeGen) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;

        for (wip.blocks.items, 0..) |*block, block_idx| {
            if (block.instructions.items.len == 0 or !block.instructions.items[block.instructions.items.len - 1].isTerminatorWip(wip)) {
                wip.cursor = .{ .block = @enumFromInt(block_idx) };
                _ = wip.@"unreachable"() catch return error.OutOfMemory;
            }
        }

        for (wip.blocks.items, 0..) |*block, block_idx| {
            for (block.instructions.items) |instruction| {
                const tag = wip.instructions.get(@intFromEnum(instruction)).tag;
                if (builtin.mode == .Debug and block_idx != 0) {
                    if (tag == .alloca or tag == .@"alloca inalloca") {
                        const builder = self.builder orelse return error.CompilationFailed;
                        const instruction_index = @intFromEnum(instruction);
                        const instruction_name = if (wip.strip)
                            "<stripped>"
                        else
                            (wip.names.items[instruction_index].slice(builder) orelse "<anonymous>");
                        const function_name = wip.function.name(builder).slice(builder) orelse "<anonymous>";
                        std.debug.panic(
                            "LLVM/codegen invariant violated: fixed-lifetime alloca '{s}' emitted in block {d} outside procedure '{s}' entry block",
                            .{ instruction_name, block_idx, function_name },
                        );
                    }
                }
                if ((tag == .phi or tag == .@"phi fast") and block.incoming != block.branches) {
                    return error.CompilationFailed;
                }
            }
            block.incoming = block.branches;
        }

        wip.finish() catch return error.OutOfMemory;
    }

    fn intBits(_: *MonoLlvmCodeGen, layout_idx: layout.Idx) u32 {
        return switch (layout_idx) {
            .bool, .u8, .i8 => 8,
            .u16, .i16 => 16,
            .u32, .i32, .f32 => 32,
            .u64, .i64, .f64, .opaque_ptr => 64,
            .u128, .i128, .dec => 128,
            .str, .zst, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2, _ => 64,
        };
    }
};

fn isFloatLayout(layout_idx: layout.Idx) bool {
    return layout_idx == .f32 or layout_idx == .f64;
}

fn isIntegerLayout(layout_idx: layout.Idx) bool {
    return layout_idx == .bool or layout_idx == .u8 or layout_idx == .i8 or
        layout_idx == .u16 or layout_idx == .i16 or layout_idx == .u32 or
        layout_idx == .i32 or layout_idx == .u64 or layout_idx == .i64 or
        layout_idx == .u128 or layout_idx == .i128;
}

fn checkedOverflowMessage(op: lir.LowLevel) []const u8 {
    return CheckedArithmetic.overflowMessage(op) orelse unreachable;
}

fn checkedZeroDenominatorMessage(op: lir.LowLevel, layout_idx: layout.Idx) []const u8 {
    return CheckedArithmetic.zeroDenominatorMessage(op, layout_idx) orelse unreachable;
}

fn intTypeForBytes(size: u8) LlvmBuilder.Type {
    return switch (size) {
        1 => .i8,
        2 => .i16,
        4 => .i32,
        8 => .i64,
        16 => .i128,
        else => unreachable,
    };
}

fn littleEndianInt(bytes: []const u8) u128 {
    std.debug.assert(bytes.len <= 16);
    var result: u128 = 0;
    for (bytes, 0..) |byte, byte_i| {
        result |= @as(u128, byte) << @intCast(byte_i * 8);
    }
    return result;
}

fn repeatedByte(byte: u8, width: u8) u128 {
    std.debug.assert(width <= 16);
    var result: u128 = 0;
    for (0..width) |byte_i| {
        result |= @as(u128, byte) << @intCast(byte_i * 8);
    }
    return result;
}

test "LLVM erased callable explicit arguments exclude capture and reuse" {
    try std.testing.expectEqual(@as(usize, 3), try MonoLlvmCodeGen.explicitProcParamCount(.erased_callable, 5));
    try std.testing.expectEqual(@as(usize, 5), try MonoLlvmCodeGen.explicitProcParamCount(.roc, 5));
    try std.testing.expectError(error.CompilationFailed, MonoLlvmCodeGen.explicitProcParamCount(.erased_callable, 1));
}
