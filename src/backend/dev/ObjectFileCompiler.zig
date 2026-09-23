//! Object File Compiler for Roc Dev Backend
//!
//! This module orchestrates the compilation of Roc code to native object files
//! using the dev backend. It generates ABI-compliant entrypoint wrappers that
//! can be linked with platform hosts.
//!
//! Supports cross-compilation to any supported RocTarget, not just the host.
//!
//! The compilation pipeline:
//! ```
//! Roc Source → checked modules → post-check IRs → LIR → Machine Code → Object File
//! ```

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;

const layout = @import("layout");
const lir = @import("lir");
const CoreCtx = @import("ctx").CoreCtx;
const LirStore = lir.LirStore;
const LirProcSpec = lir.LirProcSpec;
const RocTarget = @import("roc_target").RocTarget;
const Dwarf = @import("Dwarf.zig");
const coff = @import("object/coff.zig");

const ObjectWriter = @import("ObjectWriter.zig");
const LirCodeGenMod = @import("LirCodeGen.zig");
const ProcArtifact = @import("ProcArtifact.zig");
const NativeProcCompiler = @import("NativeProcCompiler.zig");
const Executor = @import("base").post_check_task_executor.Executor;
const static_data_export = @import("StaticDataExport.zig");
const collections = @import("collections");
const SymbolTable = @import("SymbolTable.zig");
const IndexedRelocation = @import("Relocation.zig").IndexedRelocation;
const StaticStringData = @import("StaticStringData.zig");

/// Information about an entrypoint to compile
pub const Entrypoint = struct {
    /// The exported symbol name (e.g., "roc__main")
    symbol_name: []const u8,
    /// The synthetic LIR proc to invoke for this entrypoint
    proc: lir.LirProcSpecId,
    /// Layouts of the arguments
    arg_layouts: []const layout.Idx,
    /// Layout of the return value
    ret_layout: layout.Idx,
};

pub const StaticDataExport = static_data_export.StaticDataExport;
pub const StaticDataRelocation = static_data_export.StaticDataRelocation;

/// Result of compilation
pub const CompilationResult = struct {
    /// The generated object file bytes
    object_bytes: []const u8,
    /// Allocator used - caller must free object_bytes with this
    allocator: Allocator,
    /// Whether the compiled procs emit boxy runtime calls. When set, the object
    /// references `roc_boxy_*` symbols and its entrypoints call
    /// `roc_boxy_init_embedded`, so the link must include the boxy runtime
    /// object and the embedded sidecar.
    uses_boxy: bool = false,
    /// The compiled program lifted into artifacts, when the compile ran in
    /// pack mode on a target whose artifacts can be extracted.
    artifacts: ?ProcArtifact.Set = null,

    pub fn deinit(self: *CompilationResult) void {
        self.allocator.free(self.object_bytes);
        if (self.artifacts) |*set| set.deinit();
    }
};

/// Errors that can occur during compilation
pub const CompilationError = error{
    OutOfMemory,
    NoEntrypoints,
    CodeGenerationFailed,
    ObjectGenerationFailed,
    UnsupportedTarget,
};

/// Object file compiler that generates object files from LIR.
/// Supports compilation to any RocTarget via runtime-to-comptime dispatch.
pub const ObjectFileCompiler = struct {
    allocator: Allocator,
    enable_default_platform_runtime: bool = false,
    timing: ?*Timing = null,
    post_check_executor: ?Executor = null,
    /// Borrowed artifacts from this exact LIR program, never an executable image.
    reuse_same_program: ?*const NativeProcCompiler.Retained = null,
    /// Emit every procedure as a global symbol and emit the object even
    /// without host entrypoints: the object is a module pack, not an app.
    pack_mode: bool = false,
    /// Where the artifacts of external (object-cache) procedures come from.
    splice_source: ?SpliceSource = null,
    /// Lift the compiled program into artifacts and keep them in
    /// `captured_artifacts` after `compileToObjectFileAndWrite`, so the
    /// program's own pack can be written from the build that produced it.
    capture_artifacts: bool = false,
    captured_artifacts: ?ProcArtifact.Set = null,

    pub const TimingSnapshot = struct {
        backend_setup_ns: u64 = 0,
        /// Native artifact emission, including its RC-helper closure.
        procedure_instructions_ns: u64 = 0,
        /// Kept for snapshot consumers; helpers are timed with procedures.
        rc_helper_instructions_ns: u64 = 0,
        entrypoint_instructions_ns: u64 = 0,
        symbol_relocations_ns: u64 = 0,
        dwarf_ns: u64 = 0,
        object_encoding_ns: u64 = 0,
        file_io_ns: u64 = 0,
        native_emission: NativeProcCompiler.Metrics = .{},
    };

    pub const Timing = struct {
        std_io: std.Io,
        snapshot_value: TimingSnapshot = .{},

        const Phase = enum {
            backend_setup,
            procedure_instructions,
            entrypoint_instructions,
            symbol_relocations,
            dwarf,
            object_encoding,
            file_io,
        };

        pub fn init(std_io: std.Io) Timing {
            return .{ .std_io = std_io };
        }

        pub fn snapshot(self: *const Timing) TimingSnapshot {
            return self.snapshot_value;
        }

        fn start(self: *const Timing) i96 {
            return std.Io.Timestamp.now(self.std_io, .awake).nanoseconds;
        }

        fn finish(self: *Timing, started_ns: i96, phase: Phase) void {
            const finished_ns = std.Io.Timestamp.now(self.std_io, .awake).nanoseconds;
            const elapsed_ns: u64 = @intCast(@max(0, finished_ns - started_ns));
            switch (phase) {
                .backend_setup => self.snapshot_value.backend_setup_ns += elapsed_ns,
                .procedure_instructions => self.snapshot_value.procedure_instructions_ns += elapsed_ns,
                .entrypoint_instructions => self.snapshot_value.entrypoint_instructions_ns += elapsed_ns,
                .symbol_relocations => self.snapshot_value.symbol_relocations_ns += elapsed_ns,
                .dwarf => self.snapshot_value.dwarf_ns += elapsed_ns,
                .object_encoding => self.snapshot_value.object_encoding_ns += elapsed_ns,
                .file_io => self.snapshot_value.file_io_ns += elapsed_ns,
            }
        }
    };

    pub fn init(allocator: Allocator) ObjectFileCompiler {
        return .{ .allocator = allocator };
    }

    /// Compile a pack program: every procedure is a global symbol and the
    /// object is emitted even though no host entrypoint requests it.
    pub fn initForPack(allocator: Allocator) ObjectFileCompiler {
        return .{ .allocator = allocator, .pack_mode = true };
    }

    /// Compile LIR to a native object file for the given RocTarget.
    ///
    /// Dispatches at runtime to the correct compile-time LirCodeGen
    /// instantiation for the requested target. Works for both native and
    /// cross-compilation—the caller just passes the desired target.
    ///
    /// Returns CompilationError.UnsupportedTarget for arm32 and wasm32 targets.
    pub fn compileToObjectFile(
        self: *ObjectFileCompiler,
        lir_store: *const LirStore,
        layout_store: *const layout.Store,
        entrypoints: []const Entrypoint,
        static_data_exports: []const StaticDataExport,
        proc_specs: []const LirProcSpec,
        erased_arg_desc_offsets: []const lir.LIR.ErasedArgDescOffset,
        erased_arg_desc_params: []const lir.LIR.ErasedArgDescParam,
        boxy_worker_procs: []const lir.LIR.LirProcSpecId,
        target: RocTarget,
    ) CompilationError!CompilationResult {
        return crossCompileDispatch(self.allocator, lir_store, layout_store, entrypoints, static_data_exports, proc_specs, erased_arg_desc_offsets, erased_arg_desc_params, boxy_worker_procs, target, self.enable_default_platform_runtime, self.timing, self.pack_mode, self.splice_source, self.capture_artifacts, self.post_check_executor, self.reuse_same_program);
    }

    /// Compile to an object file and write it to a path. Returns whether the
    /// compiled object emits boxy runtime calls; when set, the caller must add
    /// the boxy runtime object and the embedded sidecar to the link.
    pub fn compileToObjectFileAndWrite(
        self: *ObjectFileCompiler,
        lir_store: *const LirStore,
        layout_store: *const layout.Store,
        entrypoints: []const Entrypoint,
        static_data_exports: []const StaticDataExport,
        proc_specs: []const LirProcSpec,
        erased_arg_desc_offsets: []const lir.LIR.ErasedArgDescOffset,
        erased_arg_desc_params: []const lir.LIR.ErasedArgDescParam,
        boxy_worker_procs: []const lir.LIR.LirProcSpecId,
        target: RocTarget,
        output_path: []const u8,
        roc_ctx: CoreCtx,
    ) CompilationError!bool {
        var result = try self.compileToObjectFile(
            lir_store,
            layout_store,
            entrypoints,
            static_data_exports,
            proc_specs,
            erased_arg_desc_offsets,
            erased_arg_desc_params,
            boxy_worker_procs,
            target,
        );
        defer result.deinit();
        if (self.capture_artifacts) {
            if (self.captured_artifacts) |*previous| previous.deinit();
            self.captured_artifacts = result.artifacts;
            result.artifacts = null;
        }

        // Write to file. Use the AV-safe wrapper so a transient AccessDenied
        // from a Windows filter driver holding the just-created file open is
        // retried rather than failing the build.
        const file_io_started_ns = if (self.timing) |timing| timing.start() else 0;
        writeFileWindowsAvSafe(roc_ctx.std_io, output_path, result.object_bytes) catch |err| {
            std.log.err("failed to write object file {s}: {}", .{ output_path, err });
            return CompilationError.ObjectGenerationFailed;
        };
        if (self.timing) |timing| timing.finish(file_io_started_ns, .file_io);
        return result.uses_boxy;
    }

    /// Emit a data-only object from already materialized readonly exports.
    pub fn compileStaticDataObject(
        self: *ObjectFileCompiler,
        static_data_exports: []const StaticDataExport,
        target: RocTarget,
    ) CompilationError!CompilationResult {
        return compileStaticDataObjectBytes(self.allocator, static_data_exports, target);
    }

    /// Emit a data-only object and write it to a path.
    pub fn compileStaticDataObjectAndWrite(
        self: *ObjectFileCompiler,
        static_data_exports: []const StaticDataExport,
        target: RocTarget,
        output_path: []const u8,
        roc_ctx: CoreCtx,
    ) CompilationError!void {
        var result = try self.compileStaticDataObject(static_data_exports, target);
        defer result.deinit();

        writeFileWindowsAvSafe(roc_ctx.std_io, output_path, result.object_bytes) catch |err| {
            std.log.err("failed to write static data object file {s}: {}", .{ output_path, err });
            return CompilationError.ObjectGenerationFailed;
        };
    }
};

/// On Windows, filter drivers (Defender, EDR agents) can transiently hold a
/// just-created file open and return AccessDenied on a follow-up write from a
/// sibling process. Retry a few times with exponential backoff. Other OSes
/// pass through to a single writeFile call.
pub fn writeFileWindowsAvSafe(io: std.Io, sub_path: []const u8, data: []const u8) std.Io.Dir.WriteFileError!void {
    if (comptime builtin.os.tag != .windows) {
        return CoreCtx.writeFileCwd(io, sub_path, data);
    }
    var attempt: u32 = 0;
    const max_attempts: u32 = 6;
    while (true) : (attempt += 1) {
        CoreCtx.writeFileCwd(io, sub_path, data) catch |err| switch (err) {
            error.AccessDenied => {
                if (attempt + 1 >= max_attempts) return err;
                const delay_ms: u32 = @intCast(@as(u64, 10) * (@as(u64, 1) << @intCast(attempt)));
                std.Io.sleep(io, std.Io.Duration.fromMilliseconds(@intCast(delay_ms)), .awake) catch {};
                continue;
            },
            else => return err,
        };
        return;
    }
}

/// Generic compilation function parameterized by code generator type.
fn compileWithCodeGen(
    comptime CodeGen: type,
    allocator: Allocator,
    lir_store: *const LirStore,
    layout_store: *const layout.Store,
    entrypoints: []const Entrypoint,
    static_data_exports: []const StaticDataExport,
    proc_specs: []const LirProcSpec,
    erased_arg_desc_offsets: []const lir.LIR.ErasedArgDescOffset,
    erased_arg_desc_params: []const lir.LIR.ErasedArgDescParam,
    boxy_worker_procs: []const lir.LIR.LirProcSpecId,
    target: RocTarget,
    enable_default_platform_runtime: bool,
    timing: ?*ObjectFileCompiler.Timing,
    pack_mode: bool,
    splice_source: ?SpliceSource,
    capture_artifacts: bool,
    executor: ?Executor,
    reuse_same_program: ?*const NativeProcCompiler.Retained,
) CompilationError!CompilationResult {
    if (!pack_mode and entrypoints.len == 0 and static_data_exports.len == 0) {
        return CompilationError.NoEntrypoints;
    }

    const backend_setup_started_ns = if (timing) |timings| timings.start() else 0;
    var static_strings = StaticStringData.build(allocator, lir_store, target) catch {
        return CompilationError.OutOfMemory;
    };
    defer static_strings.deinit();

    // Initialize the code generator
    var codegen = CodeGen.initWithBoxyMetadata(
        allocator,
        lir_store,
        layout_store,
        static_strings.view(),
        erased_arg_desc_offsets,
        erased_arg_desc_params,
        boxy_worker_procs,
        target.cpuLevel(),
    ) catch return CompilationError.OutOfMemory;
    defer codegen.deinit();

    // Set object file mode to generate relocatable symbol references instead of direct pointers
    codegen.generation_mode = .object_file;
    codegen.setStaticDataSymbols(static_data_exports) catch return CompilationError.OutOfMemory;
    codegen.enable_default_platform_runtime = enable_default_platform_runtime;

    const static_rc_helpers = static_data_export.collectRequiredRcHelpers(allocator, static_data_exports) catch {
        return CompilationError.OutOfMemory;
    };
    defer allocator.free(static_rc_helpers);
    if (timing) |timings| timings.finish(backend_setup_started_ns, .backend_setup);

    // Native emission includes the transitive RC-helper closure. Do not time
    // or compile static-export helpers a second time.
    const procedure_instructions_started_ns = if (timing) |timings| timings.start() else 0;
    var spliced_data = std.ArrayList(ProcArtifact.DataItem).empty;
    defer spliced_data.deinit(allocator);
    if (splice_source) |source| {
        var external_procs = std.ArrayList(lir.LIR.LirProcSpecId).empty;
        defer external_procs.deinit(allocator);
        for (proc_specs, 0..) |proc, index| {
            if (proc.external) external_procs.append(allocator, @enumFromInt(@as(u32, @intCast(index)))) catch return CompilationError.OutOfMemory;
        }
        spliceExternalProcs(CodeGen, allocator, &codegen, proc_specs, external_procs.items, source, &spliced_data) catch return CompilationError.OutOfMemory;
    }
    var demand = std.ArrayList(lir.LIR.LirProcSpecId).empty;
    defer demand.deinit(allocator);
    for (proc_specs, 0..) |proc, index| {
        if (proc.is_static_initializer or proc.external) continue;
        demand.append(allocator, @enumFromInt(index)) catch return CompilationError.OutOfMemory;
    }
    var native_metrics: NativeProcCompiler.Metrics = .{};
    var retained = NativeProcCompiler.run(CodeGen, allocator, &codegen, demand.items, .{
        .target = target,
        .executor = executor,
        .reuse_same_program = reuse_same_program,
        .metrics_out = &native_metrics,
        .static_helpers = static_rc_helpers,
        .constant_exports = static_data_exports,
    }) catch return CompilationError.OutOfMemory;
    // Artifact data remains borrowed by the destination until object encoding
    // and optional pack capture have finished.
    defer retained.deinit();
    const native_data = retained.dataItems(allocator) catch return CompilationError.OutOfMemory;
    defer allocator.free(native_data);
    spliced_data.appendSlice(allocator, native_data) catch return CompilationError.OutOfMemory;
    if (timing) |timings| timings.snapshot_value.native_emission.add(native_metrics);
    if (timing) |timings| timings.finish(procedure_instructions_started_ns, .procedure_instructions);

    // Track symbols for object file generation
    var symbol_relocations_started_ns = if (timing) |timings| timings.start() else 0;
    var symbols = std.ArrayList(SymbolDefinition).empty;
    defer symbols.deinit(allocator);

    var rodata = std.ArrayList(u8).empty;
    defer rodata.deinit(allocator);

    var rodata_relocations = std.ArrayList(ObjectWriter.IndexedDataRelocation).empty;
    defer rodata_relocations.deinit(allocator);

    var owned_proc_symbol_names = std.ArrayList([]u8).empty;
    defer {
        for (owned_proc_symbol_names.items) |name| allocator.free(name);
        owned_proc_symbol_names.deinit(allocator);
    }

    var dwarf_procs = std.ArrayList(Dwarf.ProcEntry).empty;
    defer dwarf_procs.deinit(allocator);

    var seen_proc_symbol_names = std.StringHashMap(void).init(allocator);
    defer seen_proc_symbol_names.deinit();

    try appendStaticDataExports(allocator, &codegen.codegen.symbols, static_data_exports, &rodata, &rodata_relocations, &symbols);
    try appendStaticDataExports(allocator, &codegen.codegen.symbols, static_strings.exports, &rodata, &rodata_relocations, &symbols);
    {
        // Readonly data named by native artifacts or spliced object-cache code
        // that this program did not define itself.
        var defined = std.StringHashMap(void).init(allocator);
        defer defined.deinit();
        for (static_data_exports) |data_export| defined.put(data_export.symbol_name, {}) catch return CompilationError.OutOfMemory;
        for (static_strings.exports) |data_export| defined.put(data_export.symbol_name, {}) catch return CompilationError.OutOfMemory;
        var extra = std.ArrayList(static_data_export.StaticDataExport).empty;
        defer extra.deinit(allocator);
        var extra_relocations = std.ArrayList([]static_data_export.StaticDataRelocation).empty;
        defer {
            for (extra_relocations.items) |relocations| allocator.free(relocations);
            extra_relocations.deinit(allocator);
        }
        for (spliced_data.items) |item| {
            const gop = defined.getOrPut(item.name) catch return CompilationError.OutOfMemory;
            if (gop.found_existing) continue;
            const relocations = allocator.alloc(static_data_export.StaticDataRelocation, item.relocations.len) catch return CompilationError.OutOfMemory;
            extra_relocations.append(allocator, relocations) catch {
                allocator.free(relocations);
                return CompilationError.OutOfMemory;
            };
            for (item.relocations, relocations) |relocation, *out| out.* = .{
                .offset = relocation.offset,
                .target_symbol_name = relocation.name,
                .target = .named,
                .addend = relocation.addend,
                .kind = if (relocation.function) .function_pointer else .address,
            };
            extra.append(allocator, .{
                .symbol_name = item.name,
                .bytes = item.bytes,
                .symbol_offset = item.symbol_offset,
                .alignment = item.alignment,
                .is_global = false,
                .is_exported = false,
                .relocations = relocations,
            }) catch return CompilationError.OutOfMemory;
        }
        try appendStaticDataExports(allocator, &codegen.codegen.symbols, extra.items, &rodata, &rodata_relocations, &symbols);
    }

    for (proc_specs, 0..) |_, i| {
        const proc_id: lir.LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(i)));
        if (proc_specs[i].is_static_initializer) continue;
        const proc_symbol = codegen.compiledProcSymbol(proc_id) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("ObjectFileCompiler invariant violated: LIR proc {d} was not compiled before symbol publication", .{i});
            }
            unreachable;
        };
        const symbol_name = static_data_export.procSymbolName(allocator, proc_specs[i].identity) catch return CompilationError.OutOfMemory;
        if (seen_proc_symbol_names.contains(symbol_name)) {
            std.debug.panic("ObjectFileCompiler invariant violated: two LIR procs share the symbol {s}", .{symbol_name});
        }
        seen_proc_symbol_names.putNoClobber(symbol_name, {}) catch return CompilationError.OutOfMemory;
        owned_proc_symbol_names.append(allocator, symbol_name) catch {
            allocator.free(symbol_name);
            return CompilationError.OutOfMemory;
        };
        appendDefinition(allocator, &codegen.codegen.symbols, &symbols, .{
            .name = symbol_name,
            .offset = proc_symbol.code_start,
            .size = proc_symbol.code_end - proc_symbol.code_start,
            .is_global = pack_mode,
            .is_function = true,
            .is_external = false,
            .section = .text,
            .prologue_size = proc_symbol.prologue_size,
            .stack_alloc = proc_symbol.stack_alloc,
            .frame_size = proc_symbol.frame_size,
            .callee_saved_mask = proc_symbol.callee_saved_mask,
            .epilogue_offset = proc_symbol.epilogue_offset,
            .uses_frame_pointer = proc_symbol.uses_frame_pointer,
        }) catch return CompilationError.OutOfMemory;
        dwarf_procs.append(allocator, .{
            .name = symbol_name,
            .code_start = proc_symbol.code_start,
            .code_size = proc_symbol.code_end - proc_symbol.code_start,
            .loc = lir_store.procLoc(proc_id),
        }) catch return CompilationError.OutOfMemory;
    }
    for (static_rc_helpers) |helper_key| {
        const helper = codegen.compiledStaticDataRcHelperInfo(helper_key) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "ObjectFileCompiler invariant violated: static RC helper {x} was not compiled before symbol publication",
                    .{helper_key.encode()},
                );
            }
            unreachable;
        };
        const symbol_name = static_data_export.atomicRcHelperSymbolName(allocator, layout_store, helper_key) catch return CompilationError.OutOfMemory;
        owned_proc_symbol_names.append(allocator, symbol_name) catch {
            allocator.free(symbol_name);
            return CompilationError.OutOfMemory;
        };
        appendDefinition(allocator, &codegen.codegen.symbols, &symbols, .{
            .name = symbol_name,
            .offset = helper.start_offset,
            .size = helper.end_offset - helper.start_offset,
            .is_global = true,
            .is_function = true,
            .is_external = false,
            .is_hidden = true,
            .section = .text,
            .prologue_size = helper.prologue_size,
            .stack_alloc = helper.stack_alloc,
            .frame_size = helper.frame_size,
            .callee_saved_mask = helper.callee_saved_mask,
            .epilogue_offset = helper.epilogue_offset,
            .uses_frame_pointer = helper.uses_frame_pointer,
        }) catch return CompilationError.OutOfMemory;
    }

    // Reference-count helpers compiled inline after a procedure body get
    // local symbols and DWARF entries of their own, so a frame inside one is
    // named rather than attributed to whichever procedure precedes it. The
    // helpers static data already published above are skipped by offset.
    {
        var published_starts = std.AutoHashMapUnmanaged(u64, void).empty;
        defer published_starts.deinit(allocator);
        for (symbols.items) |definition| {
            const sym = definition.symbol;
            if (!sym.is_function or sym.is_external) continue;
            published_starts.put(allocator, sym.offset, {}) catch return CompilationError.OutOfMemory;
        }
        var recorded_ranges = std.AutoHashMapUnmanaged(u32, coff.FunctionInfo).empty;
        defer recorded_ranges.deinit(allocator);
        for (codegen.getUnwindFunctions()) |function| {
            recorded_ranges.put(allocator, function.start_offset, function) catch return CompilationError.OutOfMemory;
        }
        const rc_helpers = codegen.compiledRcHelpers(allocator) catch return CompilationError.OutOfMemory;
        defer allocator.free(rc_helpers);
        for (rc_helpers) |rc_helper| {
            if (published_starts.contains(rc_helper.start_offset)) continue;
            const info = recorded_ranges.get(@intCast(rc_helper.start_offset)) orelse continue;
            const symbol_name = LirCodeGenMod.compiledRcHelperSymbolName(allocator, layout_store, rc_helper.key) catch return CompilationError.OutOfMemory;
            owned_proc_symbol_names.append(allocator, symbol_name) catch {
                allocator.free(symbol_name);
                return CompilationError.OutOfMemory;
            };
            appendDefinition(allocator, &codegen.codegen.symbols, &symbols, .{
                .name = symbol_name,
                .offset = info.start_offset,
                .size = info.end_offset - info.start_offset,
                .is_global = false,
                .is_function = true,
                .is_external = false,
                .section = .text,
                .prologue_size = info.prologue_size,
                .stack_alloc = info.stack_alloc,
                .frame_size = info.frame_size,
                .callee_saved_mask = info.callee_saved_mask,
                .epilogue_offset = info.epilogue_offset,
                .uses_frame_pointer = info.uses_frame_pointer,
            }) catch return CompilationError.OutOfMemory;
            dwarf_procs.append(allocator, .{
                .name = symbol_name,
                .code_start = info.start_offset,
                .code_size = info.end_offset - info.start_offset,
                .loc = .none,
            }) catch return CompilationError.OutOfMemory;
        }
    }
    if (timing) |timings| timings.finish(symbol_relocations_started_ns, .symbol_relocations);

    // Generate entrypoint wrappers
    const entrypoint_instructions_started_ns = if (timing) |timings| timings.start() else 0;
    for (entrypoints) |entrypoint| {
        const export_info = codegen.generateEntrypointWrapper(
            entrypoint.symbol_name,
            entrypoint.proc,
            entrypoint.arg_layouts,
            entrypoint.ret_layout,
        ) catch return CompilationError.OutOfMemory;

        appendDefinition(allocator, &codegen.codegen.symbols, &symbols, .{
            .name = entrypoint.symbol_name,
            .offset = export_info.offset,
            .size = export_info.size,
            .is_global = true,
            .is_function = true,
            .is_external = false,
            .section = .text,
            // Unwind metadata for Windows object files.
            .prologue_size = export_info.prologue_size,
            .stack_alloc = export_info.stack_alloc,
            .frame_size = export_info.frame_size,
            .callee_saved_mask = export_info.callee_saved_mask,
            .epilogue_offset = export_info.epilogue_offset,
            .uses_frame_pointer = export_info.uses_frame_pointer,
        }) catch {
            return CompilationError.OutOfMemory;
        };
    }
    if (timing) |timings| timings.finish(entrypoint_instructions_started_ns, .entrypoint_instructions);

    // Get generated code and relocations
    symbol_relocations_started_ns = if (timing) |timings| timings.start() else 0;
    codegen.finishImage() catch return CompilationError.OutOfMemory;
    if (artifactRoundTripRequested()) {
        var fresh = CodeGen.initWithBoxyMetadata(
            allocator,
            lir_store,
            layout_store,
            static_strings.view(),
            erased_arg_desc_offsets,
            erased_arg_desc_params,
            boxy_worker_procs,
            target.cpuLevel(),
        ) catch return CompilationError.OutOfMemory;
        defer fresh.deinit();
        fresh.generation_mode = .object_file;
        fresh.setStaticDataSymbols(static_data_exports) catch return CompilationError.OutOfMemory;
        fresh.enable_default_platform_runtime = enable_default_platform_runtime;
        ProcArtifact.verifyRoundTrip(CodeGen, allocator, &codegen, &fresh, proc_specs, layout_store, static_strings.exports) catch |err| switch (err) {
            error.OutOfMemory => return CompilationError.OutOfMemory,
            error.NestedCodeRegion,
            error.UncoveredCode,
            error.DanglingReference,
            error.UnsupportedRelocation,
            error.UnknownProcIdentity,
            error.UnknownRcHelper,
            error.RoundTripMismatch,
            => std.debug.panic("dev artifact round trip failed: {s}", .{@errorName(err)}),
        };
    }
    const code = codegen.getGeneratedCode();
    const relocations = codegen.getRelocations();

    var resolved = try resolveObjectSymbols(allocator, &codegen.codegen.symbols, symbols.items, relocations);
    defer resolved.deinit(allocator);
    if (timing) |timings| timings.finish(symbol_relocations_started_ns, .symbol_relocations);

    // Build DWARF debug sections from the line entries recorded during
    // code generation.
    const dwarf_started_ns = if (timing) |timings| timings.start() else 0;
    const source_file_names = allocator.alloc([]const u8, lir_store.sourceFileCount()) catch {
        return CompilationError.OutOfMemory;
    };
    defer allocator.free(source_file_names);
    for (source_file_names, 0..) |*name, i| {
        name.* = lir_store.sourceFileName(@intCast(i));
    }
    var dwarf_sections = Dwarf.build(
        allocator,
        "roc dev",
        source_file_names,
        codegen.getLineEntries(),
        dwarf_procs.items,
        code.len,
    ) catch return CompilationError.OutOfMemory;
    defer dwarf_sections.deinit(allocator);
    if (timing) |timings| timings.finish(dwarf_started_ns, .dwarf);

    // Generate object file
    const object_encoding_started_ns = if (timing) |timings| timings.start() else 0;
    var output = std.ArrayList(u8).empty;
    errdefer output.deinit(allocator);

    ObjectWriter.generateIndexedObjectFileWithDebug(
        allocator,
        target,
        code,
        rodata.items,
        resolved.symbols,
        relocations,
        rodata_relocations.items,
        codegen.getUnwindFunctions(),
        .{
            .line = dwarf_sections.debug_line,
            .abbrev = dwarf_sections.debug_abbrev,
            .info = dwarf_sections.debug_info,
            .line_relocs = dwarf_sections.line_relocs,
            .info_relocs = dwarf_sections.info_relocs,
        },
        &output,
    ) catch |err| switch (err) {
        error.OutOfMemory => return CompilationError.OutOfMemory,
        error.UnsupportedTarget => return CompilationError.UnsupportedTarget,
    };

    const object_bytes = output.toOwnedSlice(allocator) catch return CompilationError.OutOfMemory;
    if (timing) |timings| timings.finish(object_encoding_started_ns, .object_encoding);

    var artifacts: ?ProcArtifact.Set = null;
    if (pack_mode or capture_artifacts) {
        artifacts = ProcArtifact.extract(CodeGen, allocator, &codegen, proc_specs, layout_store, static_strings.exports, static_data_exports, spliced_data.items) catch |err| switch (err) {
            error.OutOfMemory => return CompilationError.OutOfMemory,
            error.NestedCodeRegion, error.UncoveredCode, error.DanglingReference, error.UnsupportedRelocation => std.debug.panic("pack artifact extraction failed: {s}", .{@errorName(err)}),
        };
    }

    return CompilationResult{
        .object_bytes = object_bytes,
        .allocator = allocator,
        .uses_boxy = codegen.boxy_runtime_used,
        .artifacts = artifacts,
    };
}

const SymbolDefinition = struct {
    id: SymbolTable.Id,
    symbol: ObjectWriter.Symbol,
};

/// An artifact in a loaded pack.
pub const LocatedArtifact = struct {
    set: *const ProcArtifact.Set,
    index: u32,
};

/// The object cache's artifacts, asked by procedure identity. The consumer
/// that loaded the packs supplies the context and the lookup.
pub const SpliceSource = struct {
    context: *anyopaque,
    find: *const fn (context: *anyopaque, identity: lir.ProcIdentity) ?LocatedArtifact,
};

/// Place the object-cache entry of each of `external_procs`, with its
/// closure, into the code generator before the program's own procedures
/// compile. Each pack's artifacts are placed once, in the given order. The
/// data items the entries carry are appended to `data_out` for the caller
/// to define.
pub fn spliceExternalProcs(
    comptime CodeGen: type,
    allocator: Allocator,
    codegen: *CodeGen,
    proc_specs: []const LirProcSpec,
    external_procs: []const lir.LIR.LirProcSpecId,
    source: SpliceSource,
    data_out: *std.ArrayList(ProcArtifact.DataItem),
) Allocator.Error!void {
    var procs_by_identity = std.AutoHashMap(lir.ProcIdentity, lir.LIR.LirProcSpecId).init(allocator);
    defer procs_by_identity.deinit();
    for (proc_specs, 0..) |proc, index| {
        if (proc.is_static_initializer) continue;
        try procs_by_identity.put(proc.identity, @enumFromInt(@as(u32, @intCast(index))));
    }

    const PackState = struct { placed: std.AutoHashMap(u32, usize) };
    var packs = std.AutoHashMap(*const ProcArtifact.Set, PackState).init(allocator);
    defer {
        var states = packs.valueIterator();
        while (states.next()) |state| state.placed.deinit();
        packs.deinit();
    }

    for (external_procs) |proc_id| {
        const proc = proc_specs[@intFromEnum(proc_id)];
        if (!proc.external) std.debug.panic("procedure {d} was offered for splicing but is not an object-cache entry", .{@intFromEnum(proc_id)});
        const located = source.find(source.context, proc.identity) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("object cache served a specialization whose artifact {s} is not in any loaded pack", .{&proc.identity.symbolHex()});
            }
            unreachable;
        };
        const gop = try packs.getOrPut(located.set);
        if (!gop.found_existing) gop.value_ptr.* = .{ .placed = std.AutoHashMap(u32, usize).init(allocator) };
        try ProcArtifact.splice(CodeGen, allocator, codegen, located.set, &.{located.index}, &procs_by_identity, &gop.value_ptr.placed, data_out);
    }
}

fn appendDefinition(
    allocator: Allocator,
    table: *SymbolTable.Table,
    definitions: *std.ArrayList(SymbolDefinition),
    symbol: ObjectWriter.Symbol,
) Allocator.Error!void {
    const id = try table.intern(allocator, symbol.name);
    try definitions.append(allocator, .{ .id = id, .symbol = symbol });
}

/// Final symbol metadata is indexed by the IDs assigned during code generation.
const ResolvedObjectSymbols = struct {
    symbols: []ObjectWriter.Symbol,

    fn deinit(self: *ResolvedObjectSymbols, allocator: Allocator) void {
        allocator.free(self.symbols);
    }
};

fn resolveObjectSymbols(
    allocator: Allocator,
    table: *SymbolTable.Table,
    definitions: []const SymbolDefinition,
    relocations: []const IndexedRelocation,
) CompilationError!ResolvedObjectSymbols {
    const symbols = allocator.alloc(ObjectWriter.Symbol, table.names.items.len) catch return CompilationError.OutOfMemory;
    for (table.names.items, symbols) |name, *symbol| symbol.* = .{
        .name = name,
        .offset = 0,
        .size = 0,
        .is_global = false,
        .is_function = false,
        .is_external = true,
        .section = .undef,
    };
    for (relocations) |relocation| switch (relocation) {
        .linked_function => |function| {
            symbols[@intFromEnum(function.symbol)].is_function = true;
        },
        .linked_data, .local_data, .jmp_to_return, .retired => {},
    };
    for (definitions) |definition| {
        const symbol = &symbols[@intFromEnum(definition.id)];
        std.debug.assert(symbol.is_external);
        symbol.* = definition.symbol;
    }
    for (table.required_definitions.items) |id| {
        std.debug.assert(!symbols[@intFromEnum(id)].is_external);
    }
    return .{ .symbols = symbols };
}

fn appendStaticDataExports(
    allocator: Allocator,
    table: *SymbolTable.Table,
    exports: []const StaticDataExport,
    rodata: *std.ArrayList(u8),
    relocations: *std.ArrayList(ObjectWriter.IndexedDataRelocation),
    symbols: *std.ArrayList(SymbolDefinition),
) CompilationError!void {
    const data_symbols = allocator.alloc(SymbolTable.Id, exports.len) catch return CompilationError.OutOfMemory;
    defer allocator.free(data_symbols);
    for (exports, data_symbols) |data_export, *id| id.* = table.intern(allocator, data_export.symbol_name) catch return CompilationError.OutOfMemory;
    var functions = collections.DenseMap(lir.LIR.LirProcSpecId, SymbolTable.Id).init(allocator);
    defer functions.deinit();
    var helpers = std.AutoHashMap(layout.RcHelperKey, SymbolTable.Id).init(allocator);
    defer helpers.deinit();
    for (exports, data_symbols) |data_export, definition_id| {
        const start = rodata.items.len;
        try appendStaticDataExport(allocator, data_export, definition_id, rodata, symbols);
        const aligned_offset = std.mem.alignForward(usize, start, @intCast(data_export.alignment));
        for (data_export.relocations) |relocation| {
            const id = switch (relocation.target) {
                .data_symbol => |target| data_symbols[@intFromEnum(target)],
                .named => blk: {
                    if (relocation.procedure) |proc| {
                        if (functions.get(proc)) |id| break :blk id;
                        const id = table.intern(allocator, relocation.target_symbol_name) catch return CompilationError.OutOfMemory;
                        functions.put(proc, id) catch return CompilationError.OutOfMemory;
                        break :blk id;
                    }
                    if (relocation.rc_helper) |helper| {
                        if (helpers.get(helper)) |id| break :blk id;
                        const id = table.intern(allocator, relocation.target_symbol_name) catch return CompilationError.OutOfMemory;
                        helpers.put(helper, id) catch return CompilationError.OutOfMemory;
                        break :blk id;
                    }
                    break :blk table.intern(allocator, relocation.target_symbol_name) catch return CompilationError.OutOfMemory;
                },
            };
            relocations.append(allocator, .{
                .offset = @as(u64, @intCast(aligned_offset)) + relocation.offset,
                .symbol = id,
                .addend = relocation.addend,
            }) catch return CompilationError.OutOfMemory;
        }
    }
}

fn appendStaticDataExport(
    allocator: Allocator,
    data_export: StaticDataExport,
    id: SymbolTable.Id,
    rodata: *std.ArrayList(u8),
    static_data_symbols: *std.ArrayList(SymbolDefinition),
) CompilationError!void {
    const alignment = @as(usize, @intCast(data_export.alignment));
    const aligned_offset = std.mem.alignForward(usize, rodata.items.len, alignment);
    rodata.appendNTimes(allocator, 0, aligned_offset - rodata.items.len) catch {
        return CompilationError.OutOfMemory;
    };
    rodata.appendSlice(allocator, data_export.bytes) catch {
        return CompilationError.OutOfMemory;
    };

    const symbol_offset: usize = @intCast(data_export.symbol_offset);
    if (builtin.mode == .Debug and symbol_offset > data_export.bytes.len) {
        std.debug.panic(
            "ObjectFileCompiler invariant violated: static data symbol offset {d} exceeds byte length {d}",
            .{ data_export.symbol_offset, data_export.bytes.len },
        );
    }

    static_data_symbols.append(allocator, .{ .id = id, .symbol = .{
        .name = data_export.symbol_name,
        .offset = aligned_offset + symbol_offset,
        .size = data_export.bytes.len - symbol_offset,
        .is_global = data_export.is_global,
        .is_function = false,
        .is_external = false,
        .is_hidden = !data_export.is_exported,
        .section = .rodata,
    } }) catch {
        return CompilationError.OutOfMemory;
    };
}

fn compileStaticDataObjectBytes(
    allocator: Allocator,
    static_data_exports: []const StaticDataExport,
    target: RocTarget,
) CompilationError!CompilationResult {
    if (static_data_exports.len == 0) {
        return CompilationError.NoEntrypoints;
    }

    var table: SymbolTable.Table = .{};
    defer table.deinit(allocator);

    var symbols = std.ArrayList(SymbolDefinition).empty;
    defer symbols.deinit(allocator);

    var rodata = std.ArrayList(u8).empty;
    defer rodata.deinit(allocator);

    var rodata_relocations = std.ArrayList(ObjectWriter.IndexedDataRelocation).empty;
    defer rodata_relocations.deinit(allocator);

    try appendStaticDataExports(allocator, &table, static_data_exports, &rodata, &rodata_relocations, &symbols);

    // This object is linked separately from generated code. LLVM constant
    // expressions can reference any backing named by a frozen relocation,
    // including allocations private to the program. Give those definitions
    // cross-object binding while retaining their explicit hidden visibility.
    // Combined code/data objects keep the materializer's local bindings.
    for (symbols.items) |*definition| definition.symbol.is_global = true;

    var resolved = try resolveObjectSymbols(allocator, &table, symbols.items, &.{});
    defer resolved.deinit(allocator);

    var output = std.ArrayList(u8).empty;
    errdefer output.deinit(allocator);

    ObjectWriter.generateIndexedObjectFileWithDebug(
        allocator,
        target,
        &.{},
        rodata.items,
        resolved.symbols,
        &.{},
        rodata_relocations.items,
        &.{},
        null,
        &output,
    ) catch |err| switch (err) {
        error.OutOfMemory => return CompilationError.OutOfMemory,
        error.UnsupportedTarget => return CompilationError.UnsupportedTarget,
    };

    return .{
        .object_bytes = output.toOwnedSlice(allocator) catch return CompilationError.OutOfMemory,
        .allocator = allocator,
    };
}

/// Runtime-to-comptime dispatch for compilation.
/// Uses inline for over RocTarget enum fields to select the correct LirCodeGen instantiation.
///
/// Only default-CPU targets are instantiated. A `v1` target compiles through
/// its default twin's instantiation and carries its CPU level as a runtime
/// field, so the baseline targets cost no extra monomorphizations: they select
/// different instruction sequences, not a different code generator.
fn crossCompileDispatch(
    allocator: Allocator,
    lir_store: *const LirStore,
    layout_store: *const layout.Store,
    entrypoints: []const Entrypoint,
    static_data_exports: []const StaticDataExport,
    proc_specs: []const LirProcSpec,
    erased_arg_desc_offsets: []const lir.LIR.ErasedArgDescOffset,
    erased_arg_desc_params: []const lir.LIR.ErasedArgDescParam,
    boxy_worker_procs: []const lir.LIR.LirProcSpecId,
    target: RocTarget,
    enable_default_platform_runtime: bool,
    timing: ?*ObjectFileCompiler.Timing,
    pack_mode: bool,
    splice_source: ?SpliceSource,
    capture_artifacts: bool,
    executor: ?Executor,
    reuse_same_program: ?*const NativeProcCompiler.Retained,
) CompilationError!CompilationResult {
    const enum_info = @typeInfo(RocTarget).@"enum";
    const default_target = target.defaultCpuTarget();
    inline for (enum_info.fields) |field| {
        const comptime_target: RocTarget = @enumFromInt(field.value);
        if (comptime comptime_target.defaultCpuTarget() != comptime_target) continue;
        if (default_target == comptime_target) {
            const arch = comptime comptime_target.toCpuArch();
            if (comptime (arch == .x86_64 or arch == .aarch64 or arch == .aarch64_be)) {
                return compileWithCodeGen(
                    LirCodeGenMod.LirCodeGen(comptime_target),
                    allocator,
                    lir_store,
                    layout_store,
                    entrypoints,
                    static_data_exports,
                    proc_specs,
                    erased_arg_desc_offsets,
                    erased_arg_desc_params,
                    boxy_worker_procs,
                    target,
                    enable_default_platform_runtime,
                    timing,
                    pack_mode,
                    splice_source,
                    capture_artifacts,
                    executor,
                    reuse_same_program,
                );
            } else {
                return CompilationError.UnsupportedTarget;
            }
        }
    }
    return CompilationError.UnsupportedTarget;
}

// Tests

test "ObjectFileCompiler initialization" {
    const allocator = std.testing.allocator;
    const compiler = ObjectFileCompiler.init(allocator);
    try std.testing.expectEqual(allocator, compiler.allocator);
    try std.testing.expect(compiler.post_check_executor == null);
    try std.testing.expect(compiler.reuse_same_program == null);
}

test "ObjectFileCompiler native emission skips static initializers and captures packs" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(allocator, @import("base").target.TargetUsize.native);
    defer layouts.deinit();
    const result_local = try store.addLocal(.{ .layout_idx = .i64 });
    const ret = try store.addCFStmt(.{ .ret = .{ .value = result_local } }, .test_fixture);
    const body = try store.addCFStmt(.{ .assign_literal = .{
        .target = result_local,
        .value = .{ .i64_literal = .{ .value = 42, .layout_idx = .i64 } },
        .next = ret,
    } }, .test_fixture);
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .identity = lir.LIR.ProcIdentity.forTest(1),
        .args = lir.LIR.LocalSpan.empty(),
        .body = body,
        .ret_layout = .i64,
    }, .none);
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .identity = lir.LIR.ProcIdentity.forTest(2),
        .args = lir.LIR.LocalSpan.empty(),
        .body = body,
        .ret_layout = .i64,
        .is_static_initializer = true,
    }, .none);

    var timing = ObjectFileCompiler.Timing.init(std.testing.io);
    var compiler = ObjectFileCompiler.initForPack(allocator);
    compiler.timing = &timing;
    const targets = [_]RocTarget{ .x64linux, .arm64linux, .x64v1linux, .arm64v1linux, .x64v1musl, .arm64v1musl };
    for (targets) |target| {
        var result = try compiler.compileToObjectFile(
            &store,
            &layouts,
            &.{},
            &.{},
            store.getProcSpecs(),
            &.{},
            &.{},
            &.{},
            target,
        );
        defer result.deinit();
        try std.testing.expect(std.mem.startsWith(u8, result.object_bytes, "\x7fELF"));
        try std.testing.expect(!result.uses_boxy);
        try std.testing.expect(result.artifacts != null);
        try std.testing.expect(result.artifacts.?.artifacts.len > 0);
    }
    const snapshot = timing.snapshot();
    try std.testing.expectEqual(@as(u64, targets.len), snapshot.native_emission.procedures_emitted);
    try std.testing.expectEqual(@as(u64, 0), snapshot.native_emission.procedures_reused);
    try std.testing.expectEqual(@as(u64, 0), snapshot.rc_helper_instructions_ns);
}

test "ObjectFileCompiler runtime static-root pack owns only reachable canonical data" {
    const allocator = std.testing.allocator;
    const PackFile = @import("PackFile.zig");
    const text = "immutable runtime data survives its producer";
    const identity = lir.ProcIdentity.forTest(717);
    inline for (.{ RocTarget.x64linux, RocTarget.arm64linux, comptime RocTarget.detectNative() }) |target| {
        var result = producer: {
            var arena = std.heap.ArenaAllocator.init(allocator);
            defer arena.deinit();
            const a = arena.allocator();
            var store = LirStore.init(a);
            defer store.deinit();
            var layouts = try layout.Store.init(a, .u64);
            defer layouts.deinit();
            const local = try store.addLocal(.{ .layout_idx = .str });
            const ret = try store.addCFStmt(.{ .ret = .{ .value = local } }, .test_fixture);
            const body = try store.addCFStmt(.{ .assign_literal = .{
                .target = local,
                .value = .{ .static_data = @enumFromInt(7) },
                .next = ret,
            } }, .test_fixture);
            _ = try store.addProcSpec(.{
                .name = store.freshSyntheticSymbol(),
                .identity = identity,
                .args = .empty(),
                .body = body,
                .ret_layout = .str,
            }, .none);
            const descriptor = try a.alloc(u8, 24);
            @memset(descriptor, 0);
            std.mem.writeInt(u64, descriptor[8..16], text.len << 1, .little);
            std.mem.writeInt(u64, descriptor[16..24], text.len, .little);
            const backing = try a.alloc(u8, 16 + text.len);
            @memset(backing[0..16], 0);
            @memcpy(backing[16..], text);
            const exports = try a.alloc(StaticDataExport, 3);
            const relocation = try a.alloc(StaticDataRelocation, 1);
            relocation[0] = .{
                .offset = 0,
                .target_symbol_name = try a.dupe(u8, "roc__static_producer_leaf"),
                .target = .{ .data_symbol = @enumFromInt(1) },
                .addend = 8,
            };
            exports[0] = .{
                .symbol_name = try a.dupe(u8, "roc__static_producer_root"),
                .value_id = @enumFromInt(7),
                .bytes = descriptor,
                .alignment = 8,
                .is_exported = false,
                .relocations = relocation,
            };
            exports[1] = .{
                .symbol_name = try a.dupe(u8, "roc__static_producer_leaf"),
                .bytes = backing,
                .symbol_offset = 8,
                .alignment = 8,
                .is_exported = false,
            };
            exports[2] = .{
                .symbol_name = try a.dupe(u8, "roc__static_unreachable"),
                .bytes = try a.dupe(u8, "must not travel in the pack"),
                .alignment = 1,
                .is_exported = false,
            };
            var compiler = ObjectFileCompiler.initForPack(allocator);
            break :producer try compiler.compileToObjectFile(&store, &layouts, &.{}, exports, store.getProcSpecs(), &.{}, &.{}, &.{}, target);
        };
        // No source descriptor, backing, symbol names, LIR or layout owners remain.
        const bytes = serialized: {
            defer result.deinit();
            break :serialized try PackFile.write(allocator, &result.artifacts.?, &.{});
        };
        defer allocator.free(bytes);
        var pack = try PackFile.read(allocator, bytes);
        defer pack.deinit();
        try std.testing.expectEqual(@as(usize, 1), pack.set.artifacts.len);
        const artifact = pack.set.artifacts[0];
        try std.testing.expectEqual(@as(usize, 2), artifact.data.len);
        for (artifact.data) |item| {
            try std.testing.expect(std.mem.startsWith(u8, item.name, ProcArtifact.content_data_prefix));
            try std.testing.expect(!std.mem.eql(u8, item.name, "roc__static_producer_root"));
            try std.testing.expect(!std.mem.eql(u8, item.name, "roc__static_producer_leaf"));
            for (item.relocations) |relocation| {
                try std.testing.expect(!relocation.external);
                try std.testing.expect(std.mem.startsWith(u8, relocation.name, ProcArtifact.content_data_prefix));
            }
        }
        var data_references: usize = 0;
        for (artifact.relocations) |relocation| {
            if (relocation.kind != .data) continue;
            data_references += 1;
            try std.testing.expect(std.mem.startsWith(u8, relocation.name, ProcArtifact.content_data_prefix));
        }
        try std.testing.expect(data_references > 0);
        // A different consumer has neither the procedure ordinal nor any static table.
        var store = LirStore.init(allocator);
        defer store.deinit();
        var layouts = try layout.Store.init(allocator, .u64);
        defer layouts.deinit();
        const CG = LirCodeGenMod.LirCodeGen(target);
        var receiver = try CG.init(allocator, &store, &layouts, .{}, &.{}, .default);
        defer receiver.deinit();
        var procs = std.AutoHashMap(lir.ProcIdentity, lir.LIR.LirProcSpecId).init(allocator);
        defer procs.deinit();
        var placed = std.AutoHashMap(u32, usize).init(allocator);
        defer placed.deinit();
        var data = std.ArrayList(ProcArtifact.DataItem).empty;
        defer data.deinit(allocator);
        try ProcArtifact.splice(CG, allocator, &receiver, &pack.set, &.{0}, &procs, &placed, &data);
        try receiver.finishImage();
        if (comptime target == RocTarget.detectNative() and LirCodeGenMod.host_lir_codegen_available) {
            var splice = @import("HostSplice.zig").HostSplice.init(allocator);
            defer splice.deinit();
            try splice.addDataItems(data.items);
            var table: LirCodeGenMod.BoxyNativeFnTable = undefined;
            @memset(&table, 0);
            var executable = try splice.link(&receiver, &table);
            defer executable.deinit();
            const builtins = @import("builtins");
            var host = builtins.utils.TestEnv.init(allocator);
            defer host.deinit();
            const saved_host = builtins.in_process_host.enter(host.getOps(), null);
            defer builtins.in_process_host.leave(saved_host);
            var output: [3]usize = @splat(0);
            executable.callRocABIAt(placed.get(0).? + artifact.entry, @ptrCast(&output), null);
            try std.testing.expectEqual(text.len, output[2]);
            const value: [*]const u8 = @ptrFromInt(output[0]);
            try std.testing.expectEqualStrings(text, value[0..output[2]]);
        }
    }
}

/// `ROC_DEV_ARTIFACT_ROUNDTRIP` makes every object compile also assemble the
/// program from its own procedure artifacts and panic if the result differs.
fn artifactRoundTripRequested() bool {
    return std.c.getenv("ROC_DEV_ARTIFACT_ROUNDTRIP") != null;
}
