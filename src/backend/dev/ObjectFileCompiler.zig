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
const invariant = @import("base").invariant;
const dev_diagnostics = @import("base").dev_diagnostics;
const builtin = @import("builtin");
const isaOfOrNull = @import("isa.zig").isaOfOrNull;
const Allocator = std.mem.Allocator;

const layout = @import("layout");
const lir = @import("lir");
const CoreCtx = @import("ctx").CoreCtx;
const LirStore = lir.LirStore;
const LirProcSpec = lir.LirProcSpec;
const RocTarget = @import("roc_target").RocTarget;
const Dwarf = @import("Dwarf.zig");

const ObjectWriter = @import("ObjectWriter.zig");
const LirCodeGenMod = @import("LirCodeGen.zig");
const CodeExcerpt = @import("CodeExcerpt.zig");
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
    /// The compiled program lifted into excerpts, when the compile ran in
    /// pack mode on a target whose excerpts can be extracted.
    excerpts: ?CodeExcerpt.Set = null,
    /// The Boxy runtime table entries the object's code loads, in the
    /// canonical order it numbers them by; the sidecar linked beside the
    /// object holds exactly these (`LirImage.buildArrangedSidecarBlob`).
    runtime_tables: lir.RuntimeTables.Order = .{},

    pub fn deinit(self: *CompilationResult) void {
        self.allocator.free(self.object_bytes);
        if (self.excerpts) |*set| set.deinit();
        self.runtime_tables.deinit(self.allocator);
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
    /// Bytes the program's erased callables reserve before their captures
    /// (`LirProgram.Result.erased_capture_prefix`); nonzero when its code
    /// supports a hot-reload session.
    erased_capture_prefix: u32 = 0,
    timing: ?*Timing = null,
    post_check_executor: ?Executor = null,
    /// Borrowed excerpts from this exact LIR program, never an executable image.
    reuse_same_program: ?*const NativeProcCompiler.Retained = null,
    /// Emit every procedure as a global symbol and emit the object even
    /// without host entrypoints: the object is a module pack, not an app.
    pack_mode: bool = false,
    /// Where the excerpts of external (object-cache) procedures come from.
    splice_source: ?SpliceSource = null,
    /// Lift the compiled program into excerpts and keep them in
    /// `captured_excerpts` after `compileToObjectFileAndWrite`, so the
    /// program's own pack can be written from the build that produced it.
    capture_excerpts: bool = false,
    captured_excerpts: ?CodeExcerpt.Set = null,
    /// Encode the object file and its debug sections. A pack written only
    /// as excerpts leaves this off, and its result's `object_bytes` is
    /// empty.
    emit_object: bool = true,

    pub const TimingSnapshot = struct {
        backend_setup_ns: u64 = 0,
        /// Native excerpt emission, including its RC-helper closure.
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
        return crossCompileDispatch(self.allocator, lir_store, layout_store, entrypoints, static_data_exports, proc_specs, erased_arg_desc_offsets, erased_arg_desc_params, boxy_worker_procs, target, self.enable_default_platform_runtime, self.erased_capture_prefix, self.timing, self.pack_mode, self.splice_source, self.capture_excerpts, self.emit_object, self.post_check_executor, self.reuse_same_program);
    }

    /// What a written object needs linked beside it.
    pub const WrittenObject = struct {
        /// Whether the compiled object emits boxy runtime calls; when set,
        /// the caller must add the boxy runtime object and the embedded
        /// sidecar to the link.
        uses_boxy: bool,
        /// The runtime table entries the object's code loads, which its
        /// sidecar holds (`CompilationResult.runtime_tables`).
        runtime_tables: lir.RuntimeTables.Order,
    };

    /// Compile to an object file and write it to a path.
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
    ) CompilationError!WrittenObject {
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
        if (self.capture_excerpts) {
            if (self.captured_excerpts) |*previous| previous.deinit();
            self.captured_excerpts = result.excerpts;
            result.excerpts = null;
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
        const runtime_tables = result.runtime_tables;
        result.runtime_tables = .{};
        return .{ .uses_boxy = result.uses_boxy, .runtime_tables = runtime_tables };
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
    erased_capture_prefix: u32,
    timing: ?*ObjectFileCompiler.Timing,
    pack_mode: bool,
    splice_source: ?SpliceSource,
    capture_excerpts: bool,
    emit_object: bool,
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
        erased_capture_prefix,
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
    var spliced_data = std.ArrayList(CodeExcerpt.DataItem).empty;
    defer spliced_data.deinit(allocator);
    if (splice_source) |source| {
        var external_procs = std.ArrayList(lir.LIR.LirProcSpecId).empty;
        defer external_procs.deinit(allocator);
        for (proc_specs, 0..) |proc, index| {
            if (proc.external and !proc.c_abi_entry) external_procs.append(allocator, @fromBackingInt(@intCast(@as(u32, @intCast(index))))) catch return CompilationError.OutOfMemory;
        }
        spliceExternalProcs(CodeGen, allocator, &codegen, proc_specs, external_procs.items, source, &spliced_data) catch return CompilationError.OutOfMemory;
    }
    var demand = std.ArrayList(lir.LIR.LirProcSpecId).empty;
    defer demand.deinit(allocator);
    for (proc_specs, 0..) |proc, index| {
        if (proc.is_static_initializer or (proc.external and !proc.c_abi_entry)) continue;
        demand.append(allocator, @fromBackingInt(@intCast(index))) catch return CompilationError.OutOfMemory;
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
    // Excerpt data remains borrowed by the destination until object encoding
    // and optional pack capture have finished.
    defer retained.deinit();
    const native_data = retained.dataItems(allocator) catch return CompilationError.OutOfMemory;
    defer allocator.free(native_data);
    spliced_data.appendSlice(allocator, native_data) catch return CompilationError.OutOfMemory;
    if (timing) |timings| timings.snapshot_value.native_emission.add(native_metrics);
    if (timing) |timings| timings.finish(procedure_instructions_started_ns, .procedure_instructions);

    // Lay the program's code out canonically, so that whether each procedure
    // was compiled now or spliced from an object-cache entry, and in which
    // order either happened, changes nothing in the object (design.md
    // "Deterministic Dev Images"). The code generated so far is lifted into
    // excerpts and placed again, in arranged order and under canonical
    // names, into the code generator the object is encoded from.
    var symbol_relocations_started_ns = if (timing) |timings| timings.start() else 0;
    codegen.finishImage() catch return CompilationError.OutOfMemory;
    var catalog = CodeExcerpt.PreparedData.init(allocator, static_strings.exports, static_data_exports, spliced_data.items) catch return CompilationError.OutOfMemory;
    defer catalog.deinit();
    var staged = CodeExcerpt.extractCataloged(CodeGen, allocator, &codegen, proc_specs, layout_store, &catalog) catch |err| switch (err) {
        error.OutOfMemory => return CompilationError.OutOfMemory,
        error.NestedCodeRegion, error.UncoveredCode, error.DanglingReference, error.UnsupportedRelocation => invariant("arranged image extraction failed: {s}", .{@errorName(err)}),
    };
    defer staged.deinit();
    var host_visible = std.ArrayList(CodeExcerpt.DataItem).empty;
    defer host_visible.deinit(allocator);
    {
        var roots = std.ArrayList([]const u8).empty;
        defer roots.deinit(allocator);
        for (static_data_exports) |data_export| {
            if (data_export.is_exported) roots.append(allocator, data_export.symbol_name) catch return CompilationError.OutOfMemory;
        }
        CodeExcerpt.dataClosure(allocator, &catalog, roots.items, &host_visible) catch |err| switch (err) {
            error.OutOfMemory => return CompilationError.OutOfMemory,
            error.DanglingReference => invariant("a host-visible constant names a constant the program does not define", .{}),
        };
    }
    // Each refcount helper static data names, by the excerpt name of its
    // code, and the global name static data relocates to it by.
    var static_helper_names = std.StringHashMap(layout.RcHelperKey).init(allocator);
    defer {
        var names = static_helper_names.keyIterator();
        while (names.next()) |name| allocator.free(name.*);
        static_helper_names.deinit();
    }
    var helper_aliases = std.StringHashMap([]const u8).init(allocator);
    defer {
        var aliases = helper_aliases.keyIterator();
        while (aliases.next()) |alias| allocator.free(alias.*);
        helper_aliases.deinit();
    }
    for (static_rc_helpers) |helper_key| {
        const name = LirCodeGenMod.compiledRcHelperSymbolName(allocator, layout_store, LirCodeGenMod.staticDataRcHelperKey(helper_key)) catch return CompilationError.OutOfMemory;
        const gop = static_helper_names.getOrPut(name) catch {
            allocator.free(name);
            return CompilationError.OutOfMemory;
        };
        if (gop.found_existing) {
            allocator.free(name);
            continue;
        }
        gop.value_ptr.* = helper_key;
        const alias = static_data_export.atomicRcHelperSymbolName(allocator, layout_store, helper_key) catch return CompilationError.OutOfMemory;
        helper_aliases.put(alias, gop.key_ptr.*) catch {
            allocator.free(alias);
            return CompilationError.OutOfMemory;
        };
    }
    // A linked program's image holds the code its roots reach, however its
    // procedures were provided; a pack program's holds every procedure it
    // offers.
    var reachable: ?CodeExcerpt.Set = null;
    defer if (reachable) |*retained_set| retained_set.deinit();
    if (!pack_mode) {
        const root_procs = allocator.alloc(lir.ProcIdentity, entrypoints.len) catch return CompilationError.OutOfMemory;
        defer allocator.free(root_procs);
        for (entrypoints, root_procs) |entrypoint, *identity| identity.* = proc_specs[@backingInt(entrypoint.proc)].identity;
        var root_data = std.ArrayList([]const u8).empty;
        defer root_data.deinit(allocator);
        for (host_visible.items) |item| root_data.append(allocator, item.name) catch return CompilationError.OutOfMemory;
        reachable = CodeExcerpt.retainReachable(allocator, &staged, &catalog, .{
            .procs = root_procs,
            .data = root_data.items,
            .helper_aliases = &helper_aliases,
            .all_thunks = boxy_worker_procs.len != 0,
        }) catch return CompilationError.OutOfMemory;
    }
    var canonical = CodeExcerpt.arrange(allocator, if (reachable) |*retained_set| retained_set else &staged, host_visible.items) catch return CompilationError.OutOfMemory;
    defer canonical.deinit();

    var final = CodeGen.initWithBoxyMetadata(
        allocator,
        lir_store,
        layout_store,
        static_strings.view(),
        erased_arg_desc_offsets,
        erased_arg_desc_params,
        boxy_worker_procs,
        erased_capture_prefix,
        target.cpuLevel(),
    ) catch return CompilationError.OutOfMemory;
    defer final.deinit();
    final.generation_mode = .object_file;
    final.enable_default_platform_runtime = enable_default_platform_runtime;
    final.boxy_runtime_used = codegen.boxy_runtime_used;

    var procs_by_identity = std.AutoHashMap(lir.ProcIdentity, lir.LIR.LirProcSpecId).init(allocator);
    defer procs_by_identity.deinit();
    for (proc_specs, 0..) |proc, index| {
        if (proc.is_static_initializer) continue;
        procs_by_identity.put(proc.identity, @fromBackingInt(@intCast(@as(u32, @intCast(index))))) catch return CompilationError.OutOfMemory;
    }
    var helper_keys = CodeExcerpt.HelperKeys.init(allocator);
    defer {
        var names = helper_keys.keyIterator();
        while (names.next()) |name| allocator.free(name.*);
        helper_keys.deinit();
    }
    {
        const compiled_helpers = codegen.compiledRcHelpers(allocator) catch return CompilationError.OutOfMemory;
        defer allocator.free(compiled_helpers);
        for (compiled_helpers) |helper| {
            const name = LirCodeGenMod.compiledRcHelperSymbolName(allocator, layout_store, helper.key) catch return CompilationError.OutOfMemory;
            const gop = helper_keys.getOrPut(name) catch {
                allocator.free(name);
                return CompilationError.OutOfMemory;
            };
            if (gop.found_existing) {
                allocator.free(name);
                continue;
            }
            gop.value_ptr.* = helper.key;
        }
    }
    const starts = CodeExcerpt.assembleArranged(CodeGen, allocator, &final, &canonical.set, &procs_by_identity, &helper_keys) catch return CompilationError.OutOfMemory;
    defer allocator.free(starts);
    // The placed code loads the Boxy runtime's table numbers in canonical
    // order; number them by it, so they depend on nothing but that code.
    var runtime_numbering: lir.RuntimeTables.Numbering = .{};
    defer runtime_numbering.deinit(allocator);
    final.renumberRuntimeNumbers(&runtime_numbering) catch return CompilationError.OutOfMemory;

    var symbols = std.ArrayList(SymbolDefinition).empty;
    defer symbols.deinit(allocator);

    var rodata = std.ArrayList(u8).empty;
    defer rodata.deinit(allocator);

    var rodata_relocations = std.ArrayList(ObjectWriter.IndexedDataRelocation).empty;
    defer rodata_relocations.deinit(allocator);
    var zero_fill_size: u64 = 0;

    var owned_proc_symbol_names = std.ArrayList([]u8).empty;
    defer {
        for (owned_proc_symbol_names.items) |name| allocator.free(name);
        owned_proc_symbol_names.deinit(allocator);
    }

    var dwarf_procs = std.ArrayList(Dwarf.ProcEntry).empty;
    defer dwarf_procs.deinit(allocator);

    {
        // Every constant the code names, and every host-visible constant
        // with every constant it names, in name order.
        const data_exports = allocator.alloc(static_data_export.StaticDataExport, canonical.data.len) catch return CompilationError.OutOfMemory;
        defer allocator.free(data_exports);
        var data_relocations = std.ArrayList([]static_data_export.StaticDataRelocation).empty;
        defer {
            for (data_relocations.items) |relocations| allocator.free(relocations);
            data_relocations.deinit(allocator);
        }
        for (canonical.data, data_exports) |datum, *out| {
            const item = datum.item;
            const relocations = allocator.alloc(static_data_export.StaticDataRelocation, item.relocations.len) catch return CompilationError.OutOfMemory;
            data_relocations.append(allocator, relocations) catch {
                allocator.free(relocations);
                return CompilationError.OutOfMemory;
            };
            for (item.relocations, relocations) |relocation, *relocation_out| relocation_out.* = .{
                .offset = relocation.offset,
                .target_symbol_name = relocation.name,
                .target = .named,
                .addend = relocation.addend,
                .kind = if (relocation.function) .function_pointer else .address,
            };
            out.* = .{
                .symbol_name = item.name,
                .bytes = item.bytes,
                .symbol_offset = item.symbol_offset,
                .alignment = item.alignment,
                .is_global = datum.host_visible,
                .is_exported = datum.host_visible,
                .relocations = relocations,
            };
        }
        try appendStaticDataExports(allocator, &final.codegen.symbols, data_exports, &rodata, &zero_fill_size, &rodata_relocations, &symbols);
    }

    {
        var seen_proc_symbol_names = std.StringHashMap(void).init(allocator);
        defer seen_proc_symbol_names.deinit();
        for (canonical.set.excerpts, starts) |excerpt, start| switch (excerpt.kind) {
            .proc => |identity| {
                const frame = excerpt.frame orelse invariant("a procedure excerpt had no frame", .{});
                const symbol_name = static_data_export.procSymbolName(allocator, identity) catch return CompilationError.OutOfMemory;
                owned_proc_symbol_names.append(allocator, symbol_name) catch {
                    allocator.free(symbol_name);
                    return CompilationError.OutOfMemory;
                };
                if ((seen_proc_symbol_names.getOrPut(symbol_name) catch return CompilationError.OutOfMemory).found_existing) {
                    invariant("ObjectFileCompiler invariant violated: two procedures share the symbol {s}", .{symbol_name});
                }
                appendDefinition(allocator, &final.codegen.symbols, &symbols, .{
                    .name = symbol_name,
                    .offset = start,
                    .size = excerpt.code.len,
                    .is_global = pack_mode,
                    .is_function = true,
                    .is_external = false,
                    .section = .text,
                    .prologue_size = frame.prologue_size,
                    .stack_alloc = frame.stack_alloc,
                    .frame_size = frame.frame_size,
                    .callee_saved_mask = frame.callee_saved_mask,
                    .epilogue_offset = frame.epilogue_offset,
                    .uses_frame_pointer = frame.uses_frame_pointer,
                }) catch return CompilationError.OutOfMemory;
                dwarf_procs.append(allocator, .{
                    .name = symbol_name,
                    .code_start = start,
                    .code_size = excerpt.code.len,
                    .loc = excerpt.loc,
                }) catch return CompilationError.OutOfMemory;
            },
            .rc_helper => |name| {
                // A helper compiled inline after a procedure body gets a
                // local symbol and a debug entry of its own, so a frame
                // inside one is named rather than attributed to whichever
                // procedure precedes it.
                const frame = excerpt.frame orelse continue;
                const static_helper = static_helper_names.get(name);
                const symbol_name = if (static_helper) |helper_key|
                    static_data_export.atomicRcHelperSymbolName(allocator, layout_store, helper_key) catch return CompilationError.OutOfMemory
                else
                    allocator.dupe(u8, name) catch return CompilationError.OutOfMemory;
                owned_proc_symbol_names.append(allocator, symbol_name) catch {
                    allocator.free(symbol_name);
                    return CompilationError.OutOfMemory;
                };
                appendDefinition(allocator, &final.codegen.symbols, &symbols, .{
                    .name = symbol_name,
                    .offset = start + excerpt.entry,
                    .size = excerpt.code.len - excerpt.entry,
                    .is_global = static_helper != null,
                    .is_function = true,
                    .is_external = false,
                    .is_hidden = static_helper != null,
                    .section = .text,
                    .prologue_size = frame.prologue_size,
                    .stack_alloc = frame.stack_alloc,
                    .frame_size = frame.frame_size,
                    .callee_saved_mask = frame.callee_saved_mask,
                    .epilogue_offset = frame.epilogue_offset,
                    .uses_frame_pointer = frame.uses_frame_pointer,
                }) catch return CompilationError.OutOfMemory;
                if (static_helper == null) dwarf_procs.append(allocator, .{
                    .name = symbol_name,
                    .code_start = start + excerpt.entry,
                    .code_size = excerpt.code.len - excerpt.entry,
                    .loc = .none,
                }) catch return CompilationError.OutOfMemory;
            },
            .boxy_thunk, .entrypoint, .message_pool_run, .branch_island => {},
        };
    }
    if (timing) |timings| timings.finish(symbol_relocations_started_ns, .symbol_relocations);

    // Generate entrypoint wrappers
    const entrypoint_instructions_started_ns = if (timing) |timings| timings.start() else 0;
    for (entrypoints) |entrypoint| {
        const export_info = final.generateEntrypointWrapper(
            entrypoint.symbol_name,
            entrypoint.proc,
            entrypoint.arg_layouts,
            entrypoint.ret_layout,
        ) catch return CompilationError.OutOfMemory;

        appendDefinition(allocator, &final.codegen.symbols, &symbols, .{
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
    final.finishImage() catch return CompilationError.OutOfMemory;
    // `ROC_DEV_EXCERPT_ROUNDTRIP` makes every object compile also assemble the
    // program from its own procedure excerpts and panic if the result differs.
    if (dev_diagnostics.isSet(.dev_excerpt_roundtrip)) {
        var fresh = CodeGen.initWithBoxyMetadata(
            allocator,
            lir_store,
            layout_store,
            static_strings.view(),
            erased_arg_desc_offsets,
            erased_arg_desc_params,
            boxy_worker_procs,
            erased_capture_prefix,
            target.cpuLevel(),
        ) catch return CompilationError.OutOfMemory;
        defer fresh.deinit();
        fresh.generation_mode = .object_file;
        fresh.setStaticDataSymbols(static_data_exports) catch return CompilationError.OutOfMemory;
        fresh.enable_default_platform_runtime = enable_default_platform_runtime;
        CodeExcerpt.verifyRoundTrip(CodeGen, allocator, &final, &fresh, proc_specs, layout_store, static_strings.exports) catch |err| switch (err) {
            error.OutOfMemory => return CompilationError.OutOfMemory,
            error.NestedCodeRegion,
            error.UncoveredCode,
            error.DanglingReference,
            error.UnsupportedRelocation,
            error.UnknownProcIdentity,
            error.UnknownRcHelper,
            error.RoundTripMismatch,
            => invariant("dev excerpt round trip failed: {s}", .{@errorName(err)}),
        };
    }
    const code = final.getGeneratedCode();
    const relocations = final.getRelocations();

    var resolved = try resolveObjectSymbols(allocator, &final.codegen.symbols, symbols.items, relocations);
    defer resolved.deinit(allocator);
    if (timing) |timings| timings.finish(symbol_relocations_started_ns, .symbol_relocations);

    const object_bytes: []const u8 = if (emit_object)
        try encodeObject(allocator, lir_store, target, &final, code, rodata.items, zero_fill_size, resolved.symbols, relocations, rodata_relocations.items, dwarf_procs.items, timing)
    else
        &.{};
    errdefer allocator.free(object_bytes);

    var excerpts: ?CodeExcerpt.Set = null;
    if (pack_mode or capture_excerpts) {
        const canonical_data = allocator.alloc(CodeExcerpt.DataItem, canonical.data.len) catch return CompilationError.OutOfMemory;
        defer allocator.free(canonical_data);
        for (canonical.data, canonical_data) |datum, *item| item.* = datum.item;
        excerpts = CodeExcerpt.extract(CodeGen, allocator, &final, proc_specs, layout_store, &.{}, &.{}, canonical_data) catch |err| switch (err) {
            error.OutOfMemory => return CompilationError.OutOfMemory,
            error.NestedCodeRegion, error.UncoveredCode, error.DanglingReference, error.UnsupportedRelocation => invariant("pack excerpt extraction failed: {s}", .{@errorName(err)}),
        };
    }

    return CompilationResult{
        .object_bytes = object_bytes,
        .allocator = allocator,
        .uses_boxy = final.boxy_runtime_used,
        .excerpts = excerpts,
        .runtime_tables = runtime_numbering.takeOrder(),
    };
}

/// Encode the compiled program as an object file with DWARF line tables
/// built from the line entries recorded during code generation.
fn encodeObject(
    allocator: Allocator,
    lir_store: *const LirStore,
    target: RocTarget,
    codegen: anytype,
    code: []const u8,
    rodata: []const u8,
    zero_fill_size: u64,
    symbols: []const ObjectWriter.Symbol,
    relocations: []const IndexedRelocation,
    rodata_relocations: []const ObjectWriter.IndexedDataRelocation,
    dwarf_procs: []const Dwarf.ProcEntry,
    timing: ?*ObjectFileCompiler.Timing,
) CompilationError![]const u8 {
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
        dwarf_procs,
        code.len,
    ) catch return CompilationError.OutOfMemory;
    defer dwarf_sections.deinit(allocator);
    if (timing) |timings| timings.finish(dwarf_started_ns, .dwarf);

    const object_encoding_started_ns = if (timing) |timings| timings.start() else 0;
    var output = std.ArrayList(u8).empty;
    errdefer output.deinit(allocator);

    ObjectWriter.generateIndexedObjectFileWithDebug(
        allocator,
        target,
        code,
        rodata,
        zero_fill_size,
        symbols,
        relocations,
        rodata_relocations,
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
    return object_bytes;
}

const SymbolDefinition = struct {
    id: SymbolTable.Id,
    symbol: ObjectWriter.Symbol,
};

/// An excerpt in a loaded pack.
pub const LocatedExcerpt = struct {
    set: *const CodeExcerpt.Set,
    index: u32,
};

/// The object cache's excerpts, asked by the key an entry was served under
/// (`LIR.LirProcSpec.cache_key`). The consumer that loaded the packs
/// supplies the context and the lookup.
pub const SpliceSource = struct {
    context: *anyopaque,
    find: *const fn (context: *anyopaque, key: [32]u8) ?LocatedExcerpt,
};

/// Place the object-cache entry of each of `external_procs`, with its
/// closure, into the code generator before the program's own procedures
/// compile. Each pack's excerpts are placed once, in the given order. The
/// data items the entries carry are appended to `data_out` for the caller
/// to define.
pub fn spliceExternalProcs(
    comptime CodeGen: type,
    allocator: Allocator,
    codegen: *CodeGen,
    proc_specs: []const LirProcSpec,
    external_procs: []const lir.LIR.LirProcSpecId,
    source: SpliceSource,
    data_out: *std.ArrayList(CodeExcerpt.DataItem),
) Allocator.Error!void {
    var procs_by_identity = std.AutoHashMap(lir.ProcIdentity, lir.LIR.LirProcSpecId).init(allocator);
    defer procs_by_identity.deinit();
    for (proc_specs, 0..) |proc, index| {
        if (proc.is_static_initializer) continue;
        try procs_by_identity.put(proc.identity, @fromBackingInt(@intCast(@as(u32, @intCast(index)))));
    }

    const PackState = struct { placed: std.AutoHashMap(u32, usize) };
    var packs = std.AutoHashMap(*const CodeExcerpt.Set, PackState).init(allocator);
    defer {
        var states = packs.valueIterator();
        while (states.next()) |state| state.placed.deinit();
        packs.deinit();
    }

    for (external_procs) |proc_id| {
        const proc = proc_specs[@backingInt(proc_id)];
        if (!proc.external) invariant("procedure {d} was offered for splicing but is not an object-cache entry", .{@backingInt(proc_id)});
        const key = proc.cache_key orelse invariant("object-cache procedure {d} had no cache key", .{@backingInt(proc_id)});
        const located = source.find(source.context, key) orelse
            invariant("object cache served a specialization whose excerpt {s} is not in any loaded pack", .{&proc.identity.symbolHex()});
        const gop = try packs.getOrPut(located.set);
        if (!gop.found_existing) gop.value_ptr.* = .{ .placed = std.AutoHashMap(u32, usize).init(allocator) };
        try CodeExcerpt.splice(CodeGen, allocator, codegen, located.set, &.{located.index}, &procs_by_identity, &gop.value_ptr.placed, data_out);
    }
}

fn appendDefinition(
    allocator: Allocator,
    table: *SymbolTable.Table,
    definitions: *std.ArrayList(SymbolDefinition),
    symbol: ObjectWriter.Symbol,
) Allocator.Error!void {
    const id = try table.internEmitted(allocator, symbol.name);
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
            symbols[@backingInt(function.symbol)].is_function = true;
        },
        .linked_data, .local_data, .retired, .runtime_number => {},
    };
    for (definitions) |definition| {
        const symbol = &symbols[@backingInt(definition.id)];
        std.debug.assert(symbol.is_external);
        symbol.* = definition.symbol;
    }
    for (table.required_definitions.items) |id| {
        std.debug.assert(!symbols[@backingInt(id)].is_external);
    }
    return .{ .symbols = symbols };
}

fn appendStaticDataExports(
    allocator: Allocator,
    table: *SymbolTable.Table,
    exports: []const StaticDataExport,
    rodata: *std.ArrayList(u8),
    zero_fill_size: *u64,
    relocations: *std.ArrayList(ObjectWriter.IndexedDataRelocation),
    symbols: *std.ArrayList(SymbolDefinition),
) CompilationError!void {
    const data_symbols = allocator.alloc(SymbolTable.Id, exports.len) catch return CompilationError.OutOfMemory;
    defer allocator.free(data_symbols);
    for (exports, data_symbols) |data_export, *id| id.* = table.internEmitted(allocator, data_export.symbol_name) catch return CompilationError.OutOfMemory;
    var functions = collections.DenseMap(lir.LIR.LirProcSpecId, SymbolTable.Id).init(allocator);
    defer functions.deinit();
    var helpers = std.AutoHashMap(layout.RcHelperKey, SymbolTable.Id).init(allocator);
    defer helpers.deinit();
    for (exports, data_symbols) |data_export, definition_id| {
        const start = rodata.items.len;
        try appendStaticDataExport(allocator, data_export, definition_id, rodata, zero_fill_size, symbols);
        // A zero-fill export has no relocations, so this offset is only ever
        // used for exports placed in readonly data.
        const aligned_offset = std.mem.alignForward(usize, start, @intCast(data_export.alignment));
        // A data symbol named by the relocation before is the symbol it
        // interned: a list of copies of one value names it once per item.
        var previous_data: ?struct { name: []const u8, id: SymbolTable.Id } = null;
        for (data_export.relocations) |relocation| {
            const id = switch (relocation.target) {
                .data_symbol => |target| data_symbols[@backingInt(target)],
                .named => blk: {
                    if (relocation.procedure) |proc| {
                        if (functions.get(proc)) |id| break :blk id;
                        const id = table.internEmitted(allocator, relocation.target_symbol_name) catch return CompilationError.OutOfMemory;
                        functions.put(proc, id) catch return CompilationError.OutOfMemory;
                        break :blk id;
                    }
                    if (relocation.rc_helper) |helper| {
                        if (helpers.get(helper)) |id| break :blk id;
                        const id = table.internEmitted(allocator, relocation.target_symbol_name) catch return CompilationError.OutOfMemory;
                        helpers.put(helper, id) catch return CompilationError.OutOfMemory;
                        break :blk id;
                    }
                    if (previous_data) |previous| if (std.mem.eql(u8, previous.name, relocation.target_symbol_name)) break :blk previous.id;
                    const id = table.internEmitted(allocator, relocation.target_symbol_name) catch return CompilationError.OutOfMemory;
                    previous_data = .{ .name = relocation.target_symbol_name, .id = id };
                    break :blk id;
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

/// Whether an export is stored as zero-fill: bytes that are all zero and stay
/// zero after linking, which excludes anything a relocation writes into.
pub fn isZeroFillExport(data_export: StaticDataExport) bool {
    if (data_export.bytes.len == 0 or data_export.relocations.len != 0) return false;
    return std.mem.allEqual(u8, data_export.bytes, 0);
}

fn appendStaticDataExport(
    allocator: Allocator,
    data_export: StaticDataExport,
    id: SymbolTable.Id,
    rodata: *std.ArrayList(u8),
    zero_fill_size: *u64,
    static_data_symbols: *std.ArrayList(SymbolDefinition),
) CompilationError!void {
    const alignment = @as(usize, @intCast(data_export.alignment));
    const symbol_offset: usize = @intCast(data_export.symbol_offset);
    if (builtin.mode == .debug and symbol_offset > data_export.bytes.len) {
        invariant(
            "ObjectFileCompiler invariant violated: static data symbol offset {d} exceeds byte length {d}",
            .{ data_export.symbol_offset, data_export.bytes.len },
        );
    }
    if (isZeroFillExport(data_export)) {
        // The object declares the extent and stores none of the zeros.
        const zero_fill_offset = std.mem.alignForward(u64, zero_fill_size.*, alignment);
        zero_fill_size.* = zero_fill_offset + data_export.bytes.len;
        static_data_symbols.append(allocator, .{ .id = id, .symbol = .{
            .name = data_export.symbol_name,
            .offset = zero_fill_offset + symbol_offset,
            .size = data_export.bytes.len - symbol_offset,
            .is_global = data_export.is_global,
            .is_function = false,
            .is_external = false,
            .is_hidden = !data_export.is_exported,
            .section = .zero_fill,
        } }) catch {
            return CompilationError.OutOfMemory;
        };
        return;
    }
    const aligned_offset = std.mem.alignForward(usize, rodata.items.len, alignment);
    rodata.appendNTimes(allocator, 0, aligned_offset - rodata.items.len) catch {
        return CompilationError.OutOfMemory;
    };
    rodata.appendSlice(allocator, data_export.bytes) catch {
        return CompilationError.OutOfMemory;
    };

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
    var zero_fill_size: u64 = 0;

    try appendStaticDataExports(allocator, &table, static_data_exports, &rodata, &zero_fill_size, &rodata_relocations, &symbols);

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
        zero_fill_size,
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
    erased_capture_prefix: u32,
    timing: ?*ObjectFileCompiler.Timing,
    pack_mode: bool,
    splice_source: ?SpliceSource,
    capture_excerpts: bool,
    emit_object: bool,
    executor: ?Executor,
    reuse_same_program: ?*const NativeProcCompiler.Retained,
) CompilationError!CompilationResult {
    const enum_info = @typeInfo(RocTarget).@"enum";
    const default_target = target.defaultCpuTarget();
    inline for (enum_info.field_values) |field_value| {
        const comptime_target: RocTarget = @fromBackingInt(@intCast(field_value));
        if (comptime comptime_target.defaultCpuTarget() != comptime_target) continue;
        if (default_target == comptime_target) {
            if (comptime isaOfOrNull(comptime_target) != null) {
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
                    erased_capture_prefix,
                    timing,
                    pack_mode,
                    splice_source,
                    capture_excerpts,
                    emit_object,
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
        try std.testing.expect(result.excerpts != null);
        try std.testing.expect(result.excerpts.?.excerpts.len > 0);
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
    // The native target joins the fixed ones only where native LIR codegen exists.
    const native_targets = if (LirCodeGenMod.host_lir_codegen_available) .{comptime RocTarget.detectNative()} else .{};
    inline for (.{ RocTarget.x64linux, RocTarget.arm64linux } ++ native_targets) |target| {
        var result = producer: {
            var arena = collections.SingleThreadArena.init(allocator);
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
                .value = .{ .static_data = .{ .id = @fromBackingInt(@intCast(7)) } },
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
                .target_symbol_name = try a.dupe(u8, "roc__d7_1"),
                .target = .{ .data_symbol = @fromBackingInt(@intCast(1)) },
                .addend = 8,
            };
            exports[0] = .{
                .symbol_name = try a.dupe(u8, "roc__d7"),
                .value_id = @fromBackingInt(@intCast(7)),
                .bytes = descriptor,
                .alignment = 8,
                .is_exported = false,
                .relocations = relocation,
            };
            exports[1] = .{
                .symbol_name = try a.dupe(u8, "roc__d7_1"),
                .bytes = backing,
                .symbol_offset = 8,
                .alignment = 8,
                .is_exported = false,
            };
            exports[2] = .{
                .symbol_name = try a.dupe(u8, "roc__d8"),
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
            break :serialized try PackFile.write(allocator, &result.excerpts.?, &.{});
        };
        defer allocator.free(bytes);
        var pack = try PackFile.read(allocator, bytes);
        defer pack.deinit();
        try std.testing.expectEqual(@as(usize, 1), pack.set.excerpts.len);
        const excerpt = pack.set.excerpts[0];
        try std.testing.expectEqual(@as(usize, 2), excerpt.data.len);
        for (excerpt.data) |item| {
            try std.testing.expect(std.mem.startsWith(u8, item.name, CodeExcerpt.content_data_prefix));
            try std.testing.expect(!std.mem.eql(u8, item.name, "roc__d7"));
            try std.testing.expect(!std.mem.eql(u8, item.name, "roc__d7_1"));
            for (item.relocations) |relocation| {
                try std.testing.expect(!relocation.external);
                try std.testing.expect(std.mem.startsWith(u8, relocation.name, CodeExcerpt.content_data_prefix));
            }
        }
        var data_references: usize = 0;
        for (excerpt.relocations) |relocation| {
            if (relocation.kind != .data) continue;
            data_references += 1;
            try std.testing.expect(std.mem.startsWith(u8, relocation.name, CodeExcerpt.content_data_prefix));
        }
        try std.testing.expect(data_references > 0);
        // A different consumer has neither the procedure ordinal nor any static table.
        var store = LirStore.init(allocator);
        defer store.deinit();
        // The consumer has the module versions the pack's code came from.
        const files = try allocator.alloc(@import("base").SourceFileEntry, pack.set.source_files.len);
        defer allocator.free(files);
        for (files, pack.set.source_files) |*file, module_identity| file.* = .{ .name = "module", .qualified_name = "module", .module_identity = module_identity };
        try store.setSourceFiles(files);
        var layouts = try layout.Store.init(allocator, .u64);
        defer layouts.deinit();
        const CG = LirCodeGenMod.LirCodeGen(target);
        var receiver = try CG.init(allocator, &store, &layouts, .{}, &.{}, .default);
        defer receiver.deinit();
        var procs = std.AutoHashMap(lir.ProcIdentity, lir.LIR.LirProcSpecId).init(allocator);
        defer procs.deinit();
        var placed = std.AutoHashMap(u32, usize).init(allocator);
        defer placed.deinit();
        var data = std.ArrayList(CodeExcerpt.DataItem).empty;
        defer data.deinit(allocator);
        try CodeExcerpt.splice(CG, allocator, &receiver, &pack.set, &.{0}, &procs, &placed, &data);
        try receiver.finishImage();
        if (comptime target == RocTarget.detectNative()) {
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
            executable.callRocABIAt(placed.get(0).? + excerpt.entry, @ptrCast(&output), null);
            try std.testing.expectEqual(text.len, output[2]);
            const value: [*]const u8 = @ptrFromInt(output[0]);
            try std.testing.expectEqualStrings(text, value[0..output[2]]);
        }
    }
}
