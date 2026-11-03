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
const CodeLocations = @import("CodeLocations.zig");
const NativeProcCompiler = @import("NativeProcCompiler.zig");
const Executor = @import("base").post_check_task_executor.Executor;
const static_data_export = @import("StaticDataExport.zig");
const collections = @import("collections");
const SymbolTable = @import("SymbolTable.zig");
const IndexedRelocation = @import("Relocation.zig").IndexedRelocation;
const StaticStringData = @import("StaticStringData.zig");
const UnitPieces = @import("UnitPieces.zig");
const TargetUsize = @import("base").target.TargetUsize;

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
    /// The compiled program lifted into excerpts, when the compile ran in
    /// capture mode on a target whose excerpts can be extracted.
    excerpts: ?CodeExcerpt.Set = null,
    /// The object's encoded `CodeLocations`, when the compile captured them.
    code_locations: []const u8 = &.{},

    pub fn deinit(self: *CompilationResult) void {
        self.allocator.free(self.object_bytes);
        self.allocator.free(self.code_locations);
        if (self.excerpts) |*set| set.deinit();
    }
};

/// What a composition keeps and how it names what it keeps.
pub const ComposeOptions = struct {
    enable_default_platform_runtime: bool = false,
    /// The procedures the composition starts its walk from, beside every
    /// entrypoint wrapper and host-visible constant. Null keeps every piece.
    roots: ?[]const lir.ProcIdentity = null,
    /// Give every procedure a global symbol, as an image the compiler loads
    /// into its own process looks procedures up by name.
    global_procs: bool = false,
    capture_code_locations: bool = false,
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
    /// without host entrypoints, for callers inspecting all compiled excerpts.
    capture_all_procedures: bool = false,
    /// The object is one unit of a multi-object link: it binds what its
    /// module exports, which may be nothing at all, and is emitted either
    /// way.
    link_unit: bool = false,
    /// Lift the compiled program into excerpts and keep them in
    /// `captured_excerpts` after `compileToObjectFileAndWrite`.
    capture_excerpts: bool = false,
    captured_excerpts: ?CodeExcerpt.Set = null,
    /// Keep the object's `CodeLocations` in the result, for an object a
    /// compile-time program may run.
    capture_code_locations: bool = false,
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

    /// Capture all procedure excerpts: every procedure is a global symbol and the
    /// object is emitted even though no host entrypoint requests it.
    pub fn initForExcerpts(allocator: Allocator) ObjectFileCompiler {
        return .{ .allocator = allocator, .capture_all_procedures = true };
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
        target: RocTarget,
    ) CompilationError!CompilationResult {
        return crossCompileDispatch(self.allocator, lir_store, layout_store, entrypoints, static_data_exports, proc_specs, target, self.enable_default_platform_runtime, self.erased_capture_prefix, self.timing, self.capture_all_procedures, self.link_unit, self.capture_excerpts, self.capture_code_locations, self.emit_object, self.post_check_executor, self.reuse_same_program);
    }

    /// Compile to an object file and write it to a path.
    /// Compile `proc_specs` to a unit's pieces (design.md "Unit Pieces And
    /// Whole-Program Dead Code"): every procedure, helper and entrypoint
    /// wrapper lifted out of the unit's code as its own excerpt, with the
    /// constants it names under their content names. No object is encoded.
    pub fn compileToPieces(
        self: *ObjectFileCompiler,
        lir_store: *const LirStore,
        layout_store: *const layout.Store,
        entrypoints: []const Entrypoint,
        static_data_exports: []const StaticDataExport,
        proc_specs: []const LirProcSpec,
        target: RocTarget,
    ) CompilationError!UnitPieces.Pieces {
        const enum_info = @typeInfo(RocTarget).@"enum";
        const default_target = target.defaultCpuTarget();
        inline for (enum_info.field_values) |field_value| {
            const comptime_target: RocTarget = @fromBackingInt(@intCast(field_value));
            if (comptime comptime_target.defaultCpuTarget() != comptime_target) continue;
            if (default_target == comptime_target) {
                if (comptime isaOfOrNull(comptime_target) != null) {
                    return compilePiecesWithCodeGen(
                        LirCodeGenMod.LirCodeGen(comptime_target),
                        self.allocator,
                        lir_store,
                        layout_store,
                        entrypoints,
                        static_data_exports,
                        proc_specs,
                        target,
                        self.enable_default_platform_runtime,
                        self.erased_capture_prefix,
                        self.timing,
                        self.post_check_executor,
                    );
                } else {
                    return CompilationError.UnsupportedTarget;
                }
            }
        }
        return CompilationError.UnsupportedTarget;
    }

    /// Compose the pieces of a program's units into one relocatable object
    /// holding only what `options.roots` reach (design.md "Unit Pieces And
    /// Whole-Program Dead Code").
    pub fn composeObject(
        self: *ObjectFileCompiler,
        inputs: []const *const UnitPieces.Pieces,
        target: RocTarget,
        options: ComposeOptions,
    ) CompilationError!CompilationResult {
        const enum_info = @typeInfo(RocTarget).@"enum";
        const default_target = target.defaultCpuTarget();
        inline for (enum_info.field_values) |field_value| {
            const comptime_target: RocTarget = @fromBackingInt(@intCast(field_value));
            if (comptime comptime_target.defaultCpuTarget() != comptime_target) continue;
            if (default_target == comptime_target) {
                if (comptime isaOfOrNull(comptime_target) != null) {
                    return composeWithCodeGen(LirCodeGenMod.LirCodeGen(comptime_target), self.allocator, inputs, target, options, self.timing);
                } else {
                    return CompilationError.UnsupportedTarget;
                }
            }
        }
        return CompilationError.UnsupportedTarget;
    }

    pub fn compileToObjectFileAndWrite(
        self: *ObjectFileCompiler,
        lir_store: *const LirStore,
        layout_store: *const layout.Store,
        entrypoints: []const Entrypoint,
        static_data_exports: []const StaticDataExport,
        proc_specs: []const LirProcSpec,
        target: RocTarget,
        output_path: []const u8,
        roc_ctx: CoreCtx,
    ) CompilationError!void {
        var result = try self.compileToObjectFile(
            lir_store,
            layout_store,
            entrypoints,
            static_data_exports,
            proc_specs,
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
    target: RocTarget,
    enable_default_platform_runtime: bool,
    erased_capture_prefix: u32,
    timing: ?*ObjectFileCompiler.Timing,
    capture_all_procedures: bool,
    link_unit: bool,
    capture_excerpts: bool,
    capture_code_locations: bool,
    emit_object: bool,
    executor: ?Executor,
    reuse_same_program: ?*const NativeProcCompiler.Retained,
) CompilationError!CompilationResult {
    const exports_procs = for (proc_specs) |proc| {
        if (proc.exported) break true;
    } else false;
    if (!capture_all_procedures and !link_unit and entrypoints.len == 0 and static_data_exports.len == 0 and !exports_procs) {
        return CompilationError.NoEntrypoints;
    }

    const backend_setup_started_ns = if (timing) |timings| timings.start() else 0;
    var static_strings = StaticStringData.build(allocator, lir_store, target) catch {
        return CompilationError.OutOfMemory;
    };
    defer static_strings.deinit();

    // Initialize the code generator
    var codegen = CodeGen.initWithCapturePrefix(
        allocator,
        lir_store,
        layout_store,
        static_strings.view(),
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
    var demand = std.ArrayList(lir.LIR.LirProcSpecId).empty;
    defer demand.deinit(allocator);
    for (proc_specs, 0..) |proc, index| {
        // A procedure another object of the link defines is a symbol here,
        // never code (`LIR.LirProcSpec.linked`).
        if (proc.is_static_initializer or proc.linked or (proc.external)) continue;
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
    // and optional excerpt capture have finished.
    defer retained.deinit();
    const native_data = retained.dataItems(allocator) catch return CompilationError.OutOfMemory;
    defer allocator.free(native_data);
    if (timing) |timings| timings.snapshot_value.native_emission.add(native_metrics);
    if (timing) |timings| timings.finish(procedure_instructions_started_ns, .procedure_instructions);

    // Lay the program's code out canonically, so that whether each procedure
    // was compiled serially or in parallel, its emission order changes
    // nothing in the object (design.md
    // "Deterministic Dev Images"). The code generated so far is lifted into
    // excerpts and placed again, in arranged order and under canonical
    // names, into the code generator the object is encoded from.
    var symbol_relocations_started_ns = if (timing) |timings| timings.start() else 0;
    codegen.finishImage() catch return CompilationError.OutOfMemory;
    var catalog = CodeExcerpt.PreparedData.init(allocator, static_strings.exports, static_data_exports, native_data) catch return CompilationError.OutOfMemory;
    defer catalog.deinit();
    var staged = CodeExcerpt.extractCataloged(CodeGen, allocator, &codegen, proc_specs, layout_store, &catalog) catch |err| switch (err) {
        error.OutOfMemory => return CompilationError.OutOfMemory,
        error.NestedCodeRegion, error.UncoveredCode, error.DanglingReference, error.UnsupportedRelocation => invariant("arranged image extraction failed: {s}", .{@errorName(err)}),
    };
    defer staged.deinit();
    var host_visible = std.ArrayList(CodeExcerpt.DataItem).empty;
    defer host_visible.deinit(allocator);
    // The constants the host names. Only these bind globally: the rest of
    // the host-visible data is reached from them by relocation within this
    // object, and a content-named datum another object of the link also
    // carries must not clash with it.
    var exported_roots = std.StringHashMap(void).init(allocator);
    defer exported_roots.deinit();
    {
        var roots = std.ArrayList([]const u8).empty;
        defer roots.deinit(allocator);
        for (static_data_exports) |data_export| {
            if (!data_export.is_exported) continue;
            roots.append(allocator, data_export.symbol_name) catch return CompilationError.OutOfMemory;
            exported_roots.put(data_export.symbol_name, {}) catch return CompilationError.OutOfMemory;
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
    // procedures were emitted; excerpt capture retains every procedure.
    var reachable: ?CodeExcerpt.Set = null;
    defer if (reachable) |*retained_set| retained_set.deinit();
    if (!capture_all_procedures) {
        // The code the program's roots reach: its entrypoints', and that of
        // every procedure it defines for other objects of its link.
        var root_procs = std.ArrayList(lir.ProcIdentity).empty;
        defer root_procs.deinit(allocator);
        for (entrypoints) |entrypoint| root_procs.append(allocator, proc_specs[@backingInt(entrypoint.proc)].identity) catch return CompilationError.OutOfMemory;
        for (proc_specs) |proc| {
            if (proc.exported) root_procs.append(allocator, proc.identity) catch return CompilationError.OutOfMemory;
        }
        var root_data = std.ArrayList([]const u8).empty;
        defer root_data.deinit(allocator);
        for (host_visible.items) |item| root_data.append(allocator, item.name) catch return CompilationError.OutOfMemory;
        reachable = CodeExcerpt.retainReachable(allocator, &staged, &catalog, .{
            .procs = root_procs.items,
            .data = root_data.items,
            .helper_aliases = &helper_aliases,
        }) catch return CompilationError.OutOfMemory;
    }
    var canonical = CodeExcerpt.arrange(allocator, if (reachable) |*retained_set| retained_set else &staged, host_visible.items) catch return CompilationError.OutOfMemory;
    defer canonical.deinit();

    var final = CodeGen.initWithCapturePrefix(
        allocator,
        lir_store,
        layout_store,
        static_strings.view(),
        erased_capture_prefix,
        target.cpuLevel(),
    ) catch return CompilationError.OutOfMemory;
    defer final.deinit();
    final.generation_mode = .object_file;
    final.enable_default_platform_runtime = enable_default_platform_runtime;

    var procs_by_identity = std.AutoHashMap(lir.ProcIdentity, lir.LIR.LirProcSpecId).init(allocator);
    defer procs_by_identity.deinit();
    for (proc_specs, 0..) |proc, index| {
        if (proc.is_static_initializer or proc.linked) continue;
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
            const exported = datum.host_visible and exported_roots.contains(item.name);
            out.* = .{
                .symbol_name = item.name,
                .bytes = item.bytes,
                .symbol_offset = item.symbol_offset,
                .alignment = item.alignment,
                .is_global = exported,
                .is_exported = exported,
                .is_mutable = datum.item.is_mutable,
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
                // A procedure the program defines for other objects of its
                // link (`LIR.LirProcSpec.exported`) binds globally.
                const exported = if (procs_by_identity.get(identity)) |proc_id| proc_specs[@backingInt(proc_id)].exported else false;
                appendDefinition(allocator, &final.codegen.symbols, &symbols, .{
                    .name = symbol_name,
                    .offset = start,
                    .size = excerpt.code.len,
                    .is_global = capture_all_procedures or exported,
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
            .entrypoint, .message_pool_run, .branch_island => {},
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
        var fresh = CodeGen.initWithCapturePrefix(
            allocator,
            lir_store,
            layout_store,
            static_strings.view(),
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
    if (capture_all_procedures or capture_excerpts) {
        const canonical_data = allocator.alloc(CodeExcerpt.DataItem, canonical.data.len) catch return CompilationError.OutOfMemory;
        defer allocator.free(canonical_data);
        for (canonical.data, canonical_data) |datum, *item| item.* = datum.item;
        excerpts = CodeExcerpt.extract(CodeGen, allocator, &final, proc_specs, layout_store, &.{}, &.{}, canonical_data) catch |err| switch (err) {
            error.OutOfMemory => return CompilationError.OutOfMemory,
            error.NestedCodeRegion, error.UncoveredCode, error.DanglingReference, error.UnsupportedRelocation => invariant("native excerpt extraction failed: {s}", .{@errorName(err)}),
        };
    }

    errdefer if (excerpts) |*set| set.deinit();
    return CompilationResult{
        .object_bytes = object_bytes,
        .allocator = allocator,
        .excerpts = excerpts,
        .code_locations = if (capture_code_locations) try encodeCodeLocations(allocator, lir_store, &final) else &.{},
    };
}

/// The `CodeLocations` of a finished code generator's code.
fn encodeCodeLocations(allocator: Allocator, lir_store: *const LirStore, codegen: anytype) Allocator.Error![]const u8 {
    const code_regions = codegen.codeRegions();
    const regions = try allocator.alloc(CodeLocations.CodeRange, code_regions.len);
    defer allocator.free(regions);
    for (code_regions, regions) |source, *region| region.* = .{ .start = @intCast(source.start), .end = @intCast(source.end) };
    const line_entries = codegen.getLineEntries();
    const lines = try allocator.alloc(CodeLocations.Line, line_entries.len);
    defer allocator.free(lines);
    for (line_entries, lines) |source, *line| line.* = .{ .offset = source.offset, .loc = source.loc, .region = source.region };
    const failure_sites = codegen.getFailureSites();
    const sites = try allocator.alloc(CodeLocations.Site, failure_sites.len);
    defer allocator.free(sites);
    for (failure_sites, sites) |source, *site| site.* = .{ .offset = source.offset, .meaning = source.meaning };
    const files = try allocator.alloc(@import("base").SourceFileEntry, lir_store.sourceFileCount());
    defer allocator.free(files);
    for (files, 0..) |*file, index| file.* = .{
        .name = lir_store.sourceFileName(@intCast(index)),
        .qualified_name = lir_store.sourceFileQualifiedName(@intCast(index)),
        .module_identity = lir_store.sourceFileModuleIdentity(@intCast(index)),
    };
    return try CodeLocations.encode(allocator, .{ .regions = regions, .lines = lines, .sites = sites, .files = files });
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
        .linked_data, .local_data, .retired => {},
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
    std.debug.assert(!data_export.is_mutable or (data_export.relocations.len == 0 and std.mem.allEqual(u8, data_export.bytes, 0)));
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
fn compilePiecesWithCodeGen(
    comptime CodeGen: type,
    allocator: Allocator,
    lir_store: *const LirStore,
    layout_store: *const layout.Store,
    entrypoints: []const Entrypoint,
    static_data_exports: []const StaticDataExport,
    proc_specs: []const LirProcSpec,
    target: RocTarget,
    enable_default_platform_runtime: bool,
    erased_capture_prefix: u32,
    timing: ?*ObjectFileCompiler.Timing,
    executor: ?Executor,
) CompilationError!UnitPieces.Pieces {
    const backend_setup_started_ns = if (timing) |timings| timings.start() else 0;
    var static_strings = StaticStringData.build(allocator, lir_store, target) catch return CompilationError.OutOfMemory;
    defer static_strings.deinit();
    var codegen = CodeGen.initWithCapturePrefix(
        allocator,
        lir_store,
        layout_store,
        static_strings.view(),
        erased_capture_prefix,
        target.cpuLevel(),
    ) catch return CompilationError.OutOfMemory;
    defer codegen.deinit();
    codegen.generation_mode = .object_file;
    codegen.setStaticDataSymbols(static_data_exports) catch return CompilationError.OutOfMemory;
    codegen.enable_default_platform_runtime = enable_default_platform_runtime;
    const static_rc_helpers = static_data_export.collectRequiredRcHelpers(allocator, static_data_exports) catch return CompilationError.OutOfMemory;
    defer allocator.free(static_rc_helpers);
    if (timing) |timings| timings.finish(backend_setup_started_ns, .backend_setup);
    const procedure_instructions_started_ns = if (timing) |timings| timings.start() else 0;
    var demand = std.ArrayList(lir.LIR.LirProcSpecId).empty;
    defer demand.deinit(allocator);
    for (proc_specs, 0..) |proc, index| {
        if (proc.is_static_initializer or proc.linked or (proc.external)) continue;
        demand.append(allocator, @fromBackingInt(@intCast(index))) catch return CompilationError.OutOfMemory;
    }
    var native_metrics: NativeProcCompiler.Metrics = .{};
    var retained = NativeProcCompiler.run(CodeGen, allocator, &codegen, demand.items, .{
        .target = target,
        .executor = executor,
        .metrics_out = &native_metrics,
        .static_helpers = static_rc_helpers,
        .constant_exports = static_data_exports,
    }) catch return CompilationError.OutOfMemory;
    defer retained.deinit();
    if (timing) |timings| timings.snapshot_value.native_emission.add(native_metrics);
    if (timing) |timings| timings.finish(procedure_instructions_started_ns, .procedure_instructions);
    // Entrypoint wrappers join the unit's image here, so they are lifted
    // out as pieces named by their symbols.
    const entrypoint_instructions_started_ns = if (timing) |timings| timings.start() else 0;
    for (entrypoints) |entrypoint| {
        _ = codegen.generateEntrypointWrapper(entrypoint.symbol_name, entrypoint.proc, entrypoint.arg_layouts, entrypoint.ret_layout) catch return CompilationError.OutOfMemory;
    }
    if (timing) |timings| timings.finish(entrypoint_instructions_started_ns, .entrypoint_instructions);
    const extraction_started_ns = if (timing) |timings| timings.start() else 0;
    codegen.finishImage() catch return CompilationError.OutOfMemory;
    var catalog = CodeExcerpt.PreparedData.init(allocator, static_strings.exports, static_data_exports, &.{}) catch return CompilationError.OutOfMemory;
    defer catalog.deinit();
    var staged = CodeExcerpt.extractCataloged(CodeGen, allocator, &codegen, proc_specs, layout_store, &catalog) catch |err| switch (err) {
        error.OutOfMemory => return CompilationError.OutOfMemory,
        error.NestedCodeRegion, error.UncoveredCode, error.DanglingReference, error.UnsupportedRelocation => invariant("unit piece extraction failed: {s}", .{@errorName(err)}),
    };
    defer staged.deinit();
    // The unit's constants under their content names, so pieces of
    // different units can meet in one program.
    var named = CodeExcerpt.combineDistinct(allocator, &.{&staged}) catch return CompilationError.OutOfMemory;
    errdefer named.deinit();
    var arena = collections.SingleThreadArena.init(allocator);
    errdefer arena.deinit();
    const a = arena.allocator();
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
    const exported_data = a.alloc(CodeExcerpt.DataItem, host_visible.items.len) catch return CompilationError.OutOfMemory;
    for (host_visible.items, exported_data) |item, *owned| owned.* = ownDataItem(a, item) catch return CompilationError.OutOfMemory;
    const helper_aliases = a.alloc(UnitPieces.HelperAlias, static_rc_helpers.len) catch return CompilationError.OutOfMemory;
    for (static_rc_helpers, helper_aliases) |helper_key, *alias| {
        alias.* = .{
            .helper = LirCodeGenMod.compiledRcHelperSymbolName(a, layout_store, LirCodeGenMod.staticDataRcHelperKey(helper_key)) catch return CompilationError.OutOfMemory,
            .alias = static_data_export.atomicRcHelperSymbolName(a, layout_store, helper_key) catch return CompilationError.OutOfMemory,
        };
    }
    const source_files = a.alloc(UnitPieces.SourceFile, named.source_files.len) catch return CompilationError.OutOfMemory;
    for (named.source_files, source_files, 0..) |identity, *file, index| {
        std.debug.assert(std.mem.eql(u8, &identity, &lir_store.sourceFileModuleIdentity(@intCast(index))));
        file.* = .{
            .name = a.dupe(u8, lir_store.sourceFileName(@intCast(index))) catch return CompilationError.OutOfMemory,
            .qualified_name = a.dupe(u8, lir_store.sourceFileQualifiedName(@intCast(index))) catch return CompilationError.OutOfMemory,
            .identity = identity,
        };
    }
    if (timing) |timings| timings.finish(extraction_started_ns, .symbol_relocations);
    return .{
        .arena = arena,
        .set = named,
        .erased_capture_prefix = erased_capture_prefix,
        .source_files = source_files,
        .exported_data = exported_data,
        .helper_aliases = helper_aliases,
    };
}

/// A copy of `item` and its relocations, owned by `a`.
fn ownDataItem(a: Allocator, item: CodeExcerpt.DataItem) Allocator.Error!CodeExcerpt.DataItem {
    const relocations = try a.alloc(CodeExcerpt.DataRelocation, item.relocations.len);
    for (item.relocations, relocations) |relocation, *out| {
        out.* = relocation;
        out.name = try a.dupe(u8, relocation.name);
    }
    return .{
        .name = try a.dupe(u8, item.name),
        .bytes = try a.dupe(u8, item.bytes),
        .alignment = item.alignment,
        .symbol_offset = item.symbol_offset,
        .relocations = relocations,
        .program_local_name = item.program_local_name,
        .is_mutable = item.is_mutable,
    };
}

fn composeWithCodeGen(
    comptime CodeGen: type,
    allocator: Allocator,
    inputs: []const *const UnitPieces.Pieces,
    target: RocTarget,
    options: ComposeOptions,
    timing: ?*ObjectFileCompiler.Timing,
) CompilationError!CompilationResult {
    const symbol_relocations_started_ns = if (timing) |timings| timings.start() else 0;
    // The object is a function of the units, not of the order they arrive
    // in: the units are taken in the order of their source-file tables, and
    // the host-visible constants in name order.
    const ordered = allocator.alloc(*const UnitPieces.Pieces, inputs.len) catch return CompilationError.OutOfMemory;
    defer allocator.free(ordered);
    @memcpy(ordered, inputs);
    std.mem.sort(*const UnitPieces.Pieces, ordered, {}, struct {
        fn lessThan(_: void, a: *const UnitPieces.Pieces, b: *const UnitPieces.Pieces) bool {
            const common = @min(a.set.source_files.len, b.set.source_files.len);
            for (a.set.source_files[0..common], b.set.source_files[0..common]) |left, right| {
                switch (std.mem.order(u8, &left, &right)) {
                    .lt => return true,
                    .gt => return false,
                    .eq => {},
                }
            }
            return a.set.source_files.len < b.set.source_files.len;
        }
    }.lessThan);
    const sets = allocator.alloc(*const CodeExcerpt.Set, ordered.len) catch return CompilationError.OutOfMemory;
    defer allocator.free(sets);
    for (ordered, sets) |pieces, *set| set.* = &pieces.set;
    const erased_capture_prefix: u32 = if (inputs.len == 0) 0 else inputs[0].erased_capture_prefix;
    for (inputs) |pieces| {
        if (pieces.erased_capture_prefix != erased_capture_prefix) invariant("composed units were generated for different capture prefixes", .{});
    }
    var host_visible = std.ArrayList(CodeExcerpt.DataItem).empty;
    defer host_visible.deinit(allocator);
    var exported_roots = std.StringHashMap(void).init(allocator);
    defer exported_roots.deinit();
    {
        var seen = std.StringHashMap(void).init(allocator);
        defer seen.deinit();
        for (ordered) |pieces| for (pieces.exported_data) |item| {
            if ((seen.getOrPut(item.name) catch return CompilationError.OutOfMemory).found_existing) continue;
            host_visible.append(allocator, item) catch return CompilationError.OutOfMemory;
            if (!item.program_local_name) exported_roots.put(item.name, {}) catch return CompilationError.OutOfMemory;
        };
        std.mem.sort(CodeExcerpt.DataItem, host_visible.items, {}, struct {
            fn lessThan(_: void, a: CodeExcerpt.DataItem, b: CodeExcerpt.DataItem) bool {
                return std.mem.lessThan(u8, a.name, b.name);
            }
        }.lessThan);
    }
    // Static data reaches refcount helpers under their atomic aliases.
    var helper_aliases = std.StringHashMap([]const u8).init(allocator);
    defer helper_aliases.deinit();
    var alias_of_helper = std.StringHashMap([]const u8).init(allocator);
    defer alias_of_helper.deinit();
    for (inputs) |pieces| for (pieces.helper_aliases) |alias| {
        helper_aliases.put(alias.alias, alias.helper) catch return CompilationError.OutOfMemory;
        alias_of_helper.put(alias.helper, alias.alias) catch return CompilationError.OutOfMemory;
    };
    var root_procs = std.ArrayList(lir.ProcIdentity).empty;
    defer root_procs.deinit(allocator);
    if (options.roots) |roots| {
        root_procs.appendSlice(allocator, roots) catch return CompilationError.OutOfMemory;
    } else for (sets) |set| for (set.excerpts) |excerpt| switch (excerpt.kind) {
        .proc => |identity| root_procs.append(allocator, identity) catch return CompilationError.OutOfMemory,
        .rc_helper, .entrypoint, .message_pool_run, .branch_island => {},
    };
    var root_data = std.ArrayList([]const u8).empty;
    defer root_data.deinit(allocator);
    for (host_visible.items) |item| root_data.append(allocator, item.name) catch return CompilationError.OutOfMemory;
    var reachable = CodeExcerpt.reachableAcross(allocator, sets, host_visible.items, .{
        .procs = root_procs.items,
        .data = root_data.items,
        .helper_aliases = &helper_aliases,
        .entrypoints = true,
    }) catch return CompilationError.OutOfMemory;
    defer reachable.deinit();
    // The source-file table of the reached code, named from the units that
    // index it.
    const files = allocator.alloc(@import("base").SourceFileEntry, reachable.source_files.len) catch return CompilationError.OutOfMemory;
    defer allocator.free(files);
    for (reachable.source_files, files) |identity, *file| {
        file.* = named: {
            for (inputs) |pieces| for (pieces.source_files) |source| {
                if (std.mem.eql(u8, &source.identity, &identity)) break :named .{ .name = source.name, .qualified_name = source.qualified_name, .module_identity = identity };
            };
            invariant("a composed source file belongs to no unit", .{});
        };
    }
    var canonical = CodeExcerpt.arrange(allocator, &reachable, host_visible.items) catch return CompilationError.OutOfMemory;
    defer canonical.deinit();
    var store = LirStore.init(allocator);
    defer store.deinit();
    store.setSourceFiles(files) catch return CompilationError.OutOfMemory;
    var layouts = layout.Store.init(allocator, TargetUsize.fromPtrBitWidth(target.ptrBitWidth())) catch return CompilationError.OutOfMemory;
    defer layouts.deinit();
    var static_strings = StaticStringData.build(allocator, &store, target) catch return CompilationError.OutOfMemory;
    defer static_strings.deinit();
    var final = CodeGen.initWithCapturePrefix(
        allocator,
        &store,
        &layouts,
        static_strings.view(),
        erased_capture_prefix,
        target.cpuLevel(),
    ) catch return CompilationError.OutOfMemory;
    defer final.deinit();
    final.generation_mode = .object_file;
    final.enable_default_platform_runtime = options.enable_default_platform_runtime;
    var procs_by_identity = std.AutoHashMap(lir.ProcIdentity, lir.LIR.LirProcSpecId).init(allocator);
    defer procs_by_identity.deinit();
    var helper_keys = CodeExcerpt.HelperKeys.init(allocator);
    defer helper_keys.deinit();
    const starts = CodeExcerpt.assembleArranged(CodeGen, allocator, &final, &canonical.set, &procs_by_identity, &helper_keys) catch return CompilationError.OutOfMemory;
    defer allocator.free(starts);
    var symbols = std.ArrayList(SymbolDefinition).empty;
    defer symbols.deinit(allocator);
    var rodata = std.ArrayList(u8).empty;
    defer rodata.deinit(allocator);
    var rodata_relocations = std.ArrayList(ObjectWriter.IndexedDataRelocation).empty;
    defer rodata_relocations.deinit(allocator);
    var zero_fill_size: u64 = 0;
    var owned_symbol_names = std.ArrayList([]u8).empty;
    defer {
        for (owned_symbol_names.items) |name| allocator.free(name);
        owned_symbol_names.deinit(allocator);
    }
    var dwarf_procs = std.ArrayList(Dwarf.ProcEntry).empty;
    defer dwarf_procs.deinit(allocator);
    {
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
            const exported = datum.host_visible and exported_roots.contains(item.name);
            out.* = .{
                .symbol_name = item.name,
                .bytes = item.bytes,
                .symbol_offset = item.symbol_offset,
                .alignment = item.alignment,
                .is_global = exported,
                .is_exported = exported,
                .is_mutable = datum.item.is_mutable,
                .relocations = relocations,
            };
        }
        try appendStaticDataExports(allocator, &final.codegen.symbols, data_exports, &rodata, &zero_fill_size, &rodata_relocations, &symbols);
    }
    for (canonical.set.excerpts, starts) |excerpt, start| switch (excerpt.kind) {
        .proc => |identity| {
            const frame = excerpt.frame orelse invariant("a procedure excerpt had no frame", .{});
            const symbol_name = static_data_export.procSymbolName(allocator, identity) catch return CompilationError.OutOfMemory;
            owned_symbol_names.append(allocator, symbol_name) catch {
                allocator.free(symbol_name);
                return CompilationError.OutOfMemory;
            };
            appendDefinition(allocator, &final.codegen.symbols, &symbols, .{
                .name = symbol_name,
                .offset = start,
                .size = excerpt.code.len,
                .is_global = options.global_procs,
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
            const frame = excerpt.frame orelse continue;
            const alias = alias_of_helper.get(name);
            const symbol_name = allocator.dupe(u8, alias orelse name) catch return CompilationError.OutOfMemory;
            owned_symbol_names.append(allocator, symbol_name) catch {
                allocator.free(symbol_name);
                return CompilationError.OutOfMemory;
            };
            appendDefinition(allocator, &final.codegen.symbols, &symbols, .{
                .name = symbol_name,
                .offset = start + excerpt.entry,
                .size = excerpt.code.len - excerpt.entry,
                .is_global = alias != null,
                .is_function = true,
                .is_external = false,
                .is_hidden = alias != null,
                .section = .text,
                .prologue_size = frame.prologue_size,
                .stack_alloc = frame.stack_alloc,
                .frame_size = frame.frame_size,
                .callee_saved_mask = frame.callee_saved_mask,
                .epilogue_offset = frame.epilogue_offset,
                .uses_frame_pointer = frame.uses_frame_pointer,
            }) catch return CompilationError.OutOfMemory;
            if (alias == null) dwarf_procs.append(allocator, .{
                .name = symbol_name,
                .code_start = start + excerpt.entry,
                .code_size = excerpt.code.len - excerpt.entry,
                .loc = .none,
            }) catch return CompilationError.OutOfMemory;
        },
        .entrypoint => |name| {
            const frame = excerpt.frame orelse invariant("an entrypoint excerpt had no frame", .{});
            appendDefinition(allocator, &final.codegen.symbols, &symbols, .{
                .name = name,
                .offset = start,
                .size = excerpt.code.len,
                .is_global = true,
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
            dwarf_procs.append(allocator, .{ .name = name, .code_start = start, .code_size = excerpt.code.len, .loc = .none }) catch return CompilationError.OutOfMemory;
        },
        .message_pool_run, .branch_island => {},
    };
    final.finishImage() catch return CompilationError.OutOfMemory;
    const code = final.getGeneratedCode();
    const relocations = final.getRelocations();
    var resolved = try resolveObjectSymbols(allocator, &final.codegen.symbols, symbols.items, relocations);
    defer resolved.deinit(allocator);
    if (timing) |timings| timings.finish(symbol_relocations_started_ns, .symbol_relocations);
    const object_bytes = try encodeObject(allocator, &store, target, &final, code, rodata.items, zero_fill_size, resolved.symbols, relocations, rodata_relocations.items, dwarf_procs.items, timing);
    errdefer allocator.free(object_bytes);
    return CompilationResult{
        .object_bytes = object_bytes,
        .allocator = allocator,
        .code_locations = if (options.capture_code_locations) try encodeCodeLocations(allocator, &store, &final) else &.{},
    };
}

fn crossCompileDispatch(
    allocator: Allocator,
    lir_store: *const LirStore,
    layout_store: *const layout.Store,
    entrypoints: []const Entrypoint,
    static_data_exports: []const StaticDataExport,
    proc_specs: []const LirProcSpec,
    target: RocTarget,
    enable_default_platform_runtime: bool,
    erased_capture_prefix: u32,
    timing: ?*ObjectFileCompiler.Timing,
    capture_all_procedures: bool,
    link_unit: bool,
    capture_excerpts: bool,
    capture_code_locations: bool,
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
                    target,
                    enable_default_platform_runtime,
                    erased_capture_prefix,
                    timing,
                    capture_all_procedures,
                    link_unit,
                    capture_excerpts,
                    capture_code_locations,
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

test "ObjectFileCompiler emits a link unit that defines nothing and refuses an empty program" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(allocator, @import("base").target.TargetUsize.native);
    defer layouts.deinit();

    var compiler = ObjectFileCompiler.init(allocator);
    try std.testing.expectError(CompilationError.NoEntrypoints, compiler.compileToObjectFile(&store, &layouts, &.{}, &.{}, &.{}, .arm64linux));

    compiler.link_unit = true;
    var result = try compiler.compileToObjectFile(&store, &layouts, &.{}, &.{}, &.{}, .arm64linux);
    defer result.deinit();
    try std.testing.expect(std.mem.startsWith(u8, result.object_bytes, "\x7fELF"));
}

test "ObjectFileCompiler native emission skips static initializers and captures excerpts" {
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
    var compiler = ObjectFileCompiler.initForExcerpts(allocator);
    compiler.timing = &timing;
    const targets = [_]RocTarget{ .x64linux, .arm64linux, .x64v1linux, .arm64v1linux, .x64v1musl, .arm64v1musl };
    for (targets) |target| {
        var result = try compiler.compileToObjectFile(
            &store,
            &layouts,
            &.{},
            &.{},
            store.getProcSpecs(),
            target,
        );
        defer result.deinit();
        try std.testing.expect(std.mem.startsWith(u8, result.object_bytes, "\x7fELF"));
        try std.testing.expect(result.excerpts != null);
        try std.testing.expect(result.excerpts.?.excerpts.len > 0);
    }
    const snapshot = timing.snapshot();
    try std.testing.expectEqual(@as(u64, targets.len), snapshot.native_emission.procedures_emitted);
    try std.testing.expectEqual(@as(u64, 0), snapshot.native_emission.procedures_reused);
    try std.testing.expectEqual(@as(u64, 0), snapshot.rc_helper_instructions_ns);
}

test "ObjectFileCompiler runtime static-root excerpts own only reachable canonical data" {
    const allocator = std.testing.allocator;
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
                .bytes = try a.dupe(u8, "must not travel in the excerpts"),
                .alignment = 1,
                .is_exported = false,
            };
            var compiler = ObjectFileCompiler.initForExcerpts(allocator);
            break :producer try compiler.compileToObjectFile(&store, &layouts, &.{}, exports, store.getProcSpecs(), target);
        };
        // No source descriptor, backing, symbol names, LIR or layout owners remain.
        var owned = try CodeExcerpt.copySet(allocator, &result.excerpts.?);
        defer owned.deinit();
        result.deinit();
        try std.testing.expectEqual(@as(usize, 1), owned.excerpts.len);
        const excerpt = owned.excerpts[0];
        var collector = CodeExcerpt.DataCollector{ .allocator = allocator };
        defer collector.deinit();
        try collector.add(excerpt);
        try std.testing.expectEqual(@as(usize, 2), collector.items.items.len);
        for (collector.items.items) |item| {
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
        // The consumer has the module versions the excerpts came from.
        const files = try allocator.alloc(@import("base").SourceFileEntry, owned.source_files.len);
        defer allocator.free(files);
        for (files, owned.source_files) |*file, module_identity| file.* = .{ .name = "module", .qualified_name = "module", .module_identity = module_identity };
        try store.setSourceFiles(files);
        var layouts = try layout.Store.init(allocator, .u64);
        defer layouts.deinit();
        const CG = LirCodeGenMod.LirCodeGen(target);
        var receiver = try CG.init(allocator, &store, &layouts, .{}, .default);
        defer receiver.deinit();
        var procs = std.AutoHashMap(lir.ProcIdentity, lir.LIR.LirProcSpecId).init(allocator);
        defer procs.deinit();
        var placed = std.AutoHashMap(u32, usize).init(allocator);
        defer placed.deinit();
        var data = std.ArrayList(CodeExcerpt.DataItem).empty;
        defer data.deinit(allocator);
        try CodeExcerpt.splice(CG, allocator, &receiver, &owned, &.{0}, &procs, &placed, &data);
        try receiver.finishImage();
        if (comptime target == RocTarget.detectNative()) {
            var splice = @import("HostSplice.zig").HostSplice.init(allocator);
            defer splice.deinit();
            try splice.addDataItems(data.items);
            var executable = try splice.link(&receiver);
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
