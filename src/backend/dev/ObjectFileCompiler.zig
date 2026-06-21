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

const ObjectWriter = @import("ObjectWriter.zig");
const LirCodeGenMod = @import("LirCodeGen.zig");
const static_data_export = @import("StaticDataExport.zig");
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

    pub fn deinit(self: *CompilationResult) void {
        self.allocator.free(self.object_bytes);
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

    pub fn init(allocator: Allocator) ObjectFileCompiler {
        return .{ .allocator = allocator };
    }

    /// Compile LIR to a native object file for the given RocTarget.
    ///
    /// Dispatches at runtime to the correct compile-time LirCodeGen
    /// instantiation for the requested target. Works for both native and
    /// cross-compilation — the caller just passes the desired target.
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
        return crossCompileDispatch(self.allocator, lir_store, layout_store, entrypoints, static_data_exports, proc_specs, target, self.enable_default_platform_runtime);
    }

    /// Compile to an object file and write it to a path.
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

        // Write to file. Use the AV-safe wrapper so a transient AccessDenied
        // from a Windows filter driver holding the just-created file open is
        // retried rather than failing the build.
        writeFileWindowsAvSafe(roc_ctx.std_io, output_path, result.object_bytes) catch |err| {
            std.log.err("failed to write object file {s}: {}", .{ output_path, err });
            return CompilationError.ObjectGenerationFailed;
        };
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
) CompilationError!CompilationResult {
    if (entrypoints.len == 0 and static_data_exports.len == 0) {
        return CompilationError.NoEntrypoints;
    }

    var static_strings = StaticStringData.build(allocator, lir_store, target) catch {
        return CompilationError.OutOfMemory;
    };
    defer static_strings.deinit();

    // Initialize the code generator
    var codegen = CodeGen.init(
        allocator,
        lir_store,
        layout_store,
        static_strings.entries,
    ) catch return CompilationError.OutOfMemory;
    defer codegen.deinit();

    // Set object file mode to generate relocatable symbol references instead of direct pointers
    codegen.generation_mode = .object_file;
    codegen.enable_default_platform_runtime = enable_default_platform_runtime;

    // Compile all procedures first
    if (proc_specs.len > 0) {
        codegen.compileAllProcSpecs(proc_specs) catch return CompilationError.OutOfMemory;
    }

    // Track symbols for object file generation
    var symbols = std.ArrayList(ObjectWriter.Symbol).empty;
    defer symbols.deinit(allocator);

    var rodata = std.ArrayList(u8).empty;
    defer rodata.deinit(allocator);

    var rodata_relocations = std.ArrayList(ObjectWriter.DataRelocation).empty;
    defer rodata_relocations.deinit(allocator);

    var static_data_symbols = std.ArrayList(ObjectWriter.Symbol).empty;
    defer static_data_symbols.deinit(allocator);

    var owned_proc_symbol_names = std.ArrayList([]u8).empty;
    defer {
        for (owned_proc_symbol_names.items) |name| allocator.free(name);
        owned_proc_symbol_names.deinit(allocator);
    }

    var dwarf_procs = std.ArrayList(Dwarf.ProcEntry).empty;
    defer dwarf_procs.deinit(allocator);

    for (static_data_exports) |data_export| {
        try appendStaticDataExport(allocator, data_export, &rodata, &rodata_relocations, &static_data_symbols);
    }
    for (static_strings.exports) |data_export| {
        try appendStaticDataExport(allocator, data_export, &rodata, &rodata_relocations, &static_data_symbols);
    }

    // ELF requires all local symbols to appear before global symbols. Keep that
    // invariant in the shared symbol list while preserving each symbol's section
    // offset and name-based relocation target.
    for (static_data_symbols.items) |sym| {
        if (sym.is_global) continue;
        symbols.append(allocator, sym) catch return CompilationError.OutOfMemory;
    }
    for (proc_specs, 0..) |_, i| {
        const proc_id: lir.LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(i)));
        const proc_symbol = codegen.compiledProcSymbol(proc_id) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("ObjectFileCompiler invariant violated: LIR proc {d} was not compiled before symbol publication", .{i});
            }
            unreachable;
        };
        const symbol_name = static_data_export.procSymbolName(allocator, proc_symbol.name) catch return CompilationError.OutOfMemory;
        owned_proc_symbol_names.append(allocator, symbol_name) catch {
            allocator.free(symbol_name);
            return CompilationError.OutOfMemory;
        };
        symbols.append(allocator, .{
            .name = symbol_name,
            .offset = proc_symbol.code_start,
            .size = proc_symbol.code_end - proc_symbol.code_start,
            .is_global = false,
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
    for (static_data_symbols.items) |sym| {
        if (!sym.is_global) continue;
        symbols.append(allocator, sym) catch return CompilationError.OutOfMemory;
    }

    // Generate entrypoint wrappers
    for (entrypoints) |entrypoint| {
        const export_info = codegen.generateEntrypointWrapper(
            entrypoint.symbol_name,
            entrypoint.proc,
            entrypoint.arg_layouts,
            entrypoint.ret_layout,
        ) catch return CompilationError.OutOfMemory;

        symbols.append(allocator, .{
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

    // Get generated code and relocations
    const code = codegen.getGeneratedCode();
    const relocations = codegen.getRelocations();

    // Collect external symbol references from relocations
    for (relocations) |reloc| {
        switch (reloc) {
            .linked_function => |f| {
                // Check if this is already in our symbols list
                var found = false;
                for (symbols.items) |sym| {
                    if (std.mem.eql(u8, sym.name, f.name)) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    symbols.append(allocator, .{
                        .name = f.name,
                        .offset = 0,
                        .size = 0,
                        .is_global = false,
                        .is_function = true,
                        .is_external = true,
                        .section = .undef,
                    }) catch {
                        return CompilationError.OutOfMemory;
                    };
                }
            },
            .linked_data => |d| {
                var found = false;
                for (symbols.items) |sym| {
                    if (std.mem.eql(u8, sym.name, d.name)) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    symbols.append(allocator, .{
                        .name = d.name,
                        .offset = 0,
                        .size = 0,
                        .is_global = false,
                        .is_function = false,
                        .is_external = true,
                        .section = .undef,
                    }) catch {
                        return CompilationError.OutOfMemory;
                    };
                }
            },
            .local_data, .jmp_to_return => {},
        }
    }

    for (rodata_relocations.items) |reloc| {
        var found = false;
        for (symbols.items) |sym| {
            if (std.mem.eql(u8, sym.name, reloc.target_symbol_name)) {
                found = true;
                break;
            }
        }
        if (!found) {
            symbols.append(allocator, .{
                .name = reloc.target_symbol_name,
                .offset = 0,
                .size = 0,
                .is_global = false,
                .is_function = false,
                .is_external = true,
                .section = .undef,
            }) catch {
                return CompilationError.OutOfMemory;
            };
        }
    }

    // Build DWARF debug sections from the line entries recorded during
    // code generation.
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

    // Generate object file
    var output = std.ArrayList(u8).empty;
    errdefer output.deinit(allocator);

    ObjectWriter.generateObjectFileWithDebug(
        allocator,
        target,
        code,
        rodata.items,
        symbols.items,
        relocations,
        rodata_relocations.items,
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

    return CompilationResult{
        .object_bytes = output.toOwnedSlice(allocator) catch {
            return CompilationError.OutOfMemory;
        },
        .allocator = allocator,
    };
}

fn appendStaticDataExport(
    allocator: Allocator,
    data_export: StaticDataExport,
    rodata: *std.ArrayList(u8),
    rodata_relocations: *std.ArrayList(ObjectWriter.DataRelocation),
    static_data_symbols: *std.ArrayList(ObjectWriter.Symbol),
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

    static_data_symbols.append(allocator, .{
        .name = data_export.symbol_name,
        .offset = aligned_offset + symbol_offset,
        .size = data_export.bytes.len - symbol_offset,
        .is_global = data_export.is_global,
        .is_function = false,
        .is_external = false,
        .section = .rodata,
    }) catch {
        return CompilationError.OutOfMemory;
    };

    for (data_export.relocations) |relocation| {
        rodata_relocations.append(allocator, .{
            .offset = @as(u64, @intCast(aligned_offset)) + relocation.offset,
            .target_symbol_name = relocation.target_symbol_name,
            .addend = relocation.addend,
        }) catch {
            return CompilationError.OutOfMemory;
        };
    }
}

fn compileStaticDataObjectBytes(
    allocator: Allocator,
    static_data_exports: []const StaticDataExport,
    target: RocTarget,
) CompilationError!CompilationResult {
    if (static_data_exports.len == 0) {
        return CompilationError.NoEntrypoints;
    }

    var symbols = std.ArrayList(ObjectWriter.Symbol).empty;
    defer symbols.deinit(allocator);

    var rodata = std.ArrayList(u8).empty;
    defer rodata.deinit(allocator);

    var rodata_relocations = std.ArrayList(ObjectWriter.DataRelocation).empty;
    defer rodata_relocations.deinit(allocator);

    var static_data_symbols = std.ArrayList(ObjectWriter.Symbol).empty;
    defer static_data_symbols.deinit(allocator);

    for (static_data_exports) |data_export| {
        try appendStaticDataExport(allocator, data_export, &rodata, &rodata_relocations, &static_data_symbols);
    }

    for (static_data_symbols.items) |sym| {
        if (sym.is_global) continue;
        symbols.append(allocator, sym) catch return CompilationError.OutOfMemory;
    }
    for (static_data_symbols.items) |sym| {
        if (!sym.is_global) continue;
        symbols.append(allocator, sym) catch return CompilationError.OutOfMemory;
    }

    for (rodata_relocations.items) |reloc| {
        var found = false;
        for (symbols.items) |sym| {
            if (std.mem.eql(u8, sym.name, reloc.target_symbol_name)) {
                found = true;
                break;
            }
        }
        if (!found) {
            symbols.append(allocator, .{
                .name = reloc.target_symbol_name,
                .offset = 0,
                .size = 0,
                .is_global = false,
                .is_function = false,
                .is_external = true,
                .section = .undef,
            }) catch return CompilationError.OutOfMemory;
        }
    }

    var output = std.ArrayList(u8).empty;
    errdefer output.deinit(allocator);

    ObjectWriter.generateObjectFile(
        allocator,
        target,
        &.{},
        rodata.items,
        symbols.items,
        &.{},
        rodata_relocations.items,
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
fn crossCompileDispatch(
    allocator: Allocator,
    lir_store: *const LirStore,
    layout_store: *const layout.Store,
    entrypoints: []const Entrypoint,
    static_data_exports: []const StaticDataExport,
    proc_specs: []const LirProcSpec,
    target: RocTarget,
    enable_default_platform_runtime: bool,
) CompilationError!CompilationResult {
    const enum_info = @typeInfo(RocTarget).@"enum";
    inline for (enum_info.fields) |field| {
        const comptime_target: RocTarget = @enumFromInt(field.value);
        if (target == comptime_target) {
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
                    comptime_target,
                    enable_default_platform_runtime,
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
}

// Note: Full integration tests for compileToObjectFile require complex setup
// of mono stores and layout stores. These are tested via integration tests
// in the CLI (roc build --opt=dev).
