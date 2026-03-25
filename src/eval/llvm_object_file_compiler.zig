//! Object-file compilation using the LLVM backend.

const std = @import("std");
const layout = @import("layout");
const lir = @import("lir");
const backend = @import("backend");
const RocTarget = @import("roc_target").RocTarget;

const llvm_compile = @import("llvm_compile");
const MonoLlvmCodeGen = llvm_compile.MonoLlvmCodeGen;

const Allocator = std.mem.Allocator;
const LirExprStore = lir.LirExprStore;
const LirProcSpec = lir.LirProcSpec;

pub const Entrypoint = backend.Entrypoint;

pub const CompilationResult = struct {
    object_bytes: []const u8,
    allocator: Allocator,

    pub fn deinit(self: *CompilationResult) void {
        self.allocator.free(self.object_bytes);
    }
};

pub const CompilationError = error{
    OutOfMemory,
    NoEntrypoints,
    CodeGenerationFailed,
    ObjectGenerationFailed,
    UnsupportedTarget,
};

pub const OptimizationMode = enum {
    size,
    speed,
};

pub const ObjectFileCompiler = struct {
    allocator: Allocator,

    pub fn init(allocator: Allocator) ObjectFileCompiler {
        return .{ .allocator = allocator };
    }

    pub fn compileToObjectFile(
        self: *ObjectFileCompiler,
        lir_store: *const LirExprStore,
        layout_store: *const layout.Store,
        entrypoints: []const Entrypoint,
        _: []const LirProcSpec,
        target: RocTarget,
        optimization: OptimizationMode,
    ) CompilationError!CompilationResult {
        if (entrypoints.len == 0) {
            return CompilationError.NoEntrypoints;
        }

        const target_query = std.Target.Query{
            .cpu_arch = target.toCpuArch(),
            .os_tag = target.toOsTag(),
        };
        const std_target = std.zig.system.resolveTargetQuery(target_query) catch {
            return CompilationError.UnsupportedTarget;
        };

        var codegen = MonoLlvmCodeGen.initWithTarget(
            self.allocator,
            lir_store,
            std_target,
            target.toTriple(),
        );
        defer codegen.deinit();
        codegen.builtin_symbol_mode = .native_object;
        codegen.layout_store = layout_store;

        var bitcode_result = codegen.generateEntrypointModule("roc_native_module", entrypoints) catch |err| switch (err) {
            error.OutOfMemory => return CompilationError.OutOfMemory,
            error.CompilationFailed => return CompilationError.CodeGenerationFailed,
        };
        defer bitcode_result.deinit();

        const object_bytes = llvm_compile.compileToObject(
            self.allocator,
            bitcode_result.bitcode,
            .{
                .function_sections = true,
                .opt_level = switch (optimization) {
                    .size => .Less,
                    .speed => .Aggressive,
                },
                .use_module_target_triple = true,
                .merge_builtin_bitcode = false,
            },
        ) catch |err| switch (err) {
            error.OutOfMemory => return CompilationError.OutOfMemory,
            else => return CompilationError.ObjectGenerationFailed,
        };

        return CompilationResult{
            .object_bytes = object_bytes,
            .allocator = self.allocator,
        };
    }

    pub fn compileToObjectFileAndWrite(
        self: *ObjectFileCompiler,
        lir_store: *const LirExprStore,
        layout_store: *const layout.Store,
        entrypoints: []const Entrypoint,
        proc_specs: []const LirProcSpec,
        target: RocTarget,
        optimization: OptimizationMode,
        output_path: []const u8,
    ) CompilationError!void {
        var result = try self.compileToObjectFile(
            lir_store,
            layout_store,
            entrypoints,
            proc_specs,
            target,
            optimization,
        );
        defer result.deinit();

        std.fs.cwd().writeFile(.{
            .sub_path = output_path,
            .data = result.object_bytes,
        }) catch {
            return CompilationError.ObjectGenerationFailed;
        };
    }
};
