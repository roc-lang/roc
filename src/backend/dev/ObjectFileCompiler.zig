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
//! Roc Source → CIR → Mono IR → Machine Code → Object File
//! ```

const std = @import("std");
const Allocator = std.mem.Allocator;

const layout = @import("layout");
const mono = @import("mono");
const RocTarget = @import("roc_target").RocTarget;

const ObjectWriter = @import("ObjectWriter.zig");
const HostMonoExprCodeGen = @import("MonoExprCodeGen.zig").HostMonoExprCodeGen;
const StaticDataInterner = @import("StaticDataInterner.zig");

/// Information about an entrypoint to compile
pub const Entrypoint = struct {
    /// The exported symbol name (e.g., "roc__main")
    symbol_name: []const u8,
    /// The Mono IR expression ID for the entrypoint body
    body_expr: mono.MonoExprId,
    /// Layouts of the arguments
    arg_layouts: []const layout.Idx,
    /// Layout of the return value
    ret_layout: layout.Idx,
};

/// Result of native compilation
pub const CompilationResult = struct {
    /// The generated object file bytes
    object_bytes: []const u8,
    /// Allocator used - caller must free object_bytes with this
    allocator: Allocator,

    pub fn deinit(self: *CompilationResult) void {
        self.allocator.free(self.object_bytes);
    }
};

/// Errors that can occur during native compilation
pub const CompilationError = error{
    OutOfMemory,
    NoEntrypoints,
    CodeGenerationFailed,
    ObjectGenerationFailed,
    UnsupportedTarget,
};

/// Object file compiler that generates object files from Mono IR.
/// Supports cross-compilation to any RocTarget.
pub const ObjectFileCompiler = struct {
    allocator: Allocator,

    pub fn init(allocator: Allocator) ObjectFileCompiler {
        return .{ .allocator = allocator };
    }

    /// Compile Mono IR to a native object file.
    ///
    /// This is the main entry point for native compilation. It:
    /// 1. Creates a code generator
    /// 2. Compiles all procedures (if any)
    /// 3. Generates entrypoint wrappers following RocCall ABI
    /// 4. Produces an object file with proper symbols and relocations
    pub fn compileToObjectFile(
        self: *ObjectFileCompiler,
        mono_store: *const mono.MonoExprStore,
        layout_store: *const layout.Store,
        entrypoints: []const Entrypoint,
        procs: []const mono.MonoProc,
        target: RocTarget,
    ) CompilationError!CompilationResult {
        if (entrypoints.len == 0) {
            return CompilationError.NoEntrypoints;
        }

        // Create memory backend for static data allocation
        var memory_backend = StaticDataInterner.MemoryBackend.init(self.allocator);
        defer memory_backend.deinit();

        // Create static data interner for string literals
        var static_interner = StaticDataInterner.init(self.allocator, memory_backend.backend());
        defer static_interner.deinit();

        // Initialize the code generator
        var codegen = HostMonoExprCodeGen.init(
            self.allocator,
            mono_store,
            layout_store,
            &static_interner,
        );
        defer codegen.deinit();

        // Compile all procedures first
        if (procs.len > 0) {
            codegen.compileAllProcs(procs) catch {
                return CompilationError.CodeGenerationFailed;
            };
        }

        // Track symbols for object file generation
        var symbols = std.ArrayList(ObjectWriter.Symbol).empty;
        defer symbols.deinit(self.allocator);

        // Generate entrypoint wrappers
        for (entrypoints) |entrypoint| {
            const export_info = codegen.generateEntrypointWrapper(
                entrypoint.symbol_name,
                entrypoint.body_expr,
                entrypoint.arg_layouts,
                entrypoint.ret_layout,
            ) catch {
                return CompilationError.CodeGenerationFailed;
            };

            symbols.append(self.allocator, .{
                .name = entrypoint.symbol_name,
                .offset = export_info.offset,
                .size = export_info.size,
                .is_global = true,
                .is_function = true,
                .is_external = false,
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
                        symbols.append(self.allocator, .{
                            .name = f.name,
                            .offset = 0,
                            .size = 0,
                            .is_global = false,
                            .is_function = true,
                            .is_external = true,
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
                        symbols.append(self.allocator, .{
                            .name = d.name,
                            .offset = 0,
                            .size = 0,
                            .is_global = false,
                            .is_function = false,
                            .is_external = true,
                        }) catch {
                            return CompilationError.OutOfMemory;
                        };
                    }
                },
                else => {},
            }
        }

        // Generate object file
        var output = std.ArrayList(u8).empty;
        errdefer output.deinit(self.allocator);

        ObjectWriter.generateObjectFile(
            self.allocator,
            target,
            code,
            symbols.items,
            relocations,
            &output,
        ) catch {
            return CompilationError.ObjectGenerationFailed;
        };

        return CompilationResult{
            .object_bytes = output.toOwnedSlice(self.allocator) catch {
                return CompilationError.OutOfMemory;
            },
            .allocator = self.allocator,
        };
    }

    /// Compile to an object file and write it to a path.
    pub fn compileToObjectFileAndWrite(
        self: *ObjectFileCompiler,
        mono_store: *const mono.MonoExprStore,
        layout_store: *const layout.Store,
        entrypoints: []const Entrypoint,
        procs: []const mono.MonoProc,
        target: RocTarget,
        output_path: []const u8,
    ) CompilationError!void {
        var result = try self.compileToObjectFile(
            mono_store,
            layout_store,
            entrypoints,
            procs,
            target,
        );
        defer result.deinit();

        // Write to file
        std.fs.cwd().writeFile(.{
            .sub_path = output_path,
            .data = result.object_bytes,
        }) catch {
            return CompilationError.ObjectGenerationFailed;
        };
    }
};

// Tests

test "ObjectFileCompiler initialization" {
    const allocator = std.testing.allocator;
    const compiler = ObjectFileCompiler.init(allocator);
    try std.testing.expectEqual(allocator, compiler.allocator);
}

// Note: Full integration tests for compileToObjectFile require complex setup
// of mono stores and layout stores. These are tested via integration tests
// in the CLI (roc build --backend=dev).
