//! LIR to LLVM code generator
//!
//! The previous LLVM backend implementation has been deleted. Strongest-form LIR is now
//! statement-only and local-based, so LLVM code generation must be rewritten
//! directly against `lir.LirStore` CF statements and explicit locals.

const std = @import("std");
const layout = @import("layout");
const lir = @import("lir");

const Allocator = std.mem.Allocator;

/// Public struct `MonoLlvmCodeGen`.
pub const MonoLlvmCodeGen = struct {
    allocator: Allocator,
    store: *const lir.LirStore,
    layout_store: ?*const layout.Store = null,

    pub const Error = error{
        OutOfMemory,
        CompilationFailed,
    };

    pub const GenerateResult = struct {
        bitcode: []const u32,
        allocator: Allocator,

        pub fn deinit(self: *GenerateResult) void {
            self.allocator.free(self.bitcode);
        }
    };

    pub fn init(allocator: Allocator, store: *const lir.LirStore) MonoLlvmCodeGen {
        return .{
            .allocator = allocator,
            .store = store,
            .layout_store = null,
        };
    }

    pub fn deinit(_: *MonoLlvmCodeGen) void {}

    pub fn reset(_: *MonoLlvmCodeGen) void {}

    pub fn generateCode(
        _: *MonoLlvmCodeGen,
        _: lir.LIR.LirProcSpecId,
        _: layout.Idx,
    ) Error!GenerateResult {
        std.debug.panic(
            "todo implement LLVM codegen for statement-only LIR",
            .{},
        );
    }
};
