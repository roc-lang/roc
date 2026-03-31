//! Statement-only LIR -> WebAssembly code generator.
//!
//! The old expression-era wasm backend has been deleted. Reintroducing wasm
//! code generation must start from strongest-form statement-only LIR and must
//! not recover removed expression-tree concepts.

const std = @import("std");
const layout = @import("layout");
const lir = @import("lir");

const Allocator = std.mem.Allocator;
const LIR = lir.LIR;
const LirStore = lir.LirStore;
const LayoutStore = layout.Store;

const Self = @This();

allocator: Allocator,
store: *const LirStore,
layout_store: *const LayoutStore,
/// Configurable wasm stack size in bytes (default 1MB).
wasm_stack_bytes: u32 = 1024 * 1024,
/// Configurable wasm memory pages (0 = auto-compute from stack size).
wasm_memory_pages: u32 = 0,

pub const GenerateResult = struct {
    wasm_bytes: []u8,
    result_layout: layout.Idx,
    has_imports: bool = false,
};

pub fn init(allocator: Allocator, store: *const LirStore, layout_store: *const LayoutStore) Self {
    return .{
        .allocator = allocator,
        .store = store,
        .layout_store = layout_store,
    };
}

pub fn deinit(_: *Self) void {}

pub fn generateModule(self: *Self, root_proc_id: LIR.LirProcSpecId, result_layout: layout.Idx) Allocator.Error!GenerateResult {
    _ = self;
    _ = root_proc_id;
    _ = result_layout;
    std.debug.panic(
        "statement-only wasm backend TODO: reimplement WebAssembly code generation from strongest-form LIR only",
        .{},
    );
}
