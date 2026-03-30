//! Statement-only LIR -> WebAssembly code generator.
//!
//! The previous wasm backend implementation depended on deleted expression-era
//! LIR concepts. It has been removed completely and must be rewritten against
//! strongest-form statement-only LIR.

const std = @import("std");
const layout = @import("layout");
const lir = @import("lir");

const Allocator = std.mem.Allocator;
const LIR = lir.LIR;
const LirStore = lir.LirStore;

pub const WasmCodeGen = struct {
    allocator: Allocator,
    store: *const LirStore,
    layout_store: *const layout.Store,
    wasm_stack_bytes: u32 = 1024 * 1024,

    pub const GenerateResult = struct {
        wasm_bytes: []u8,
        result_layout: layout.Idx,
        has_imports: bool = false,
    };

    pub fn init(
        allocator: Allocator,
        store: *const LirStore,
        layout_store: *const layout.Store,
    ) WasmCodeGen {
        return .{
            .allocator = allocator,
            .store = store,
            .layout_store = layout_store,
        };
    }

    pub fn deinit(_: *WasmCodeGen) void {}

    pub fn generateModule(
        self: *WasmCodeGen,
        root_proc_id: LIR.LirProcSpecId,
        result_layout: layout.Idx,
    ) Allocator.Error!GenerateResult {
        _ = self;
        _ = root_proc_id;
        _ = result_layout;
        std.debug.panic(
            "todo implement wasm codegen for statement-only LIR",
            .{},
        );
    }
};
