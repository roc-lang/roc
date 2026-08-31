//! Regression test for issue #10980.

const std = @import("std");
const builtin = @import("builtin");
const lir = @import("lir");
const llvm_codegen = @import("llvm_codegen");

const harness = @import("lower_to_lir_harness.zig");

/// A dictionary-dispatch thunk exists so `roc_boxy_call_dict` can resolve a
/// boxy method slot to a worker procedure at runtime. This app has no runtime
/// worker slots, so every thunk it emits is dead code that each host entrypoint
/// still registers at startup.
fn expectNoBoxyDictThunks(
    lowered: *const lir.CheckedPipeline.LoweredProgram,
) harness.LowerToLirHarnessError!void {
    try std.testing.expect(lowered.lir_result.boxy_dicts.items.len == 0);
    try std.testing.expectEqual(@as(usize, 0), lowered.lir_result.boxy_worker_procs.items.len);

    var erased_callable_procs: usize = 0;
    for (lowered.lir_result.store.getProcSpecs()) |proc| {
        if (proc.abi == .erased_callable) erased_callable_procs += 1;
    }
    try std.testing.expect(erased_callable_procs > 0);

    var codegen = llvm_codegen.MonoLlvmCodeGen.initForLinkedObject(
        std.testing.allocator,
        &lowered.lir_result.store,
        lowered.lir_result.boxy_erased_arg_desc_offsets.items,
        lowered.lir_result.boxy_erased_arg_desc_params.items,
        lowered.lir_result.boxy_worker_procs.items,
        builtin.target,
    );
    defer codegen.deinit();
    codegen.layout_store = &lowered.lir_result.layouts;

    var generated = codegen.generateEntrypointModule("issue_10980", &.{}) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.TestUnexpectedResult,
    };
    defer generated.deinit();

    try std.testing.expectEqual(@as(usize, 0), codegen.boxy_dict_thunks.count());
}

test "issue 10980: handing a closure to the host emits no boxy dictionary thunks" {
    // Repro for https://github.com/roc-lang/roc/issues/10980.
    //
    // This platform exposes `Box(U64 -> U64)` across the host ABI, so the app
    // builds a packed erased callable. That construction marks the Boxy runtime
    // as used, and the LLVM backend then emits a `roc_boxy_dict_thunk_N` for
    // every procedure in the program and registers all of them from every host
    // entrypoint. The app owns no runtime worker slots, so the expected thunk
    // count is zero. Emitting one per procedure nearly doubled the reporter's
    // wasm binary.
    try harness.runAppPathLoweredInspection(
        "test/postcheck/provided_callable_boundary/app.roc",
        .{ .proc_debug_names = true },
        expectNoBoxyDictThunks,
    );
}
