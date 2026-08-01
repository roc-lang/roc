//! LIR regression coverage for erased callables in platform ABI schemas.

const std = @import("std");
const collections = @import("collections");
const layout = @import("layout");
const lir = @import("lir");

const harness = @import("lower_to_lir_harness.zig");

fn expectHostAbiCallablesUseErasedRepresentation(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    var found_provided_drop = false;
    var found_plain_box_control = false;
    var found_nested_box_control = false;
    var found_hosted_maker = false;
    var found_hosted_drop = false;

    for (store.getProcSpecs(), 0..) |proc, index| {
        const proc_id: lir.LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(index)));
        const args = store.getLocalSpan(proc.args);

        if (proc.hosted != null and args.len == 1) {
            const arg = collections.GuardedList.at(args, 0);
            const arg_layout = store.getLocal(arg).layout_idx;
            const arg_tag = layouts.getLayout(arg_layout).tag;
            const ret_tag = layouts.getLayout(proc.ret_layout).tag;
            if (arg_layout == .u64 and ret_tag == .erased_callable) found_hosted_maker = true;
            if (arg_tag == .erased_callable and ret_tag == .zst) found_hosted_drop = true;
        }

        const name = store.procDebugName(proc_id) orelse continue;
        if (std.mem.eql(u8, name, "test_drop_plain_box_for_host")) {
            try std.testing.expectEqual(@as(usize, 1), args.len);
            const arg = collections.GuardedList.at(args, 0);
            try std.testing.expectEqual(layout.LayoutTag.box, layouts.getLayout(store.getLocal(arg).layout_idx).tag);
            found_plain_box_control = true;
            continue;
        }
        if (std.mem.eql(u8, name, "test_drop_nested_boxed_callable_for_host")) {
            try std.testing.expectEqual(@as(usize, 1), args.len);
            const arg = collections.GuardedList.at(args, 0);
            const arg_layout = store.getLocal(arg).layout_idx;
            try std.testing.expectEqual(layout.LayoutTag.box, layouts.getLayout(arg_layout).tag);
            const box_abi = layouts.builtinBoxAbi(arg_layout);
            try std.testing.expectEqual(layout.LayoutTag.erased_callable, box_abi.elem_layout.tag);
            found_nested_box_control = true;
            continue;
        }
        if (!std.mem.eql(u8, name, "test_drop_boxed_callable_for_host")) continue;

        try std.testing.expectEqual(@as(usize, 1), args.len);
        const arg = collections.GuardedList.at(args, 0);
        const arg_layout = store.getLocal(arg).layout_idx;
        try std.testing.expectEqual(layout.LayoutTag.erased_callable, layouts.getLayout(arg_layout).tag);

        var found_release = false;
        for (store.getCFStmts()) |stmt| switch (stmt) {
            .decref => |release| if (release.rc.layout_idx == arg_layout) {
                try std.testing.expectEqual(
                    @as(std.meta.Tag(layout.RcHelperPlan), .erased_callable_decref),
                    std.meta.activeTag(layouts.rcHelperPlan(release.rc)),
                );
                try std.testing.expectEqual(lir.LIR.RcAtomicity.atomic, release.atomicity);
                found_release = true;
            },
            else => {},
        };
        try std.testing.expect(found_release);
        found_provided_drop = true;
    }

    try std.testing.expect(found_provided_drop);
    try std.testing.expect(found_plain_box_control);
    try std.testing.expect(found_nested_box_control);
    try std.testing.expect(found_hosted_maker);
    try std.testing.expect(found_hosted_drop);
}

test "host ABI callable positions commit erased layouts without collapsing outer boxes" {
    try harness.runAppPathLirInspection(
        "test/postcheck/provided_callable_boundary/app.roc",
        .{ .proc_debug_names = true },
        expectHostAbiCallablesUseErasedRepresentation,
    );
}
