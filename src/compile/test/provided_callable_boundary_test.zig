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

fn expectContextOutcomeContainsErasedCallable(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    var found_outcome = false;
    var found_step = false;

    for (store.getProcSpecs()) |proc| {
        const args = store.getLocalSpan(proc.args);

        if (proc.abi == .erased_callable and proc.body != null and proc.ret_layout == .u64 and args.len >= 1) {
            const first_arg = collections.GuardedList.at(args, 0);
            if (store.getLocal(first_arg).layout_idx == .u64) found_step = true;
        }

        if (args.len != 1 or layouts.getLayout(proc.ret_layout).tag != .tag_union) continue;
        const state_arg = collections.GuardedList.at(args, 0);
        const state_layout = layouts.getLayout(store.getLocal(state_arg).layout_idx);
        try std.testing.expectEqual(layout.LayoutTag.struct_, state_layout.tag);
        const state_info = layouts.getStructInfo(state_layout);
        try std.testing.expectEqual(@as(usize, 2), state_info.fields.len);

        const outcome_layout = layouts.getLayout(proc.ret_layout);
        try std.testing.expectEqual(layout.LayoutTag.tag_union, outcome_layout.tag);
        const outcome_info = layouts.getTagUnionInfo(outcome_layout);
        try std.testing.expectEqual(@as(usize, 2), outcome_info.variants.len);
        try std.testing.expect(outcome_info.data.discriminant_size != 0);

        var found_response = false;
        var found_stream = false;
        for (0..outcome_info.variants.len) |variant_index| {
            const payload_idx = outcome_info.variants.get(variant_index).payload_layout;
            if (payload_idx == .u16) {
                found_response = true;
                continue;
            }

            const payload_layout = layouts.getLayout(payload_idx);
            if (payload_layout.tag != .struct_) continue;
            const stream_info = layouts.getStructInfo(payload_layout);
            try std.testing.expectEqual(@as(usize, 2), stream_info.fields.len);

            var u64_fields: usize = 0;
            var erased_fields: usize = 0;
            for (0..stream_info.fields.len) |field_index| {
                const field = stream_info.fields.get(field_index);
                if (field.is_padding) continue;
                const field_tag = layouts.getLayout(field.layout).tag;
                if (field.layout == .u64) u64_fields += 1;
                if (field_tag == .erased_callable) erased_fields += 1;
            }
            try std.testing.expectEqual(@as(usize, 1), u64_fields);
            try std.testing.expectEqual(@as(usize, 1), erased_fields);
            found_stream = true;
        }

        try std.testing.expect(found_response);
        try std.testing.expect(found_stream);
        found_outcome = true;
    }

    try std.testing.expect(found_outcome);
    try std.testing.expect(found_step);
}

test "provided context may return an erased callable nested in an outcome" {
    try harness.runAppPathLirInspection(
        "test/postcheck/provided_callable_context_union/app.roc",
        .{ .proc_debug_names = true },
        expectContextOutcomeContainsErasedCallable,
    );
}

fn expectRecursiveBoxedCallableForwardsReuseThroughLet(
    store: *const lir.LirStore,
    _: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    var specialized_count: usize = 0;
    var matching_repack_count: usize = 0;

    for (store.getProcSpecs(), 0..) |proc, index| {
        if (proc.abi != .roc or proc.body == null or proc.erased_reuse_arg == null) continue;
        const proc_id: lir.LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(index)));
        const name = store.procDebugName(proc_id) orelse continue;
        if (!std.mem.eql(u8, name, "from_state")) continue;

        specialized_count += 1;
        const reuse_arg = proc.erased_reuse_arg.?;
        var work = std.ArrayList(lir.LIR.CFStmtId).empty;
        defer work.deinit(store.allocator);
        var visited = std.AutoHashMap(lir.LIR.CFStmtId, void).init(store.allocator);
        defer visited.deinit();
        try work.append(store.allocator, proc.body.?);
        while (work.pop()) |stmt_id| {
            const entry = try visited.getOrPut(stmt_id);
            if (entry.found_existing) continue;
            switch (store.getCFStmt(stmt_id)) {
                .assign_packed_erased_fn => |pack| {
                    if (pack.reuse == reuse_arg) matching_repack_count += 1;
                },
                else => {},
            }
            try lir.BodyClone.appendSuccessors(@constCast(store), &work, stmt_id);
        }
    }

    try std.testing.expectEqual(@as(usize, 1), specialized_count);
    try std.testing.expectEqual(@as(usize, 1), matching_repack_count);
}

test "recursive boxed callable forwards reuse through Box, let, and alias boundaries" {
    try harness.runAppPathLirInspection(
        "test/postcheck/erased_callable_return_forwarding/app.roc",
        .{ .inline_mode = .wrappers, .proc_debug_names = true },
        expectRecursiveBoxedCallableForwardsReuseThroughLet,
    );
}

test "alternative callable producers may share one runtime-selected reuse owner" {
    try harness.runAppPathLirInspection(
        "test/postcheck/erased_callable_return_forwarding/alternatives.roc",
        .{ .inline_mode = .wrappers, .proc_debug_names = true },
        expectRecursiveBoxedCallableForwardsReuseThroughLet,
    );
}

fn expectNoDestinationSpecializedFromState(
    store: *const lir.LirStore,
    _: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    var specialized_count: usize = 0;
    for (store.getProcSpecs(), 0..) |proc, index| {
        if (proc.abi != .roc or proc.erased_reuse_arg == null) continue;
        const proc_id: lir.LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(index)));
        const name = store.procDebugName(proc_id) orelse continue;
        if (std.mem.eql(u8, name, "from_state")) specialized_count += 1;
    }

    // Both constructors execute before the runtime choice. Lowering must not
    // specialize either producer with the call's one affine reuse owner.
    try std.testing.expectEqual(@as(usize, 0), specialized_count);
}

test "competing eager callable producers cannot both consume one reuse owner" {
    try harness.runAppPathLirInspection(
        "test/postcheck/erased_callable_return_forwarding/competitors.roc",
        .{ .inline_mode = .wrappers, .proc_debug_names = true },
        expectNoDestinationSpecializedFromState,
    );
}

test "repeatable callable producers cannot consume one reuse owner per iteration" {
    try harness.runAppPathLirInspection(
        "test/postcheck/erased_callable_return_forwarding/loop.roc",
        .{ .inline_mode = .wrappers, .proc_debug_names = true },
        expectNoDestinationSpecializedFromState,
    );
}
