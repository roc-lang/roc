//! Regression coverage for ownership ordering before `List.map` reuse checks.

const std = @import("std");
const collections = @import("collections");
const layout = @import("layout");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

const GuardedList = collections.GuardedList;

fn retainReachesPrepare(
    store: *const lir.LirStore,
    start: lir.LIR.CFStmtId,
    retained: lir.LIR.LocalId,
) bool {
    var current = start;
    var retained_alias = retained;
    var remaining = store.cfStmtCount() + 1;
    while (remaining > 0) : (remaining -= 1) {
        switch (store.getCFStmt(current)) {
            .assign_low_level => |assign| {
                if (assign.op == .list_map_prepare_reuse) {
                    const args = store.getLocalSpan(assign.args);
                    return GuardedList.at(args, 0) == retained_alias;
                }
                if (assign.target == retained_alias) return false;
                current = assign.next;
            },
            .decref => |release| {
                if (release.value == retained_alias) return false;
                current = release.next;
            },
            .decref_if_initialized => |release| {
                if (release.value == retained_alias) return false;
                current = release.next;
            },
            .free => |release| {
                if (release.value == retained_alias) return false;
                current = release.next;
            },
            .assign_ref => |assign| {
                switch (assign.op) {
                    .local => |source| if (source == retained_alias) {
                        retained_alias = assign.target;
                    } else if (assign.target == retained_alias) {
                        return false;
                    },
                    else => if (assign.target == retained_alias) return false,
                }
                current = assign.next;
            },
            inline .assign_literal,
            .init_uninitialized,
            .assign_call,
            .assign_call_erased,
            .assign_packed_erased_fn,
            .assign_list,
            .assign_struct,
            .assign_tag,
            => |assign| {
                if (assign.target == retained_alias) return false;
                current = assign.next;
            },
            .set_local => |assign| {
                if (assign.target == retained_alias) return false;
                current = assign.next;
            },
            inline .store_struct,
            .store_tag,
            .debug,
            .expect,
            .comptime_branch_taken,
            .incref,
            => |stmt| current = stmt.next,
            else => return false,
        }
    }
    return false;
}

fn inspect(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
    var prepare_count: usize = 0;
    var retained_prepare_count: usize = 0;

    for (store.getCFStmts()) |stmt| switch (stmt) {
        .assign_low_level => |assign| if (assign.op == .list_map_prepare_reuse) {
            prepare_count += 1;
            try std.testing.expectEqual(@as(u64, 1), assign.rc_effect.consume_args);
            try std.testing.expectEqual(@as(u64, 1), assign.rc_effect.result_aliases_consumed_args);
        },
        .incref => |retain| if (retainReachesPrepare(store, retain.next, retain.value)) {
            retained_prepare_count += 1;
        },
        else => {},
    };

    try std.testing.expect(prepare_count >= 2);
    // The first map over the list shared with the second map must preserve the
    // second ownership unit before the runtime uniqueness observation.
    try std.testing.expect(retained_prepare_count >= 1);
}

test "issue 10368: shared List.map input is retained before reuse preparation" {
    // Repro for https://github.com/roc-lang/roc/issues/10368.
    try harness.runAppPathLirInspection(
        "test/fx/issue_10368_list_map_reuse.roc",
        .{ .list_in_place_map = true, .inline_mode = .wrappers, .proc_debug_names = true },
        inspect,
    );
}
