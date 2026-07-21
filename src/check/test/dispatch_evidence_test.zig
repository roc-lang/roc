//! Tests for the canonical evidence-param enumeration
//! (`src/check/dispatch_evidence.zig`).
//!
//! The enumeration order is a cross-module contract: a caller enumerating its
//! structural copy of an imported scheme must produce the same list the
//! defining module publishes for its template. These tests pin the order and
//! that copy-invariance.

const std = @import("std");
const ModuleEnv = @import("can").ModuleEnv;
const dispatch_evidence = @import("../dispatch_evidence.zig");
const TestEnv = @import("./TestEnv.zig");

const types = @import("types");
const Var = types.Var;

fn defVar(env: *const ModuleEnv, name: []const u8) error{TestUnexpectedResult}!Var {
    const idents = env.getIdentStoreConst();
    const defs_slice = env.store.sliceDefs(env.all_defs);
    for (defs_slice) |def_idx| {
        const def = env.store.getDef(def_idx);
        switch (env.store.getPattern(def.pattern)) {
            .assign => |assign| {
                if (std.mem.eql(u8, name, idents.getText(assign.ident))) {
                    return ModuleEnv.varFrom(def_idx);
                }
            },
            else => {},
        }
    }
    return error.TestUnexpectedResult;
}

/// Find an associated method's def var by method name via the method-defs map.
fn methodDefVar(env: *ModuleEnv, name: []const u8) error{TestUnexpectedResult}!Var {
    const idents = env.getIdentStoreConst();
    for (env.method_defs.entries.items) |entry| {
        if (std.mem.eql(u8, name, idents.getText(entry.key.methodIdent()))) {
            return ModuleEnv.varFrom(entry.value.def_idx);
        }
    }
    return error.TestUnexpectedResult;
}

fn enumerate(
    gpa: std.mem.Allocator,
    env: *const ModuleEnv,
    root: Var,
    out: *std.ArrayListUnmanaged(dispatch_evidence.EvidenceParam),
) std.mem.Allocator.Error!void {
    var scratch = dispatch_evidence.Scratch{};
    defer scratch.deinit(gpa);
    try dispatch_evidence.enumerateEvidenceParams(gpa, &env.types, root, &scratch, out);
}

fn constrainedVar(env: *ModuleEnv, method_name: []const u8) std.mem.Allocator.Error!Var {
    const unit_var = try env.types.freshFromContent(.{ .structure = .empty_record });
    const fn_args = try env.types.appendVars(&.{unit_var});
    const fn_var = try env.types.freshFromContent(.{ .structure = .{ .fn_pure = .{
        .args = fn_args,
        .ret = unit_var,
    } } });
    const constraint = types.StaticDispatchConstraint{
        .fn_name = try env.insertIdent(@import("base").Ident.for_text(method_name)),
        .fn_var = fn_var,
        .origin = .{ .where_clause = .{} },
    };
    const constraints = try env.types.appendStaticDispatchConstraints(&.{constraint});
    return try env.types.freshFromContent(.{ .flex = .{
        .name = null,
        .constraints = constraints,
    } });
}

fn transparentAlias(env: *ModuleEnv, name: []const u8, backing: Var, args: []const Var) std.mem.Allocator.Error!Var {
    const ident = try env.insertIdent(@import("base").Ident.for_text(name));
    const content = try env.types.mkAlias(
        .{ .ident_idx = ident },
        backing,
        args,
        env.selfModuleIdentity(),
    );
    return try env.types.freshFromContent(content);
}

test "evidence params enumerate in signature order" {
    const source =
        \\Thing := [Val(Str)].{
        \\  to_str : Thing -> Str
        \\  to_str = |Thing.Val(s)| s
        \\  to_hex : Thing -> Str
        \\  to_hex = |Thing.Val(s)| s
        \\}
        \\
        \\helper : a, b -> Str where [a.to_str : a -> Str, b.to_hex : b -> Str]
        \\helper = |x, _y| x.to_str()
        \\
        \\main : Str
        \\main = helper(Thing.Val("hello"), Thing.Val("hi"))
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("main", "Str");

    const gpa = std.testing.allocator;
    const env = test_env.module_env;
    var params = std.ArrayListUnmanaged(dispatch_evidence.EvidenceParam).empty;
    defer params.deinit(gpa);
    try enumerate(gpa, env, try defVar(env, "helper"), &params);

    try std.testing.expectEqual(@as(usize, 2), params.items.len);
    const idents = env.getIdentStoreConst();
    try std.testing.expectEqualStrings("to_str", idents.getText(params.items[0].constraint.fn_name));
    try std.testing.expectEqualStrings("to_hex", idents.getText(params.items[1].constraint.fn_name));
    try std.testing.expect(params.items[0].dispatcher_var != params.items[1].dispatcher_var);
}

test "evidence params reach vars bound only inside constraint fn types" {
    // The clauses on `b` are phantom (the body never dispatches them), which
    // keeps this a valid polymorphic signature; the enumeration must still
    // reach `b` through `a.step`'s fn type.
    const source =
        \\helper : a -> Str where [a.step : a -> b, b.to_str : b -> Str]
        \\helper = |_x| "hi"
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("helper", "a -> Str where [a.step : a -> b, b.to_str : b -> Str]");

    const gpa = std.testing.allocator;
    const env = test_env.module_env;
    var params = std.ArrayListUnmanaged(dispatch_evidence.EvidenceParam).empty;
    defer params.deinit(gpa);
    try enumerate(gpa, env, try defVar(env, "helper"), &params);

    // `a` occurs in the signature and emits `step`; walking that constraint's
    // function type reaches the constraint-only `b` and emits `to_str`.
    try std.testing.expectEqual(@as(usize, 2), params.items.len);
    const idents = env.getIdentStoreConst();
    try std.testing.expectEqualStrings("step", idents.getText(params.items[0].constraint.fn_name));
    try std.testing.expectEqualStrings("to_str", idents.getText(params.items[1].constraint.fn_name));
}

test "record-tail evidence path is normalized to its logical row field" {
    var test_env = try TestEnv.init("Test", "");
    defer test_env.deinit();

    const gpa = std.testing.allocator;
    const env = test_env.module_env;
    const dispatcher = try constrainedVar(env, "inspect_tail");
    const head_value = try env.types.freshFromContent(.{ .structure = .empty_record });
    const empty_tail = try env.types.freshFromContent(.{ .structure = .empty_record });
    const head_name = try env.insertIdent(@import("base").Ident.for_text("head"));
    const tail_name = try env.insertIdent(@import("base").Ident.for_text("tail"));

    const tail_fields = try env.types.appendRecordFields(&.{.{ .name = tail_name, .var_ = dispatcher }});
    const tail_row = try env.types.freshFromContent(.{ .structure = .{ .record = .{
        .fields = tail_fields,
        .ext = empty_tail,
    } } });
    const aliased_tail = try transparentAlias(env, "TailFields", tail_row, &.{dispatcher});
    const head_fields = try env.types.appendRecordFields(&.{.{ .name = head_name, .var_ = head_value }});
    const root = try env.types.freshFromContent(.{ .structure = .{ .record = .{
        .fields = head_fields,
        .ext = aliased_tail,
    } } });

    var params = std.ArrayListUnmanaged(dispatch_evidence.EvidenceParam).empty;
    defer params.deinit(gpa);
    var scratch = dispatch_evidence.Scratch{};
    defer scratch.deinit(gpa);
    try dispatch_evidence.enumerateEvidenceParams(gpa, &env.types, root, &scratch, &params);

    try std.testing.expectEqual(@as(usize, 1), params.items.len);
    try std.testing.expectEqual(@as(usize, 1), params.items[0].path.len);
    try std.testing.expectEqual(@intFromEnum(dispatch_evidence.PathStep.Kind.record_field), params.items[0].path[0].kind);
    try std.testing.expectEqual(@as(u32, @bitCast(tail_name)), params.items[0].path[0].data);
}

test "tag-tail evidence path is normalized to its logical tag payload" {
    var test_env = try TestEnv.init("Test", "");
    defer test_env.deinit();

    const gpa = std.testing.allocator;
    const env = test_env.module_env;
    const dispatcher = try constrainedVar(env, "inspect_tail");
    const head_value = try env.types.freshFromContent(.{ .structure = .empty_record });
    const empty_tail = try env.types.freshFromContent(.{ .structure = .empty_tag_union });
    const head_name = try env.insertIdent(@import("base").Ident.for_text("Head"));
    const tail_name = try env.insertIdent(@import("base").Ident.for_text("Tail"));

    const tail_tag = try env.types.mkTag(tail_name, &.{dispatcher});
    const tail_row = try env.types.freshFromContent(try env.types.mkTagUnion(&.{tail_tag}, empty_tail));
    const aliased_tail = try transparentAlias(env, "TailTags", tail_row, &.{dispatcher});
    const head_tag = try env.types.mkTag(head_name, &.{head_value});
    const root = try env.types.freshFromContent(try env.types.mkTagUnion(&.{head_tag}, aliased_tail));

    var params = std.ArrayListUnmanaged(dispatch_evidence.EvidenceParam).empty;
    defer params.deinit(gpa);
    var scratch = dispatch_evidence.Scratch{};
    defer scratch.deinit(gpa);
    try dispatch_evidence.enumerateEvidenceParams(gpa, &env.types, root, &scratch, &params);

    try std.testing.expectEqual(@as(usize, 1), params.items.len);
    try std.testing.expectEqual(@as(usize, 2), params.items[0].path.len);
    try std.testing.expectEqual(@intFromEnum(dispatch_evidence.PathStep.Kind.tag_payload_tag), params.items[0].path[0].kind);
    try std.testing.expectEqual(@as(u32, @bitCast(tail_name)), params.items[0].path[0].data);
    try std.testing.expectEqual(@intFromEnum(dispatch_evidence.PathStep.Kind.tag_payload_index), params.items[0].path[1].kind);
    try std.testing.expectEqual(@as(u32, 0), params.items[0].path[1].data);
}

test "imported scheme copy enumerates the same param list as the defining module" {
    // Module `Wrap` owns a generic associated method with a where clause;
    // module B dispatches it on a local nominal. Discharging that dispatch in
    // B copies `unwrap`'s scheme into B's store and records the copy as a
    // `dispatch_target` instantiation. The copy must enumerate the same param
    // list as the defining module's own scheme.
    const source_wrap =
        \\Wrap(a) := [W(a)].{
        \\  unwrap : Wrap(a) -> Str where [a.to_str : a -> Str]
        \\  unwrap = |Wrap.W(x)| x.to_str()
        \\}
    ;
    var test_env_wrap = try TestEnv.init("Wrap", source_wrap);
    defer test_env_wrap.deinit();

    const source_b =
        \\import Wrap
        \\
        \\Thing := [Val(Str)].{
        \\  to_str : Thing -> Str
        \\  to_str = |Thing.Val(s)| s
        \\}
        \\
        \\main : Str
        \\main = Wrap.W(Thing.Val("hi")).unwrap()
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "Wrap", &test_env_wrap);
    defer test_env_b.deinit();
    try test_env_b.assertDefType("main", "Str");

    const gpa = std.testing.allocator;

    // Defining module's enumeration over `unwrap`'s own scheme.
    const env_wrap = test_env_wrap.module_env;
    var params_wrap = std.ArrayListUnmanaged(dispatch_evidence.EvidenceParam).empty;
    defer params_wrap.deinit(gpa);
    try enumerate(gpa, env_wrap, try methodDefVar(env_wrap, "unwrap"), &params_wrap);
    try std.testing.expectEqual(@as(usize, 1), params_wrap.items.len);
    try std.testing.expectEqualStrings(
        "to_str",
        env_wrap.getIdentStoreConst().getText(params_wrap.items[0].constraint.fn_name),
    );

    // Caller module's enumeration over the pristine scheme copy captured by
    // the dispatch_target scheme-use record.
    const env_b = test_env_b.module_env;
    var found_matching_record = false;
    for (env_b.scheme_uses.items.items) |record| {
        if (record.slot_kind != @intFromEnum(ModuleEnv.SchemeUseRecord.Slot.dispatch_target)) continue;

        var params_b = std.ArrayListUnmanaged(dispatch_evidence.EvidenceParam).empty;
        defer params_b.deinit(gpa);
        try enumerate(gpa, env_b, @enumFromInt(record.scheme_root), &params_b);
        if (params_b.items.len != 1) continue;

        const name_b = env_b.getIdentStoreConst().getText(params_b.items[0].constraint.fn_name);
        if (std.mem.eql(u8, name_b, "to_str")) found_matching_record = true;
    }
    try std.testing.expect(found_matching_record);
}
