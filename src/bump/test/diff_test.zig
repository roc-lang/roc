//! Rule-matrix tests for the `roc bump` differ: one test per row of the
//! Elm-style severity table, on hand-built `PackageApi` values.

const std = @import("std");
const base = @import("base");

const PackageApi = @import("../PackageApi.zig");
const diff_mod = @import("../diff.zig");
const Magnitude = diff_mod.Magnitude;
const TypeId = PackageApi.TypeId;

const gpa = std.testing.allocator;

fn diffMagnitude(old: *PackageApi, new: *PackageApi) std.mem.Allocator.Error!Magnitude {
    try old.normalize();
    try new.normalize();
    var result = try diff_mod.diff(gpa, old, new);
    defer result.deinit();
    return result.magnitude;
}

fn builtinStr(api: *PackageApi) std.mem.Allocator.Error!TypeId {
    return api.addType(.{ .named = .{ .origin = .builtin, .path = "Str", .args = &.{} } });
}

/// A module "M" with a single value item "M.f" of type `Str -> Str`.
fn strToStrApi() std.mem.Allocator.Error!PackageApi {
    var api = PackageApi.init(gpa);
    errdefer api.deinit();
    const str = try builtinStr(&api);
    const args = try api.allocator().dupe(TypeId, &.{str});
    const fn_ty = try api.addType(.{ .function = .{ .effectful = false, .args = args, .ret = str } });
    const module = try api.addModule("M");
    try api.addItem(module, .{ .path = "M.f", .kind = .{ .value = fn_ty } });
    return api;
}

test "no API change is a patch" {
    var old = try strToStrApi();
    defer old.deinit();
    var new = try strToStrApi();
    defer new.deinit();

    try std.testing.expectEqual(Magnitude.patch, try diffMagnitude(&old, &new));
}

test "adding a module is minor" {
    var old = try strToStrApi();
    defer old.deinit();
    var new = try strToStrApi();
    defer new.deinit();
    _ = try new.addModule("Extras");

    try std.testing.expectEqual(Magnitude.minor, try diffMagnitude(&old, &new));
}

test "removing a module is major" {
    var old = try strToStrApi();
    defer old.deinit();
    _ = try old.addModule("Extras");
    var new = try strToStrApi();
    defer new.deinit();

    try std.testing.expectEqual(Magnitude.major, try diffMagnitude(&old, &new));
}

test "adding an item is minor" {
    var old = try strToStrApi();
    defer old.deinit();
    var new = try strToStrApi();
    defer new.deinit();
    const str = try builtinStr(&new);
    try new.addItem(0, .{ .path = "M.g", .kind = .{ .value = str } });

    try std.testing.expectEqual(Magnitude.minor, try diffMagnitude(&old, &new));
}

test "removing an item is major" {
    var old = try strToStrApi();
    defer old.deinit();
    const str = try builtinStr(&old);
    try old.addItem(0, .{ .path = "M.g", .kind = .{ .value = str } });
    var new = try strToStrApi();
    defer new.deinit();

    try std.testing.expectEqual(Magnitude.major, try diffMagnitude(&old, &new));
}

test "renaming type variables is a patch" {
    // f : a -> a   vs   f : value -> value
    var old = PackageApi.init(gpa);
    defer old.deinit();
    {
        const v = try old.addType(.{ .variable = .{ .display_name = "a" } });
        const args = try old.allocator().dupe(TypeId, &.{v});
        const fn_ty = try old.addType(.{ .function = .{ .effectful = false, .args = args, .ret = v } });
        const module = try old.addModule("M");
        try old.addItem(module, .{ .path = "M.f", .kind = .{ .value = fn_ty } });
    }
    var new = PackageApi.init(gpa);
    defer new.deinit();
    {
        const v = try new.addType(.{ .variable = .{ .display_name = "value" } });
        const args = try new.allocator().dupe(TypeId, &.{v});
        const fn_ty = try new.addType(.{ .function = .{ .effectful = false, .args = args, .ret = v } });
        const module = try new.addModule("M");
        try new.addItem(module, .{ .path = "M.f", .kind = .{ .value = fn_ty } });
    }

    try std.testing.expectEqual(Magnitude.patch, try diffMagnitude(&old, &new));
}

test "changing a value signature is major" {
    var old = try strToStrApi();
    defer old.deinit();
    // f : Str, Str -> Str
    var new = PackageApi.init(gpa);
    defer new.deinit();
    {
        const str = try builtinStr(&new);
        const args = try new.allocator().dupe(TypeId, &.{ str, str });
        const fn_ty = try new.addType(.{ .function = .{ .effectful = false, .args = args, .ret = str } });
        const module = try new.addModule("M");
        try new.addItem(module, .{ .path = "M.f", .kind = .{ .value = fn_ty } });
    }

    try std.testing.expectEqual(Magnitude.major, try diffMagnitude(&old, &new));
}

test "changing function purity is major" {
    var old = try strToStrApi();
    defer old.deinit();
    var new = PackageApi.init(gpa);
    defer new.deinit();
    {
        const str = try builtinStr(&new);
        const args = try new.allocator().dupe(TypeId, &.{str});
        const fn_ty = try new.addType(.{ .function = .{ .effectful = true, .args = args, .ret = str } });
        const module = try new.addModule("M");
        try new.addItem(module, .{ .path = "M.f", .kind = .{ .value = fn_ty } });
    }

    try std.testing.expectEqual(Magnitude.major, try diffMagnitude(&old, &new));
}

/// A module "M" with a value "M.f" whose type is a variable, optionally
/// carrying an `eq` constraint.
fn constrainedVarApi(with_constraint: bool) std.mem.Allocator.Error!PackageApi {
    var api = PackageApi.init(gpa);
    errdefer api.deinit();
    const bool_ty = try api.addType(.{ .named = .{ .origin = .builtin, .path = "Bool", .args = &.{} } });
    const v = try api.addType(.{ .variable = .{} });
    if (with_constraint) {
        const eq_args = try api.allocator().dupe(TypeId, &.{ v, v });
        const eq_fn = try api.addType(.{ .function = .{ .effectful = false, .args = eq_args, .ret = bool_ty } });
        const constraints = try api.allocator().dupe(PackageApi.Constraint, &.{.{ .method = "eq", .fn_ty = eq_fn }});
        api.getType(v).variable.constraints = constraints;
    }
    const module = try api.addModule("M");
    try api.addItem(module, .{ .path = "M.f", .kind = .{ .value = v } });
    return api;
}

test "adding a static dispatch constraint is major" {
    var old = try constrainedVarApi(false);
    defer old.deinit();
    var new = try constrainedVarApi(true);
    defer new.deinit();

    try std.testing.expectEqual(Magnitude.major, try diffMagnitude(&old, &new));
}

fn aliasApi(target_path: []const u8) std.mem.Allocator.Error!PackageApi {
    var api = PackageApi.init(gpa);
    errdefer api.deinit();
    const target = try api.addType(.{ .named = .{ .origin = .builtin, .path = target_path, .args = &.{} } });
    const module = try api.addModule("M");
    try api.addItem(module, .{ .path = "M", .kind = .{ .alias = .{ .arity = 0, .target = target } } });
    return api;
}

test "changing an alias target is major" {
    var old = try aliasApi("Str");
    defer old.deinit();
    var new = try aliasApi("U64");
    defer new.deinit();

    try std.testing.expectEqual(Magnitude.major, try diffMagnitude(&old, &new));
}

fn recordNominalApi(field_names: []const []const u8) std.mem.Allocator.Error!PackageApi {
    var api = PackageApi.init(gpa);
    errdefer api.deinit();
    const str = try builtinStr(&api);
    const fields = try api.allocator().alloc(PackageApi.Field, field_names.len);
    for (field_names, 0..) |name, i| {
        fields[i] = .{ .name = name, .ty = str };
    }
    const backing = try api.addType(.{ .record = .{ .fields = fields, .ext = null } });
    const module = try api.addModule("M");
    try api.addItem(module, .{ .path = "M", .kind = .{ .nominal = .{ .arity = 0, .is_opaque = false, .backing = backing } } });
    return api;
}

test "adding a field to a transparent record backing is major" {
    var old = try recordNominalApi(&.{"name"});
    defer old.deinit();
    var new = try recordNominalApi(&.{ "name", "age" });
    defer new.deinit();

    try std.testing.expectEqual(Magnitude.major, try diffMagnitude(&old, &new));
}

fn unionNominalApi(tag_names: []const []const u8) std.mem.Allocator.Error!PackageApi {
    var api = PackageApi.init(gpa);
    errdefer api.deinit();
    const tags = try api.allocator().alloc(PackageApi.Tag, tag_names.len);
    for (tag_names, 0..) |name, i| {
        tags[i] = .{ .name = name, .args = &.{} };
    }
    const backing = try api.addType(.{ .tag_union = .{ .tags = tags, .ext = null } });
    const module = try api.addModule("M");
    try api.addItem(module, .{ .path = "M", .kind = .{ .nominal = .{ .arity = 0, .is_opaque = false, .backing = backing } } });
    return api;
}

test "adding a tag to a transparent union backing is major" {
    var old = try unionNominalApi(&.{ "Red", "Green" });
    defer old.deinit();
    var new = try unionNominalApi(&.{ "Red", "Green", "Blue" });
    defer new.deinit();

    try std.testing.expectEqual(Magnitude.major, try diffMagnitude(&old, &new));
}

fn opaqueNominalApi() std.mem.Allocator.Error!PackageApi {
    var api = PackageApi.init(gpa);
    errdefer api.deinit();
    const module = try api.addModule("M");
    try api.addItem(module, .{ .path = "M", .kind = .{ .nominal = .{ .arity = 0, .is_opaque = true, .backing = null } } });
    return api;
}

test "opaque nominals compare equal regardless of backing" {
    // The model deliberately omits opaque backings, so two versions with
    // different (hidden) backings but the same public surface are a patch.
    var old = try opaqueNominalApi();
    defer old.deinit();
    var new = try opaqueNominalApi();
    defer new.deinit();

    try std.testing.expectEqual(Magnitude.patch, try diffMagnitude(&old, &new));
}

test "transparent to opaque is major" {
    var old = try recordNominalApi(&.{"name"});
    defer old.deinit();
    var new = try opaqueNominalApi();
    defer new.deinit();

    try std.testing.expectEqual(Magnitude.major, try diffMagnitude(&old, &new));
}

test "changing nominal arity is major" {
    var old = PackageApi.init(gpa);
    defer old.deinit();
    {
        const module = try old.addModule("M");
        try old.addItem(module, .{ .path = "M", .kind = .{ .nominal = .{ .arity = 1, .is_opaque = true, .backing = null } } });
    }
    var new = try opaqueNominalApi(); // arity 0
    defer new.deinit();

    try std.testing.expectEqual(Magnitude.major, try diffMagnitude(&old, &new));
}

fn externalRefApi(major: u32, minor: u32) std.mem.Allocator.Error!PackageApi {
    var api = PackageApi.init(gpa);
    errdefer api.deinit();
    const ref = try api.addType(.{ .named = .{
        .origin = .{ .external = .{ .url_id = "example.com/json", .major = major, .minor = minor } },
        .path = "Json.Decoder",
        .args = &.{},
    } });
    const module = try api.addModule("M");
    try api.addItem(module, .{ .path = "M.decoder", .kind = .{ .value = ref } });
    return api;
}

test "changing an external dependency major version in a signature is major" {
    var old = try externalRefApi(1, 0);
    defer old.deinit();
    var new = try externalRefApi(2, 0);
    defer new.deinit();

    try std.testing.expectEqual(Magnitude.major, try diffMagnitude(&old, &new));
}

test "changing an external dependency 0.X group in a signature is major" {
    var old = try externalRefApi(0, 5);
    defer old.deinit();
    var new = try externalRefApi(0, 6);
    defer new.deinit();

    try std.testing.expectEqual(Magnitude.major, try diffMagnitude(&old, &new));
}

test "magnitudes combine to the maximum" {
    // One added item (minor) and one removed item (major) -> major overall.
    var old = try strToStrApi();
    defer old.deinit();
    const old_str = try builtinStr(&old);
    try old.addItem(0, .{ .path = "M.removed", .kind = .{ .value = old_str } });
    var new = try strToStrApi();
    defer new.deinit();
    const new_str = try builtinStr(&new);
    try new.addItem(0, .{ .path = "M.added", .kind = .{ .value = new_str } });

    try old.normalize();
    try new.normalize();
    var result = try diff_mod.diff(gpa, &old, &new);
    defer result.deinit();

    try std.testing.expectEqual(Magnitude.major, result.magnitude);
    try std.testing.expectEqual(@as(usize, 2), result.changes.len);
}

test "diff reports structured changes with rendered signatures" {
    var old = try strToStrApi();
    defer old.deinit();
    var new = PackageApi.init(gpa);
    defer new.deinit();
    {
        const str = try builtinStr(&new);
        const args = try new.allocator().dupe(TypeId, &.{ str, str });
        const fn_ty = try new.addType(.{ .function = .{ .effectful = false, .args = args, .ret = str } });
        const module = try new.addModule("M");
        try new.addItem(module, .{ .path = "M.f", .kind = .{ .value = fn_ty } });
    }

    try old.normalize();
    try new.normalize();
    var result = try diff_mod.diff(gpa, &old, &new);
    defer result.deinit();

    try std.testing.expectEqual(@as(usize, 1), result.changes.len);
    const change = result.changes[0];
    try std.testing.expectEqual(diff_mod.ChangeKind.item_changed, change.kind);
    try std.testing.expectEqualStrings("M", change.module);
    try std.testing.expectEqualStrings("M.f", change.path);
    try std.testing.expectEqualStrings("Str -> Str", change.old_rendered.?);
    try std.testing.expectEqualStrings("Str, Str -> Str", change.new_rendered.?);
}

test "nextVersion follows Elm bump arithmetic" {
    const v = base.url.Version{ .major = 1, .minor = 2, .patch = 3 };

    try std.testing.expectEqual(
        base.url.Version{ .major = 2, .minor = 0, .patch = 0 },
        diff_mod.nextVersion(v, .major),
    );
    try std.testing.expectEqual(
        base.url.Version{ .major = 1, .minor = 3, .patch = 0 },
        diff_mod.nextVersion(v, .minor),
    );
    try std.testing.expectEqual(
        base.url.Version{ .major = 1, .minor = 2, .patch = 4 },
        diff_mod.nextVersion(v, .patch),
    );
}

test "nextVersion uses 0.X.Y convention rules before 1.0.0" {
    const v = base.url.Version{ .major = 0, .minor = 5, .patch = 2 };

    // Breaking changes bump the minor slot; compatible changes (additions
    // and patches alike) bump the patch slot.
    try std.testing.expectEqual(
        base.url.Version{ .major = 0, .minor = 6, .patch = 0 },
        diff_mod.nextVersion(v, .major),
    );
    try std.testing.expectEqual(
        base.url.Version{ .major = 0, .minor = 5, .patch = 3 },
        diff_mod.nextVersion(v, .minor),
    );
    try std.testing.expectEqual(
        base.url.Version{ .major = 0, .minor = 5, .patch = 3 },
        diff_mod.nextVersion(v, .patch),
    );
}

test "declaredMagnitude classifies bumps from 1.0.0 on" {
    const old = base.url.Version{ .major = 1, .minor = 2, .patch = 3 };
    const cases = [_]struct { declared: base.url.Version, magnitude: ?Magnitude }{
        .{ .declared = .{ .major = 2, .minor = 0, .patch = 0 }, .magnitude = .major },
        // Skipping versions is fine; only the highest changed slot counts.
        .{ .declared = .{ .major = 4, .minor = 1, .patch = 1 }, .magnitude = .major },
        .{ .declared = .{ .major = 1, .minor = 3, .patch = 0 }, .magnitude = .minor },
        .{ .declared = .{ .major = 1, .minor = 3, .patch = 5 }, .magnitude = .minor },
        .{ .declared = .{ .major = 1, .minor = 2, .patch = 4 }, .magnitude = .patch },
        .{ .declared = .{ .major = 1, .minor = 2, .patch = 10 }, .magnitude = .patch },
        // Not moving forward is never a valid bump.
        .{ .declared = .{ .major = 1, .minor = 2, .patch = 3 }, .magnitude = null },
        .{ .declared = .{ .major = 1, .minor = 2, .patch = 2 }, .magnitude = null },
        .{ .declared = .{ .major = 1, .minor = 1, .patch = 9 }, .magnitude = null },
        .{ .declared = .{ .major = 0, .minor = 9, .patch = 0 }, .magnitude = null },
    };

    for (cases) |case| {
        try std.testing.expectEqual(case.magnitude, diff_mod.declaredMagnitude(old, case.declared));
    }
}

test "declaredMagnitude classifies bumps with 0.X.Y convention rules" {
    const old = base.url.Version{ .major = 0, .minor = 5, .patch = 2 };
    const cases = [_]struct { declared: base.url.Version, magnitude: ?Magnitude }{
        // Bumping the 0.X compatibility group carries major semantics, as
        // does crossing to 1.0.0 (always the author's call).
        .{ .declared = .{ .major = 0, .minor = 6, .patch = 0 }, .magnitude = .major },
        .{ .declared = .{ .major = 0, .minor = 9, .patch = 1 }, .magnitude = .major },
        .{ .declared = .{ .major = 1, .minor = 0, .patch = 0 }, .magnitude = .major },
        .{ .declared = .{ .major = 2, .minor = 1, .patch = 0 }, .magnitude = .major },
        // A 0.patch bump carries minor semantics (patches are lumped in).
        .{ .declared = .{ .major = 0, .minor = 5, .patch = 3 }, .magnitude = .minor },
        .{ .declared = .{ .major = 0, .minor = 5, .patch = 9 }, .magnitude = .minor },
        // Not moving forward is never a valid bump.
        .{ .declared = .{ .major = 0, .minor = 5, .patch = 2 }, .magnitude = null },
        .{ .declared = .{ .major = 0, .minor = 5, .patch = 1 }, .magnitude = null },
        .{ .declared = .{ .major = 0, .minor = 4, .patch = 9 }, .magnitude = null },
    };

    for (cases) |case| {
        try std.testing.expectEqual(case.magnitude, diff_mod.declaredMagnitude(old, case.declared));
    }
}
