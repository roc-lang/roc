//! Minimal hardcoded ModuleEnvs for the bootstrap compiler
//!
//! These provide just enough type information to compile the builtin .roc files.
//! They contain ONLY the nominal type declarations, no implementations.

const std = @import("std");
const can = @import("can");
const base = @import("base");
const types_mod = @import("types");

const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Statement = CIR.Statement;
const Ident = base.Ident;
const Region = base.Region;
const DataSpan = base.DataSpan;
const TypeVar = types_mod.Var;

/// Creates minimal Bool type: `Bool := [True, False]`
pub fn createBool(gpa: std.mem.Allocator) !*ModuleEnv {
    var env = try gpa.create(ModuleEnv);
    env.* = try ModuleEnv.init(gpa, "");
    errdefer env.deinit();

    try env.initCIRFields(gpa, "Bool");

    const type_ident = try env.insertIdent(base.Ident.for_text("Bool"));
    const true_ident = try env.insertIdent(base.Ident.for_text("True"));
    const false_ident = try env.insertIdent(base.Ident.for_text("False"));

    // Create type header
    const header_idx = try env.addTypeHeaderAndTypeVar(.{
        .name = type_ident,
        .args = .{ .span = DataSpan.empty() },
    }, .err, Region.zero());

    // Create tag union body
    const scratch_top = env.store.scratchTypeAnnoTop();

    const true_tag = try env.addTypeAnnoAndTypeVar(
        .{ .tag = .{ .name = true_ident, .args = .{ .span = DataSpan.empty() } } },
        .err,
        Region.zero(),
    );
    try env.store.addScratchTypeAnno(true_tag);

    const false_tag = try env.addTypeAnnoAndTypeVar(
        .{ .tag = .{ .name = false_ident, .args = .{ .span = DataSpan.empty() } } },
        .err,
        Region.zero(),
    );
    try env.store.addScratchTypeAnno(false_tag);

    const tag_union = try env.addTypeAnnoAndTypeVar(.{ .tag_union = .{
        .tags = try env.store.typeAnnoSpanFrom(scratch_top),
        .ext = null,
    } }, .err, Region.zero());

    // Create type declaration
    _ = try env.addStatementAndTypeVar(Statement{
        .s_nominal_decl = .{ .header = header_idx, .anno = tag_union },
    }, .err, Region.zero());

    return env;
}

/// Creates minimal Result type: `Result(ok, err) := [Ok(ok), Err(err)]`
pub fn createResult(gpa: std.mem.Allocator) !*ModuleEnv {
    var env = try gpa.create(ModuleEnv);
    env.* = try ModuleEnv.init(gpa, "");
    errdefer env.deinit();

    try env.initCIRFields(gpa, "Result");

    const type_ident = try env.insertIdent(base.Ident.for_text("Result"));
    const ok_tag_ident = try env.insertIdent(base.Ident.for_text("Ok"));
    const err_tag_ident = try env.insertIdent(base.Ident.for_text("Err"));
    const ok_var_ident = try env.insertIdent(base.Ident.for_text("ok"));
    const err_var_ident = try env.insertIdent(base.Ident.for_text("err"));

    // Create type header with type variables
    const header_scratch_top = env.store.scratchTypeAnnoTop();

    const ok_rigid = try env.addTypeAnnoAndTypeVar(.{ .rigid_var = .{ .name = ok_var_ident } }, .err, Region.zero());
    try env.store.addScratchTypeAnno(ok_rigid);

    const err_rigid = try env.addTypeAnnoAndTypeVar(.{ .rigid_var = .{ .name = err_var_ident } }, .err, Region.zero());
    try env.store.addScratchTypeAnno(err_rigid);

    const header_idx = try env.addTypeHeaderAndTypeVar(.{
        .name = type_ident,
        .args = try env.store.typeAnnoSpanFrom(header_scratch_top),
    }, .err, Region.zero());

    // Create Ok(ok) tag
    const ok_scratch = env.store.scratchTypeAnnoTop();
    const ok_arg = try env.addTypeAnnoAndTypeVar(.{ .rigid_var_lookup = .{ .ref = ok_rigid } }, .err, Region.zero());
    try env.store.addScratchTypeAnno(ok_arg);
    const ok_tag = try env.addTypeAnnoAndTypeVar(
        .{ .tag = .{ .name = ok_tag_ident, .args = try env.store.typeAnnoSpanFrom(ok_scratch) } },
        .err,
        Region.zero(),
    );

    // Create Err(err) tag
    const err_scratch = env.store.scratchTypeAnnoTop();
    const err_arg = try env.addTypeAnnoAndTypeVar(.{ .rigid_var_lookup = .{ .ref = err_rigid } }, .err, Region.zero());
    try env.store.addScratchTypeAnno(err_arg);
    const err_tag = try env.addTypeAnnoAndTypeVar(
        .{ .tag = .{ .name = err_tag_ident, .args = try env.store.typeAnnoSpanFrom(err_scratch) } },
        .err,
        Region.zero(),
    );

    // Create tag union
    const union_scratch = env.store.scratchTypeAnnoTop();
    try env.store.addScratchTypeAnno(ok_tag);
    try env.store.addScratchTypeAnno(err_tag);
    const tag_union = try env.addTypeAnnoAndTypeVar(.{ .tag_union = .{
        .tags = try env.store.typeAnnoSpanFrom(union_scratch),
        .ext = null,
    } }, .err, Region.zero());

    // Create type declaration
    _ = try env.addStatementAndTypeVar(
        Statement{
            .s_nominal_decl = .{ .header = header_idx, .anno = tag_union },
        },
        .err,
        Region.zero(),
    );

    return env;
}

/// Creates minimal List type: `List(a) := ...` (opaque)
pub fn createList(gpa: std.mem.Allocator) !*ModuleEnv {
    var env = try gpa.create(ModuleEnv);
    env.* = try ModuleEnv.init(gpa, "");
    errdefer env.deinit();

    try env.initCIRFields(gpa, "List");

    // For now, just create an empty module
    // The actual List type is compiler-intrinsic
    return env;
}

/// Creates minimal Str type: `Str := ...` (opaque)
pub fn createStr(gpa: std.mem.Allocator) !*ModuleEnv {
    var env = try gpa.create(ModuleEnv);
    env.* = try ModuleEnv.init(gpa, "");
    errdefer env.deinit();

    try env.initCIRFields(gpa, "Str");

    // For now, just create an empty module
    // The actual Str type is compiler-intrinsic
    return env;
}

/// Creates minimal Num type: `Num := ...` (opaque)
pub fn createNum(gpa: std.mem.Allocator) !*ModuleEnv {
    var env = try gpa.create(ModuleEnv);
    env.* = try ModuleEnv.init(gpa, "");
    errdefer env.deinit();

    try env.initCIRFields(gpa, "Num");

    // For now, just create an empty module
    // Numeric types are compiler-intrinsic
    return env;
}

/// Create all minimal builtins
pub fn createAll(gpa: std.mem.Allocator) ![]const *ModuleEnv {
    var list = std.ArrayList(*ModuleEnv).init(gpa);
    errdefer {
        for (list.items) |env| {
            env.deinit();
            gpa.destroy(env);
        }
        list.deinit();
    }

    try list.append(try createBool(gpa));
    try list.append(try createResult(gpa));
    try list.append(try createList(gpa));
    try list.append(try createStr(gpa));
    try list.append(try createNum(gpa));

    return list.toOwnedSlice();
}
