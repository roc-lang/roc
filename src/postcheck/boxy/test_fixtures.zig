//! Shared checked-type fixtures for the Boxy planning, layout, and lowering
//! tests. The three stages build the same synthetic checked payloads, so the
//! builders live here once rather than being copied into each test section.

const can = @import("can");
const check = @import("check");
const std = @import("std");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const names = check.CheckedNames;

/// Index into a fixture-local table, named so the literal reads as a table
/// position rather than a magic number.
pub fn tableIndex(comptime index: u32) u32 {
    return index;
}

/// A stored nominal for a builtin type, rooted at the fixture tables.
pub fn builtinNominal(
    builtin: checked.CheckedBuiltinNominal,
    _: checked.CheckedTypeId,
    args: checked.CheckedTypeRange,
) checked.StoredNominal {
    return .{
        .name = @fromBackingInt(@intCast(tableIndex(0))),
        .origin_module = @fromBackingInt(@intCast(tableIndex(0))),
        .owner_module = .{},
        .builtin = builtin,
        .is_opaque = false,
        .representation = .{ .builtin = builtin },
        .args = args,
    };
}

/// Add Bool's closed checked row to a fixture containing a Bool nominal.
pub fn addBoolDeclaration(
    allocator: std.mem.Allocator,
    module: *checked.CheckedModuleArtifact,
    bool_ty: checked.CheckedTypeId,
) std.mem.Allocator.Error!void {
    const types = &module.checked_types;
    const nominal = types.payloads.items[@backingInt(bool_ty)].nominal;
    const empty: checked.CheckedTypeId = @fromBackingInt(@intCast(@as(u32, @intCast(types.payloads.items.len))));
    try types.payloads.append(allocator, .empty_tag_union);
    const tags_start: u32 = @intCast(types.tag_pool.items.len);
    for ([_][]const u8{ "False", "True" }) |name| {
        const label = try module.canonical_names.internTagLabel(name);
        try types.tag_pool.append(allocator, .{ .name = label, .args_start = 0, .args_len = 0 });
    }
    const backing: checked.CheckedTypeId = @fromBackingInt(@intCast(@as(u32, @intCast(types.payloads.items.len))));
    try types.payloads.append(allocator, .{ .tag_union = .{
        .tags = .{ .start = tags_start, .len = 2 },
        .ext = empty,
    } });
    const declaration_index: u32 = @intCast(types.nominal_declarations.items.len);
    try types.nominal_declarations.append(allocator, .{
        .id = @fromBackingInt(@intCast(declaration_index)),
        .nominal = .{ .module = nominal.origin_module, .type_name = nominal.name, .source_decl = nominal.source_decl },
        .source_statement = 0,
        .declaration_root = bool_ty,
        .backing = backing,
    });
    types.builtin_nominal_declarations[@backingInt(checked.CheckedBuiltinNominal.bool)] = declaration_index + 1;
}

/// The checked-source module env every fixture artifact and numeral literal
/// shares; its CIR fields are initialized once.
pub fn testModuleEnv() *can.ModuleEnv {
    const S = struct {
        var env: ?*can.ModuleEnv = null;
    };
    if (S.env) |existing| return existing;
    const page_allocator = std.heap.page_allocator;
    const env = page_allocator.create(can.ModuleEnv) catch unreachable;
    env.* = can.ModuleEnv.init(page_allocator, "") catch unreachable;
    env.initCIRFields("Test") catch unreachable;
    S.env = env;
    return env;
}

/// An empty checked artifact for unit tests that build plans and lowerings by hand.
pub fn minimalCheckedArtifact(allocator: Allocator) checked.CheckedModuleArtifact {
    return .{
        .key = moduleKey(1),
        .canonical_names = names.CanonicalNameStore.init(allocator),
        .module_identity = testModuleIdentity(),
        .checking_context_identity = .{},
        .module_env = .{ .checked_source = testModuleEnv() },
        .exports = .{},
        .provides_requires = .{},
        .method_registry = .{},
        .static_dispatch_plans = .{},
        .resolved_value_refs = .{},
        .checked_procedure_templates = .{},
        .top_level_procedure_bindings = .{},
        .root_requests = .{},
        .hosted_procs = .{},
        .platform_required_declarations = .{},
        .platform_required_bindings = .{},
        .interface_capabilities = .{},
        .compile_time_roots = .{},
        .top_level_values = .{},
        .hoisted_constants = .{},
        .const_templates = .{},
        .const_store = check.ConstStore.ConstStore.init(allocator),
    };
}

/// The module identity of the single module in `minimalCheckedArtifact`.
pub fn testModuleIdentity() checked.ModuleIdentity {
    return .{
        .module_idx = 0,
        .module_name = @fromBackingInt(@intCast(tableIndex(0))),
        .display_module_name = @fromBackingInt(@intCast(tableIndex(0))),
        .kind = .module,
    };
}

/// A distinct module key whose first byte is `byte`.
pub fn moduleKey(byte: u8) checked.ModuleId {
    var key = checked.ModuleId{};
    key.bytes[0] = byte;
    return key;
}

/// A distinct canonical type key whose first byte is `byte`.
pub fn typeKey(byte: u8) names.CanonicalTypeKey {
    var key = names.CanonicalTypeKey{};
    key.bytes[0] = byte;
    return key;
}

/// A distinct canonical type scheme key whose first byte is `byte`.
pub fn typeSchemeKey(byte: u8) names.CanonicalTypeSchemeKey {
    var key = names.CanonicalTypeSchemeKey{};
    key.bytes[0] = byte;
    return key;
}

/// A procedure template reference into `key`'s artifact.
pub fn procedureTemplateRef(key: checked.CheckedModuleArtifactKey, raw_template_id: u32) names.ProcedureTemplateRef {
    return .{
        .artifact = .{ .bytes = key.bytes },
        .proc_base = @fromBackingInt(@intCast(raw_template_id)),
        .template = @fromBackingInt(@intCast(raw_template_id)),
    };
}

/// The procedure value reference naming `template`.
pub fn procedureValueRef(template: names.ProcedureTemplateRef) names.ProcedureValueRef {
    return .{
        .artifact = template.artifact,
        .proc_base = template.proc_base,
    };
}

/// A checked procedure template for a test fixture body.
pub fn checkedTemplate(
    template_ref: names.ProcedureTemplateRef,
    checked_fn_root: checked.CheckedTypeId,
    body: checked.CheckedBodyId,
    target: checked.ProcTarget,
) checked.CheckedProcedureTemplate {
    return .{
        .proc_base = template_ref.proc_base,
        .template_id = template_ref.template,
        .body = .{ .checked_body = body },
        .checked_fn_scheme = typeSchemeKey(9),
        .checked_fn_root = checked_fn_root,
        .static_dispatch_plans = .{},
        .direct_dispatch_plans = .{},
        .dispatch_relations = .{},
        .resolved_value_refs = .{},
        .top_level_value_uses = .{},
        .nested_proc_sites = .{},
        .target = target,
    };
}

/// An integer numeral literal recorded for a test.
pub fn testIntNumeral(value: u128) Allocator.Error!can.ModuleEnv.NumeralLiteral {
    return recordTestNumeral(value, 0, 0, false, false);
}

/// A small decimal numeral literal recorded for a test.
pub fn testSmallDecNumeral(value: can.CIR.SmallDecValue) Allocator.Error!can.ModuleEnv.NumeralLiteral {
    var divisor: u128 = 1;
    for (0..value.denominator_power_of_ten) |_| divisor *= 10;

    const magnitude: u128 = @intCast(if (value.numerator < 0) -@as(i32, value.numerator) else value.numerator);
    return recordTestNumeral(
        magnitude / divisor,
        magnitude % divisor,
        value.denominator_power_of_ten,
        value.numerator < 0,
        true,
    );
}

/// Record a numeral literal with the given digits and flags for a test.
pub fn recordTestNumeral(
    before: u128,
    after: u128,
    scale: u8,
    is_negative: bool,
    is_fractional: bool,
) Allocator.Error!can.ModuleEnv.NumeralLiteral {
    var before_buffer: [16]u8 = undefined;
    var after_buffer: [16]u8 = undefined;
    const before_digits = base256Digits(before, &before_buffer);
    const after_digits = if (scale == 0) &.{} else base256Digits(after, &after_buffer);
    const env = testModuleEnv();
    const node = @as(can.CIR.Node.Idx, @fromBackingInt(@intCast(tableIndex(0))));
    try env.recordNumeralLiteral(
        node,
        before_digits,
        after_digits,
        scale,
        is_negative,
        is_fractional,
        is_fractional,
        true,
    );
    return env.numeralLiteralForNode(node) orelse unreachable;
}

fn base256Digits(value: u128, buffer: *[16]u8) []const u8 {
    var remaining = value;
    var len: usize = 0;
    while (true) {
        buffer.*[buffer.len - 1 - len] = @truncate(remaining);
        len += 1;
        remaining >>= 8;
        if (remaining == 0) return buffer.*[buffer.len - len ..];
    }
}
