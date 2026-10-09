//! Checks producer-authored data-only proofs before runtime-root preparation.
//!
//! This first demand-planning domain proves complete direct procedure subtrees
//! independent of concrete substitutions. Every other subtree explicitly
//! requests the existing type/callable solution. It never treats an unknown
//! function value as an empty callee set.

const std = @import("std");
const check = @import("check");
const checked = check.CheckedModule;
const canonical = check.CheckedNames;
const Common = @import("common.zig");

const Allocator = std.mem.Allocator;
const Key = struct { module: [32]u8, template: u32 };
const Edge = struct { caller: usize, callee: usize };
const Address = struct { module: usize, template: u32 };

pub const Proof = enum {
    complete_data_only_subtree,
    type_callable_solution_required,
};

/// Session-owned proof index. No executable body, solved type graph, or dense
/// identity is persisted. All borrowed checked views must outlive this value.
pub const Plan = struct {
    allocator: Allocator,
    modules: std.ArrayList(checked.ImportedModuleView),
    index: std.AutoHashMap(Key, usize),
    required: []bool,
    analyzed: []bool = &.{},

    pub fn init(allocator: Allocator, modules: Common.CheckedModules, roots: Common.RootRequests) Allocator.Error!Plan {
        var plan = Plan{
            .allocator = allocator,
            .modules = .empty,
            .index = std.AutoHashMap(Key, usize).init(allocator),
            .required = &.{},
        };
        errdefer plan.deinit();
        try plan.addModule(checked.importedView(modules.root.module));
        for (modules.imports) |module| try plan.addModule(module);
        for (modules.root.relation_modules) |module| try plan.addModule(module);
        var count: usize = 0;
        for (plan.modules.items) |module| {
            for (module.checked_procedure_templates.templates.items) |template| {
                try plan.index.put(.{ .module = module.key.bytes, .template = @intFromEnum(template.template_id) }, count);
                count += 1;
            }
        }
        plan.required = try allocator.alloc(bool, count);
        @memset(plan.required, false);
        plan.analyzed = try allocator.alloc(bool, count);
        @memset(plan.analyzed, false);
        const addresses = try allocator.alloc(Address, count);
        defer allocator.free(addresses);
        for (plan.modules.items, 0..) |module, module_index| {
            for (module.checked_procedure_templates.templates.items) |template| {
                addresses[plan.node(module.key.bytes, @intFromEnum(template.template_id))] = .{
                    .module = module_index,
                    .template = @intFromEnum(template.template_id),
                };
            }
        }
        const queue = try allocator.alloc(usize, count);
        defer allocator.free(queue);
        var written: usize = 0;
        if (roots.source_modules.len != 0 and roots.source_modules.len != roots.requests.len) {
            @panic("semantic demand roots omitted their checked owners");
        }
        for (roots.requests, 0..) |request, root_index| {
            if (!rootMayBeOmitted(request)) continue;
            const owner = if (roots.source_modules.len == 0) modules.root.module.key else roots.source_modules[root_index];
            const target = plan.requestTemplate(request, owner) orelse continue;
            const instance = plan.templateNode(target);
            if (plan.analyzed[instance]) continue;
            plan.analyzed[instance] = true;
            queue[written] = instance;
            written += 1;
        }
        var edges = std.ArrayList(Edge).empty;
        defer edges.deinit(allocator);
        var read: usize = 0;
        while (read < written) : (read += 1) {
            const caller = queue[read];
            const address = addresses[caller];
            const module = plan.modules.items[address.module];
            const template = module.checked_procedure_templates.get(@enumFromInt(address.template));
            if (template.semantic_preparation == .type_callable_solution_required or
                template.evidence_params.len != 0 or
                template.static_dispatch_plans.len != 0 or
                template.target == .hosted)
            {
                plan.required[caller] = true;
                continue;
            }
            const refs = template.resolved_value_refs;
            for (module.resolved_value_refs.template_refs[refs.start .. refs.start + refs.len]) |ref| {
                const record = module.resolved_value_refs.records[@intFromEnum(ref)];
                const procedure: ?checked.ProcedureUseTemplate = switch (record.ref) {
                    .top_level_proc, .imported_proc, .hosted_proc, .promoted_top_level_proc => |procedure| procedure,
                    .platform_required_proc => |required| required.procedure,
                    .local_param, .local_value, .local_mutable_version, .pattern_binder => null,
                    // These are explicit demands for existing callable or
                    // checked-value solution, not guessed dependency sets.
                    .local_proc,
                    .selected_hoisted_const,
                    .top_level_const,
                    .imported_const,
                    .platform_required_declaration,
                    .platform_required_checked_error,
                    .platform_required_const,
                    => {
                        plan.required[caller] = true;
                        continue;
                    },
                };
                if (procedure) |use| {
                    if (plan.templateForUse(use)) |target| {
                        const callee = plan.templateNode(target);
                        try edges.append(allocator, .{ .caller = caller, .callee = callee });
                        if (!plan.analyzed[callee]) {
                            plan.analyzed[callee] = true;
                            queue[written] = callee;
                            written += 1;
                        }
                    } else {
                        plan.required[caller] = true;
                    }
                }
            }
        }
        try propagateRequired(allocator, plan.required, edges.items);
        return plan;
    }

    pub fn deinit(self: *Plan) void {
        self.allocator.free(self.required);
        self.allocator.free(self.analyzed);
        self.index.deinit();
        self.modules.deinit(self.allocator);
        self.* = undefined;
    }

    /// Compile-time roots and test observations are never removed. The proof
    /// only authorizes withholding a runtime root with no semantic obligations.
    pub fn rootProof(self: *const Plan, request: checked.RootRequest, source_module: checked.ModuleId) Proof {
        if (!rootMayBeOmitted(request)) return .type_callable_solution_required;
        const target = self.requestTemplate(request, source_module) orelse return .type_callable_solution_required;
        const instance = self.templateNode(target);
        if (!self.analyzed[instance]) @panic("runtime root was not declared to semantic demand planning");
        return if (self.required[instance])
            .type_callable_solution_required
        else
            .complete_data_only_subtree;
    }

    fn requestTemplate(self: *const Plan, request: checked.RootRequest, source_module: checked.ModuleId) ?canonical.ProcTemplate {
        return if (request.procedure_binding) |binding|
            directTemplate(self.moduleView(source_module.bytes).top_level_procedure_bindings.get(binding).body)
        else if (request.procedure_template) |declared_template|
            declared_template
        else if (request.procedure_use) |use|
            self.templateForUse(use)
        else
            @panic("semantic demand root has no producer-owned procedure identity");
    }

    fn addModule(self: *Plan, view: checked.ImportedModuleView) Allocator.Error!void {
        for (self.modules.items) |existing| {
            if (std.mem.eql(u8, &existing.key.bytes, &view.key.bytes)) return;
        }
        try self.modules.append(self.allocator, view);
    }

    fn moduleView(self: *const Plan, key: [32]u8) checked.ImportedModuleView {
        for (self.modules.items) |view| {
            if (std.mem.eql(u8, &view.key.bytes, &key)) return view;
        }
        @panic("semantic demand referenced an unpublished checked module");
    }

    fn node(self: *const Plan, module_key: [32]u8, template: u32) usize {
        return self.index.get(.{ .module = module_key, .template = template }) orelse
            @panic("semantic demand referenced an unpublished checked template");
    }

    fn templateNode(self: *const Plan, template: canonical.ProcTemplate) usize {
        return self.node(canonical.procTemplateModuleDigest(template).bytes, @intFromEnum(template.template));
    }

    fn templateForUse(self: *const Plan, use: checked.ProcedureUseTemplate) ?canonical.ProcTemplate {
        return switch (use.binding) {
            .top_level => |binding| directTemplate(self.moduleView(binding.artifact.bytes).top_level_procedure_bindings.get(binding.binding).body),
            .platform_required => |binding| directTemplate(self.moduleView(binding.artifact.bytes).top_level_procedure_bindings.get(binding.procedure_binding).body),
            .hosted => |binding| binding.template,
            .imported => |binding| blk: {
                const view = self.moduleView(binding.artifact.bytes);
                for (view.exported_procedure_bindings.bindings) |row| {
                    if (row.binding.def == binding.def and row.binding.pattern == binding.pattern) {
                        break :blk directTemplate(row.body);
                    }
                }
                @panic("semantic demand imported procedure binding was not published");
            },
        };
    }
};

fn rootMayBeOmitted(request: checked.RootRequest) bool {
    if (request.requires_pairing) return false;
    return switch (request.kind) {
        .runtime_entrypoint, .provided_export, .platform_required_binding => true,
        .hosted_export, .test_expect, .repl_expr, .dev_expr, .compile_time_constant, .compile_time_callable => false,
    };
}

fn directTemplate(body: anytype) ?canonical.ProcTemplate {
    comptime {
        if (@TypeOf(body) != checked.ProcedureBindingBody and @TypeOf(body) != checked.ImportedProcedureBindingBody) {
            @compileError("semantic demand consumes only published local or imported procedure bindings");
        }
    }
    return switch (body) {
        .direct_template => |direct| switch (direct.template) {
            .checked => |template| template,
            .lifted, .synthetic => @panic("non-checked procedure reached semantic demand planning"),
        },
        .callable_eval_template, .checked_error => null,
    };
}

/// Propagate required solution through reverse exact edges in linear work.
/// A recursive component with no local obligations remains proved data-only;
/// one required member makes every transitive caller require solution.
fn propagateRequired(allocator: Allocator, required: []bool, edges: []const Edge) Allocator.Error!void {
    const offsets = try allocator.alloc(usize, required.len + 1);
    defer allocator.free(offsets);
    @memset(offsets, 0);
    for (edges) |edge| offsets[edge.callee + 1] += 1;
    for (1..offsets.len) |index| offsets[index] += offsets[index - 1];
    const cursors = try allocator.dupe(usize, offsets[0..required.len]);
    defer allocator.free(cursors);
    const callers = try allocator.alloc(usize, edges.len);
    defer allocator.free(callers);
    for (edges) |edge| {
        callers[cursors[edge.callee]] = edge.caller;
        cursors[edge.callee] += 1;
    }
    const queue = try allocator.alloc(usize, required.len);
    defer allocator.free(queue);
    var written: usize = 0;
    for (required, 0..) |needs_solution, index| if (needs_solution) {
        queue[written] = index;
        written += 1;
    };
    var read: usize = 0;
    while (read < written) : (read += 1) {
        const callee = queue[read];
        for (callers[offsets[callee]..offsets[callee + 1]]) |caller| {
            if (required[caller]) continue;
            required[caller] = true;
            queue[written] = caller;
            written += 1;
        }
    }
}

test "semantic demand proves obligation-free recursive direct subtree" {
    var required = [_]bool{ false, false, false };
    const edges = [_]Edge{
        .{ .caller = 0, .callee = 1 },
        .{ .caller = 1, .callee = 0 },
        .{ .caller = 2, .callee = 1 },
    };
    try propagateRequired(std.testing.allocator, &required, &edges);
    try std.testing.expectEqualSlices(bool, &.{ false, false, false }, &required);
}

test "semantic demand preserves a transitive obligation inside recursive direct subtree" {
    var required = [_]bool{ false, false, true, false };
    const edges = [_]Edge{
        .{ .caller = 0, .callee = 1 },
        .{ .caller = 1, .callee = 0 },
        .{ .caller = 1, .callee = 2 },
    };
    try propagateRequired(std.testing.allocator, &required, &edges);
    try std.testing.expectEqualSlices(bool, &.{ true, true, true, false }, &required);
}

test "semantic demand only omits proved runtime roots never observations or pairing" {
    const allocator = std.testing.allocator;
    const module_key = checked.ModuleId{ .bytes = [_]u8{0x71} ** 32 };
    var plan = Plan{
        .allocator = allocator,
        .modules = .empty,
        .index = std.AutoHashMap(Key, usize).init(allocator),
        .required = try allocator.dupe(bool, &.{false}),
    };
    defer plan.deinit();
    plan.analyzed = try allocator.dupe(bool, &.{true});
    try plan.index.put(.{ .module = module_key.bytes, .template = 0 }, 0);
    var request = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(0),
        .abi = .roc,
        .exposure = .private,
        .procedure_template = .{
            .artifact = .{ .bytes = module_key.bytes },
            .proc_base = @enumFromInt(0),
            .template = @enumFromInt(0),
        },
    };
    try std.testing.expectEqual(Proof.complete_data_only_subtree, plan.rootProof(request, module_key));
    request.requires_pairing = true;
    try std.testing.expectEqual(Proof.type_callable_solution_required, plan.rootProof(request, module_key));
    request.requires_pairing = false;
    request.kind = .compile_time_constant;
    try std.testing.expectEqual(Proof.type_callable_solution_required, plan.rootProof(request, module_key));
    request.kind = .test_expect;
    try std.testing.expectEqual(Proof.type_callable_solution_required, plan.rootProof(request, module_key));
    request.kind = .runtime_entrypoint;
    plan.required[0] = true;
    try std.testing.expectEqual(Proof.type_callable_solution_required, plan.rootProof(request, module_key));
}
