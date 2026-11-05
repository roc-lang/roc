//! The procedures a `--specialize=no` program reaches, decided from its
//! checked modules (design.md "Unit Pieces And Whole-Program Dead Code"): a
//! walk from the root's provided exports through every reference a root's
//! evaluation follows (`eval.checked_root_graph`), over every module of the
//! link, naming each reached template's worker by the identity its unit's
//! object defines it under. The composition resolves these first, in
//! parallel, and keeps exactly what the pieces reach, so this walk may over-
//! or under-approximate without changing the program.
const std = @import("std");
const base = @import("base");
const check = @import("check");
const eval = @import("eval");
const lir = @import("lir");
const Allocator = std.mem.Allocator;
const Artifact = check.CheckedArtifact.CheckedModuleArtifact;
const TemplateKey = eval.checked_root_graph.TemplateKey;

/// The modules of one link, each with the name its workers are digested
/// under, as the walk reads them.
pub const Modules = struct {
    artifacts: []const *const Artifact,
    /// Parallel to `artifacts`.
    link_names: []const [32]u8,
};

/// The template workers the program reaches, by module key.
pub const Reach = struct {
    gpa: Allocator,
    identities: []lir.ProcIdentity,
    modules: std.AutoHashMapUnmanaged([32]u8, Span) = .empty,

    const Span = struct { start: u32, len: u32 };

    pub fn deinit(self: *Reach) void {
        self.modules.deinit(self.gpa);
        self.gpa.free(self.identities);
        self.* = undefined;
    }

    /// The workers reached in module `key`; none for a module the walk never met.
    pub fn of(self: *const Reach, key: [32]u8) []const lir.ProcIdentity {
        const span = self.modules.get(key) orelse return &.{};
        return self.identities[span.start..][0..span.len];
    }
};

/// Walk from `root`'s provided exports over `modules`, which include `root`.
pub fn compute(gpa: Allocator, root: *const Artifact, modules: Modules) Allocator.Error!Reach {
    std.debug.assert(modules.artifacts.len == modules.link_names.len);
    const root_at: u32 = for (modules.artifacts, 0..) |artifact, i| {
        if (artifact == root) break @intCast(i);
    } else base.invariant("the root module is no module of its own link", .{});
    var starts = std.ArrayList(eval.checked_root_graph.Start).empty;
    defer starts.deinit(gpa);
    for ((try root.rootRequests()).runtime_requests) |request| {
        if (request.kind != .provided_export) continue;
        if (request.procedure_template) |template| {
            try starts.append(gpa, .{ .template = template });
        } else if (request.procedure_binding) |binding| {
            try starts.append(gpa, .{ .binding = .{ .module = root_at, .binding = binding } });
        }
    }
    const templates = try eval.checked_root_graph.reachTemplates(gpa, modules.artifacts, starts.items);
    defer gpa.free(templates);
    std.mem.sort(TemplateKey, templates, {}, struct {
        fn lessThan(_: void, lhs: TemplateKey, rhs: TemplateKey) bool {
            if (lhs.module != rhs.module) return lhs.module < rhs.module;
            return @backingInt(lhs.template) < @backingInt(rhs.template);
        }
    }.lessThan);
    var reach: Reach = .{ .gpa = gpa, .identities = try gpa.alloc(lir.ProcIdentity, templates.len) };
    errdefer reach.deinit();
    var i: usize = 0;
    while (i < templates.len) {
        const module = templates[i].module;
        const start = i;
        while (i < templates.len and templates[i].module == module) : (i += 1) {
            reach.identities[i] = lir.CheckedPipeline.boxyTemplateWorkerIdentity(modules.link_names[module], templates[i].template);
        }
        try reach.modules.put(gpa, modules.artifacts[module].key.bytes, .{ .start = @intCast(start), .len = @intCast(i - start) });
    }
    return reach;
}
